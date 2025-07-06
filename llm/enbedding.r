setwd("~/GIT/literature/literature")
library(reticulate)
use_python("/Library/Frameworks/Python.framework/Versions/3.13/bin/python3", required = TRUE)  # 指定 Python 的路径
Sys.setenv("KMP_DUPLICATE_LIB_OK" = 'TRUE') 
Sys.setenv("OMP_NUM_THREADS" = 8)  # Limit to 1 thread
# loading required libraries
library(xml2) #read, parse, and manipulate XML,HTML documents
library(jsonlite) #manipulate JSON objects
library(text)
library(RKaggle) # download datasets from Kaggle 
library(dplyr)   # data manipulation
library(pdftools)
library(text)
library(text2map)
library(ollamar)

if (F){
  recipes_list <- RKaggle::get_dataset("thedevastator/better-recipes-for-a-better-life")
  saveRDS(recipes_list, "../Data/recipes_list.rda")
}
recipes_list<-readRDS("../Data/recipes_list.rda")
recipes_df <- recipes_list[[1]]
recipes_df <- as.data.frame(recipes_df[, -1])
# drop unnecessary columns
cleaned_recipes_df <- subset(recipes_df, select = -c(yield,rating,url,cuisine_path,nutrition,timing,img_src))
which(is.na(cleaned_recipes_df), arr.ind = TRUE)

# a quick inspection reveals columns [2:4] have missing values
subset_column_names <- colnames(cleaned_recipes_df)[2:4]

# Replace NA values dynamically based on conditions
cols_to_modify <- c("prep_time", "cook_time", "total_time")
cleaned_recipes_df[cols_to_modify] <- lapply(
  cleaned_recipes_df[cols_to_modify],
  function(x, df) {
    # Replace NA in prep_time and cook_time where both are NA
    replace(x, is.na(df$prep_time) & is.na(df$cook_time), "unknown")
  },
  df = cleaned_recipes_df  # Pass the whole dataframe for conditions
)
cleaned_recipes_df <- cleaned_recipes_df %>%
  mutate(
    prep_time = case_when(
      # If cooktime is present but preptime is NA, replace with "no preparation required"
      !is.na(cook_time) & is.na(prep_time) ~ "no preparation required",
      # Otherwise, retain original value
      TRUE ~ as.character(prep_time)
    ),
    cook_time = case_when(
      # If prep_time is present but cook_time is NA, replace with "no cooking required"
      !is.na(prep_time) & is.na(cook_time) ~ "no cooking required",
      # Otherwise, retain original value
      TRUE ~ as.character(cook_time)
    )
  )
# confirm there are no missing values
any(is.na(cleaned_recipes_df))


# confirm the replacing NA logic works by inspecting specific rows
cleaned_recipes_df[1081,]
cleaned_recipes_df[1,]
cleaned_recipes_df[405,]
cleaned_recipes_df <- head(cleaned_recipes_df,250)


chunk_size <- 1

# Get the total number of rows in the dataframe
n <- nrow(cleaned_recipes_df)

# Create a vector of group numbers for chunking
# Each group number repeats for 'chunk_size' rows
# Ensure the vector matches the total number of rows
r <- rep(1:ceiling(n/chunk_size), each = chunk_size)[1:n]

# Split the dataframe into smaller chunks (subsets) based on the group numbers
chunks <- split(cleaned_recipes_df, r)

recipe_sentence_embeddings <-  data.frame(
  recipe = character(),
  recipe_vec_embeddings = I(list()),
  recipe_id = character()
)
pb <- txtProgressBar(min = 1, max = length(chunks), style = 3)

# load required library

# # ensure to read loading instructions here for smooth running of the 'text' library
# # https://www.r-text.org/
# embedding data
for (i in 1:length(chunks)) {
  recipe <- as.character(chunks[i])
  recipe_id <- paste0("recipe",i)
  recipe_embeddings <- textEmbed(as.character(recipe),
                                 layers = 10:11,
                                 aggregation_from_layers_to_tokens = "concatenate",
                                 aggregation_from_tokens_to_texts = "mean",
                                 keep_token_embeddings = FALSE,
                                 batch_size = 1
  )
  
  # convert tibble to vector
  recipe_vec_embeddings <- unlist(recipe_embeddings, use.names = FALSE)
  recipe_vec_embeddings <- list(recipe_vec_embeddings)
  
  # Append the current chunk's data to the dataframe
  recipe_sentence_embeddings <- recipe_sentence_embeddings %>%
    add_row(
      recipe = recipe,
      recipe_vec_embeddings = recipe_vec_embeddings,
      recipe_id = recipe_id
    )
  
  # track embedding progress
  setTxtProgressBar(pb, i)
  
}
