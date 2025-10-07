library(data.table)
library(ggplot2)
library(forcats)
library(ggrepel)
library(readr)
library(stringr)
library(tm)
library(SnowballC)
library(topicmodels)
library(tidytext)
library(wordcloud)

library(dplyr)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

if (F){
  keywords<-readRDS("../Data/BIOGEOGRAPHY/keywords.rda")
  df<-list()
  for (d in unique(keywords$doi)){
    item<-keywords[doi==d]
    
    keyword<-str_c(item$keyword, collapse = ", ")
    title<-item[1]$title
    abstract<-item[1]$abstract
    
    item.df<-data.table(doi=item[1]$doi, journal=item[1]$journal.x,
                        year=item[1]$year, title=title, abstract=abstract,
                        keyword=keyword, 
                        full.text=str_c(c(title, abstract, keyword), collapse = "\n"))
    df[[d]]<-item.df
  }
  dt<-rbindlist(df)
  
  
  dt<-dt[title!="Issue Information"]
  dt<-dt[title!="Author Index"]
  dt<-dt[title!="An Attractive and Accessible Text"]
  dt<-dt[title!="Book Reviews"]
  dt<-dt[title!="Book reviews"]
  dt<-dt[title!="Cover Image"]
  dt<-dt[title!="Front Cover"]
  dt<-dt[title!="Cover page"]
  dt<-dt[title!="Conclusions"]
  dt<-dt[toupper(title)!="ERRATUM"]
  dt<-dt[title!="List of Referees"]
  dt<-dt[title!="Contents"]
  dt<-dt[!grepl("Contents of", title)]
  dt<-dt[!grepl("CORRIGENDUM", toupper(title))]
  dt<-dt[!grepl("EDITORIAL", toupper(title))]
  dt<-dt[!grepl("Correction to", title)]
  saveRDS(dt, "../Data/BIOGEOGRAPHY/full.text.rda")
  
}
full.text<-readRDS("../Data/BIOGEOGRAPHY/full.text.rda")
#authors.df.full.gpd<-readRDS("../Data/BIOGEOGRAPHY/authors.fixed.rda")
#keywords<-keywords[doi %in% unique(authors.df.full.gpd$doi)]

#keywords$keyword<-toupper(keywords$keyword)
#write_file(str_c(unique(keywords$keyword), collapse = ", "),

# *** CRITICAL PARAMETER: The number of topics (k) to discover ***
# This is a parameter you should experiment with. A good starting point is 30.

num_topics <- 100


# Create a data.table, which is ideal for efficient processing
keywords_dt <- full.text[, c("doi", "full.text")]
keywords_dt$doc_id<-as.character(seq_along(full.text$full.text))
# Create a Corpus, the starting point for text processing with the 'tm' package
corpus <- Corpus(VectorSource(keywords_dt$full.text))

# Begin the text cleaning pipeline
corpus <- tm_map(corpus, content_transformer(tolower)) # 1. Convert to lowercase
corpus <- tm_map(corpus, removeNumbers)                # 2. Remove numbers
corpus <- tm_map(corpus, removePunctuation)            # 3. Remove punctuation
corpus <- tm_map(corpus, removeWords, stopwords("english")) # 4. Remove common English stop words
corpus <- tm_map(corpus, stripWhitespace)             # 5. Remove excess whitespace
corpus <- tm_map(corpus, stemDocument)                 # 6. Perform stemming (very important!)
# e.g., "modelling" -> "model", "species" -> "speci"

# --- 3. Create Document-Term Matrix (DTM) ---

# The DTM is the input for the LDA model: rows are documents (keyword phrases),
# columns are terms (stems), and values are term frequencies.
dtm <- DocumentTermMatrix(corpus)

# Many terms that appear only once can add noise. We can remove very sparse terms.
# For this large dataset, we will keep all terms for now, but you can uncomment
# the line below to make the DTM smaller and potentially faster.
# dtm <- removeSparseTerms(dtm, 0.995) # Keeps terms that appear in at least 0.5% of docs

# The LDA model requires that every document (row) has at least one term.
# Remove empty rows that might result from preprocessing.
row_totals <- apply(dtm, 1, sum)
dtm <- dtm[row_totals > 0, ]
# Also, update our original keywords data.table to keep them in sync
keywords_dt_filtered <- keywords_dt[doc_id %in% dtm$dimnames$Docs]
keywords_dt[doc_id=="8833"]


cat("Preprocessing complete. DTM dimensions:", nrow(dtm), "documents,", ncol(dtm), "terms.\n")

# --- 4. Run the LDA Model ---

cat("Running LDA model to find", num_topics, "topics... (This may take a few minutes)\n")

# Set a random seed for reproducibility
set.seed(12345)

# Run the LDA algorithm
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 12345))
saveRDS(lda_model, "../Data/BIOGEOGRAPHY/lda_model.rda")
cat("LDA model training complete.\n")


# --- 5. Interpret and Use the Results ---

# === 5.1 Understand Topic Composition (Beta values) ===

# Use tidytext's tidy() function to extract the per-topic-per-term probabilities (beta)
topics_beta <- tidy(lda_model, matrix = "beta")
setDT(topics_beta) # Convert to data.table for efficiency
saveRDS(topics_beta, "../Data/BIOGEOGRAPHY/topics_beta.rda")

# Find the top 100 terms within each topic
top_terms_per_topic <- topics_beta[, .SD[order(-beta)][1:100], by = topic]
fwrite(top_terms_per_topic, "../Data/BIOGEOGRAPHY/topic.term.csv")


# Extract the per-document-per-topic probabilities (gamma)
documents_gamma <- tidy(lda_model, matrix = "gamma")
setDT(documents_gamma) # Convert to data.table
saveRDS(documents_gamma, "../Data/BIOGEOGRAPHY/documents_gamma.rda")


# For each document, find the topic with the highest gamma probability
top_topic_for_doc <- documents_gamma[, .SD[which.max(gamma)], by = document]
threshold<-min(top_topic_for_doc$gamma)
top_topic_for_doc[document=="8833"]
full.text[doc_id=="8833"]
topic_for_doc<-documents_gamma[gamma>threshold]
# Merge the results with the original keywords
# The 'document' column format is 'doc_id', which we use for the join
results_dt <- keywords_dt_filtered[topic_for_doc, on = .(doc_id = document)]
setnames(results_dt, c("doi","original_keyword",  "doc_id", "topic", "probability"))
setorder(results_dt, topic, -probability)

final.result<-merge(results_dt, full.text, by="doi")

topics<-fread("../Data/BIOGEOGRAPHY/topics.csv")
final.result<-merge(final.result, topics, by="topic")

saveRDS(final.result, "../Data/BIOGEOGRAPHY/final.topic.result.rda")
final.result$title<-NULL
final.result$full.text<-NULL
final.result$Interpretation<-NULL
final.result$keyword<-NULL
final.result$original_keyword<-NULL
final.result$abstract<-NULL
final.result.N<-final.result[,.(N=.N), by=list(year, journal, Topic_Name)]
ggplot(final.result.N)+geom_line(aes(x=year, y=N, color=Topic_Name))+
  facet_wrap(~journal, ncol=1)+
  theme(legend.position = "none")
final.result.N.all<-final.result[,.(N=.N), by=list(journal,Topic_Name)]
ggplot(final.result)+geom_histogram(aes(x=Topic_Name), stat="count")+facet_wrap(~journal, ncol=1)

single.topic<-final.result[, .(N=.N), by=list(doi)]
min(final.result[doi %in% single.topic[N==1]$doi]$probability)
final.result[doc_id=="8833"]
probability<0.1]
final.result[doi=="ECOG.02849"]
wordcloud(words = final.result.N.all[journal=="OURNAL OF BIOGEOGRAPHY"]$Topic_Name,
          freq = final.result.N.all[journal=="OURNAL OF BIOGEOGRAPHY"]$N,
          min.freq = 1,          # Minimum frequency for a word to be plotted
          max.words = 100,       # Maximum number of words to be plotted
          random.order = FALSE,  # Plot words in decreasing frequency
          rot.per = 0.3,         # Proportion of words rotated 90 degrees
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))  
wordcloud(words = final.result.N.all[journal=="JOURNAL OF BIOGEOGRAPHY"]$Topic_Name,
          freq = final.result.N.all[journal=="JOURNAL OF BIOGEOGRAPHY"]$N,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.3,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))

wordcloud(words = final.result.N.all[journal=="ECOGRAPHY"]$Topic_Name,
          freq = final.result.N.all[journal=="ECOGRAPHY"]$N,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.3,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))
wordcloud(words = final.result.N.all[journal=="DIVERSITY AND DISTRIBUTIONS"]$Topic_Name,
          freq = final.result.N.all[journal=="DIVERSITY AND DISTRIBUTIONS"]$N,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.3,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))
wordcloud(words = final.result.N.all[journal=="GLOBAL ECOLOGY AND BIOGEOGRAPHY"]$Topic_Name,
          freq = final.result.N.all[journal=="GLOBAL ECOLOGY AND BIOGEOGRAPHY"]$N,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.3,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))
