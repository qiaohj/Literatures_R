#usethis::edit_r_environ()
library(reticulate)
library(httr)
library(data.table)
library(xml2)
library(stringr)
library(stringi)
library(zoo)
library(pdftools)
library(readr)
library(pdftools)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

args = commandArgs(trailingOnly=TRUE)
api.index<-as.numeric(args[1])
book<-args[2]
if (is.na(api.index)){
  api.index<-2
}


#Slpit PDF into pages to feed LLM
# --- 2. Define the Splitting Function ---

split_pdf_by_pages <- function(input_path, output_dir, pages_per_split) {
  
  # Check if the input file exists
  if (!file.exists(input_path)) {
    stop("Error: Input PDF file not found at: ", input_path)
  }
  
  # Check for and create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    message("Output directory not found. Creating it at: ", output_dir)
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get the total number of pages in the PDF
  total_pages <- pdf_info(input_path)$pages
  message(paste("The file '", basename(input_path), "' has a total of ", total_pages, " pages.", sep = ""))
  
  # Calculate how many smaller files will be created
  num_files <- ceiling(total_pages / pages_per_split)
  message(paste("It will be split into ", num_files, " smaller files, with a maximum of ", pages_per_split, " pages each.", sep = ""))
  
  # Get the base filename (without extension) to name the output files
  base_filename <- tools::file_path_sans_ext(basename(input_path))
  
  # Loop through and create each smaller file
  for (i in 1:num_files) {
    # Calculate the start and end pages for the current chunk
    start_page <- (i - 1) * pages_per_split + 1
    end_page <- min(i * pages_per_split, total_pages) # Use min() to ensure the last page doesn't exceed the total
    
    # Define the output filename, e.g., large_file_part_1.pdf, large_file_part_2.pdf ...
    output_filename <- sprintf("%s_part_%04d.pdf", base_filename, i)
    output_filepath <- file.path(output_dir, output_filename)
    
    # Print a status message for the current operation
    message(paste("Generating '", output_filename, "' (pages ", start_page, "-", end_page, ")...", sep = ""))
    
    # Use the pdf_subset() function to perform the split
    # This is the core step!
    pdf_subset(
      input = input_path,
      pages = start_page:end_page,
      output = output_filepath
    )
  }
  
  message("\nSplitting process complete! All files have been saved to: ", output_dir)
}

books<-list.files("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/RAW.BOOK", pattern="\\.pdf")

#books<-books[!grepl("植被", books)]
#books<-books[grepl("植被", books)]
#books<-books[! (books %in% c("中国植物志全版.pdf", "云南植物志 1977-2006版 1-21卷合订（带书签可搜索）.pdf"))]
#books<-books[(books %in% c("中国植物志全版.pdf"))]
books<-gsub("\\.pdf", "", books)
books<-books[sample(length(books), length(books))]
#book<-"中国植物志全版"
for (book in books){
  #book<-"台湾植被"
  pdf<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/RAW.BOOK/%s.pdf", book)
  split.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/Splitted.PDF/%s", book)
  output.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/Splitted.TXT/%s", book)
  if (!dir.exists(split.folder)){
    dir.create(split.folder)
    max_token<-100000
    token_per_page<-5000
    pages_per_split<-ceiling(max_token/token_per_page)
    pages_per_split<-10
    
    split_pdf_by_pages(
      input_path = pdf,
      output_dir = split.folder,
      pages_per_split = pages_per_split
    )
  }
  if (!dir.exists(output.folder)){
    dir.create(output.folder, recursive = T)
  }
  
  
  
  prompt_ocr<-"
You are an expert OCR (Optical Character Recognition) engine. Your task is to extract the full text from the provided PDF document and format it as clean, readable Markdown.

Follow these rules strictly:

1.  **Avoid Excessive Repetition:** Do not use more than ten consecutive identical characters anywhere in the output. For example, avoid creating `----` or `        ` or `.....`(ten or more spaces) to align content. Instead, use regular paragraphs to present the data.
2.  **Focus on Readability:** Do not try to replicate the original layout, columns, or exact line breaks. Instead, create well-structured paragraphs. If headings are obvious, use Markdown headings (e.g., #, ##).
3.  **Ensure Fidelity:** The transcription must be highly accurate and faithful to the source text. Do not add, summarize, or omit any content from the main body of the document.
4.  **Identify Main Content:** The scanned document may be slightly skewed, causing small portions of the previous or next page to be visible at the left or right edges. Your primary task is to identify and transcribe **only the main, central text block** of the page(s). **Completely ignore and discard any fragmented text or partial lines that clearly belong to an adjacent page.**
5.  **Handle Tables:** When you encounter a table, first determine if it is a simple table (no merged-cell). Convert a table to Markdown format only if it is \"simple\" AND the content of each cell is concise. A \"simple\" table is one where every row and every column consists of a single, un-merged cell. If a table is simple but a cell contains a large amount of text, or if the table is not simple (i.e., it has merged or split cells), do not attempt to create a Markdown table. In these cases, extract the content of each cell as CSV format, row by row.
6.  **Direct Markdown Output:** Provide ONLY the transcribed Markdown content. Do not include any introductory phrases (like \"Here is the text...\"), concluding remarks, or code block fences (like ```markdown). Your entire response must be the extracted text itself, ready to be saved directly to a file.
7.  **Process the Entire Document:** Transcribe the full content of the provided file at once.
8.  **Special Character:** Ensure that any tilde characters (both ～ and ~) are preserved as ～ to avoid unintended strikethrough formatting.
"
  
  apis<-c()
  for (ikey in c(1:42)){
    apis<-c(apis, Sys.getenv(sprintf("gemini.key.%d", ikey)))
  }
  
  #api.index<-ceiling(runif(1) * 9)
  #api.index<-4
  
  gemini.key<-apis[api.index]
  Sys.setenv("http_proxy"="http://127.0.0.1:7897")
  Sys.setenv("https_proxy"="http://127.0.0.1:7897")
  Sys.setenv("all_proxy"="http://127.0.0.1:7897")
  
  
  use_condaenv("rag.literature", required = TRUE)
  
  if (F){
    rep<-GET("https://google.com")
    rep
    py_run_string("import os; print(os.environ.get('http_proxy'))")
    py_run_string("import requests; print(requests.get('https://google.com').status_code)")
    
  }
  
  google_genai <- import("google.generativeai")
  google_genai$configure(api_key = gemini.key)
  
  safety_settings <- list(
    dict(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE"),
    dict(category = "HARM_CATEGORY_SEXUAL", threshold = "BLOCK_NONE"),
    dict(category = "HARM_CATEGORY_HATE_SPEECH", threshold = "BLOCK_NONE"),
    dict(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE")
  )
  
  gen_config <- list(
    temperature = 1,
    top_p = 0.95,
    max_output_tokens = 100000L
  )
  
  models<-c("gemini-2.5-pro", "gemini-2.5-flash")
  mstr<-models[2]
  
  model <- google_genai$GenerativeModel(mstr,
                                        generation_config=gen_config,
                                        safety_settings = safety_settings,
                                        system_instruction = prompt_ocr)
  
  
  pdf.items<-list.files(split.folder, pattern="\\.pdf")
  
  i=1
  
  for (i in c(1:length(pdf.items))){
    pdf_path<-sprintf("%s/%s", split.folder, pdf.items[i])
    
    output_filepath<-sprintf(sprintf("%s/%s", output.folder, gsub("\\.pdf", "\\.txt", pdf.items[i])))
    if (file.exists(output_filepath)){
      next()
    }
    saveRDS(NULL, output_filepath)
    tryCatch({
      print(sprintf("api.index: %d", api.index))
      print(pdf.items[i])
      uploaded_file <- google_genai$upload_file(path = pdf_path, display_name = pdf.items[i])
      message(sprintf("   File uploaded successfully. Name: %s", uploaded_file$name))
      print(system.time({
        message(sprintf("2. Generating content with Gemini (%s)...", mstr))
        response <- model$generate_content(uploaded_file)
      }))
      
      
      extracted_text <- tryCatch({response$text},
                                 error = function(e) {
                                   message("Error: ", e$message)
                                   return("")
                                 },
                                 warning = function(w) {
                                   message("Warning: ", w$message)
                                   return("")
                                 },
                                 finally = {
                                   
                                 })
      
      writeLines(extracted_text, output_filepath, useBytes = TRUE)
      n.token<-ifelse(extracted_text=="", 0, (model$count_tokens(extracted_text))$total_tokens)
      message(sprintf("3. Successfully saved OCR text to '%s', total count: %d", 
                      output_filepath, n.token))
    },
    error = function(e) {
      message("Error: ", e$message)
    },
    warning = function(w) {
      message("Warning: ", w$message)
      
    },
    finally = {
      
    })
    
    
  }
}

#find . -type f -name "*.txt" -size 44c -delete
