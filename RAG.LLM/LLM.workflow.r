library(reticulate)
library(httr)
library(data.table)
library(xml2)
library(stringr)
library(zoo)
library(readr)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")


source("RAG.LLM/prompt.r")
source("RAG.LLM/pdf2xml.r")
source("RAG.LLM/xml2csv.r")

Sys.setenv("http_proxy"="http://127.0.0.1:7897")
Sys.setenv("https_proxy"="http://127.0.0.1:7897")
Sys.setenv("all_proxy"="http://127.0.0.1:7897")
rep<-GET("https://google.com")
rep

Sys.getenv("http_proxy")
Sys.getenv("https_proxy")
Sys.getenv("all_proxy")
Sys.getenv("no_proxy")

#conda activate rag.literature
use_condaenv("rag.literature", required = TRUE)

py_run_string("import os; print(os.environ.get('http_proxy'))")
py_run_string("import requests; print(requests.get('https://google.com').status_code)")

google_genai <- import("google.generativeai")
google_genai$configure(api_key = Sys.getenv("gemini.key"))

max.token<-99e5
target_labels <- c("Title", "Abstract", "Methods", "Introduction", "Results", "Discussion", "Conclusion", "Unknown")
#file_size <- file.info("RAG.LLM/test.txt")$size
#text <- readChar("RAG.LLM/test.txt", file_size, useBytes = TRUE)

pdf.folder<-"/media/huijieqiao/WD22T_11/literatures/Data/CSC/test/"
pdf.list<-list.files(pdf.folder, pattern="\\.PDF")
xml.folder<-"/media/huijieqiao/WD22T_11/literatures/Data/CSC/test/"
csv.folder<-"/media/huijieqiao/WD22T_11/literatures/Data/CSC/test/"
text<-c()
for (i in c(1:length(pdf.list))){
  pdf<-pdf.list[i]
  xml<-gsub("\\.PDF", "\\.XML", pdf)
  csv<-gsub("\\.PDF", "\\.RDA", pdf)
  pdf.path<-sprintf("%s/%s", pdf.folder, pdf)
  xml.path<-sprintf("%s/%s", xml.folder, xml)
  csv.path<-sprintf("%s/%s", csv.folder, csv)
  
  if (!file.exists(csv.path)){
    if (!file.exists(xml.path)){
      res<-pdf2xml(pdf.path, xml.path)
      if (res!=xml.path){
        stop("Paste PDF Error")
      }
    }
    csv.df<-xml2csv(xml.path)
    saveRDS(csv.df, csv.path)
  }
  csv.df<-data.table(readRDS(csv.path))
  csv.text<-str_c(csv.df[(active_section %in% target_labels) & word.length>0]$text, collapse = "\n")
  doi<-sprintf("[doi:%d]\n", i)
  text<-c(text, doi, csv.text)
  word_count <- sum(str_count(text, "\\b\\w+\\b"), na.rm=T)
  if (word_count>max.token){
    adsf
  }
}
View(data.table(x=text))
full.text<-str_c(text, collapse = "\n")
doc_text<-str_c(c(prompt_template_en, full.text), collapse = "\n")
gen_config <- list(
  temperature = 0.2,
  max_output_tokens = 8192L
)
model <- google_genai$GenerativeModel("gemini-2.5-pro",
                                      generation_config=gen_config)
response <- model$generate_content(doc_text)
raw_text <- response$text
df <- read_csv(raw_text, col_types = cols(.default = "c"))
View(df)
