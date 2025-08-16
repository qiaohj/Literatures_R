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
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")


source("RAG.LLM/prompt.r")
source("RAG.LLM/pdf2xml.r")
source("RAG.LLM/xml2csv.r")

#target_labels <- c("Title", "Abstract", "Methods", "Introduction", "Results", "Discussion", "Conclusion", "Unknown")
target_labels <- c("Title", "Abstract", "Methods", "Results", "Table", "Unknown")

journals<-readRDS("../Data/CSC/target.journals_20250730.rda")
j=2
if (F){
  for (j in c(2:11)){
    journal<-journals[j]
    article.file<-sprintf("../Data/CSC/wos.journals/%s.rda", journal$Journal_name)
    articles<-readRDS(article.file)
    articles<-articles[document_type %in% c("Article", "Data Paper", "Early Access")]
    
    articles<-articles[!is.na(doi)]
    articles<-articles[doi!="NA"]
    articles[, c("doi.prefix", "doi.suffix") := {
      parts <- stri_split_fixed(doi, "/", n = 2)
      list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
    }]
    articles$pdf<-sprintf("%s.PDF", 
                          URLencode(toupper(articles$doi.suffix), reserved = T))
    articles$pdf.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s", 
                               journal$Journal_name, articles$pdf)
    articles$pdf.exist<-file.exists(articles$pdf.path)
    
    articles$elsevier.xml.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/XML/%s/%s", 
                                        journal$Journal_name, articles$pdf)
    articles$elsevier.exist<-file.exists(articles$elsevier.xml.path)
    
    articles$xml.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/GROBID.XML/%s/%s", 
                               journal$Journal_name, gsub("\\.PDF", "\\.XML", articles$pdf))
    articles$xml.exist<-file.exists(articles$xml.path)
    
    target.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/temp/%s", 
                           journal$Journal_name)
    source.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/CSV/%s", 
                           journal$Journal_name)
    
    if (!dir.exists(target.folder)){
      dir.create(target.folder, recursive = T)
    }
    articles$csv.path<-sprintf("%s/%s", 
                               source.folder, gsub("\\.PDF", "\\.RDA", articles$pdf))
    articles$txt.path<-sprintf("%s/%s", 
                               source.folder, gsub("\\.PDF", "\\.TXT", articles$pdf))
    articles<-articles[sample(nrow(articles), 10)]
    for (i in c(1:nrow(articles))){
      print(paste(i, nrow(articles), journal$Journal_name))
      file.copy(articles[i]$csv.path, target.folder)
      file.copy(articles[i]$txt.path, target.folder)
      file.copy(articles[i]$pdf.path, target.folder)
    }
  }
  
}
j=43
journals<-journals[1:500]
journals<-journals[sample(nrow(journals), nrow(journals))]
for (j in c(1:500)){
#for (j in c(11)){
  journal<-journals[j]
  article.file<-sprintf("../Data/CSC/wos.journals/%s.rda", journal$Journal_name)
  if (!file.exists(article.file)){
    next()
  }
  articles<-readRDS(article.file)
  articles<-articles[document_type %in% c("Article", "Data Paper", "Early Access")]
  
  articles<-articles[!is.na(doi)]
  articles<-articles[doi!="NA"]
  articles[, c("doi.prefix", "doi.suffix") := {
    parts <- stri_split_fixed(doi, "/", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  articles$pdf<-sprintf("%s.PDF", 
                        URLencode(toupper(articles$doi.suffix), reserved = T))
  articles$pdf.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s", 
                             journal$Journal_name, articles$pdf)
  articles$pdf.exist<-file.exists(articles$pdf.path)
  
  articles$elsevier.xml.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/XML/%s/%s", 
                             journal$Journal_name, articles$pdf)
  articles$elsevier.exist<-file.exists(articles$elsevier.xml.path)
  xml.dir<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/GROBID.XML/%s", 
                   journal$Journal_name)
  if (!dir.exists(xml.dir)){
    dir.create(xml.dir)
  }
  articles$xml.path<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/GROBID.XML/%s/%s", 
                                      journal$Journal_name, gsub("\\.PDF", "\\.XML", articles$pdf))
  articles$xml.exist<-file.exists(articles$xml.path)
  
  target.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/CSV/%s", 
                         journal$Journal_name)
  if (!dir.exists(target.folder)){
    dir.create(target.folder, recursive = T)
  }else{
    next()
  }
  articles$csv.path<-sprintf("%s/%s", 
                            target.folder, gsub("\\.PDF", "\\.RDA", articles$pdf))
  articles$txt.path<-sprintf("%s/%s", 
                             target.folder, gsub("\\.PDF", "\\.TXT", articles$pdf))
  if (nrow(articles)==0){
    next()
  }
  for (i in c(1:nrow(articles))){
    print(paste(i, nrow(articles), journal$Journal_name))
    item<-articles[i]
    pdf.path<-item$pdf.path
    xml.path<-item$xml.path
    csv.path<-item$csv.path
    txt.path<-item$txt.path
    elsevier.xml.path<-item$elsevier.xml.path
    if (!file.exists(pdf.path) & file.exists(gsub("/PDF/", "/CSC/pdf/", pdf.path))){
      file.remove(gsub("/PDF/", "/CSC/pdf/", pdf.path))
    }
    if (file.exists(pdf.path)){
      #pdf.path<-"/media/huijieqiao/WD22T_11/literatures/Data/CSC/pdf/FRONTIERS IN PLANT SCIENCE/FPLS.2019.00721.PDF"
      text<-tryCatch({
        pdf_text(pdf.path)
      },
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
      text<-paste0(text, collapse = "")
      nchar<-nchar(text)
      text<-gsub(" ", "", text)
      text<-tolower(text)
      if (nchar<1000 | grepl("provisionalpdf", text) | grepl("acceptedarticle", text)){
        print(sprintf("Remove %s", pdf.path))
        
        file.remove(pdf.path)
        file.remove(gsub("/PDF/", "/CSC/pdf/", pdf.path))
        file.remove(xml.path)
        file.remove(csv.path)
        file.remove(txt.path)
      }
    }
    if (file.exists(pdf.path) | file.exists(elsevier.xml.path)){
      if (!file.exists(csv.path)){
        
        if (!file.exists(xml.path)){
          if (file.exists(pdf.path)){
            print("pdf.exist")
            res<-pdf2xml(pdf.path, xml.path)
            if (res!=xml.path){
              next()
              stop("Paste PDF Error")
            }
          }
          
        }
        if (file.exists(elsevier.xml.path)){
          print("elsevier.exist")
          csv.df<-elsevier.xml2csv(elsevier.xml.path)
        }else{
          csv.df<-xml2csv(xml.path)
        }
        
        saveRDS(csv.df, csv.path)
      }
      if (!file.exists(txt.path)){
        csv.df<-data.table(readRDS(csv.path))
        doi<-sprintf("[doi:%s]\n", item$doi)
        csv.text<-str_c(c(doi, csv.df[(active_section %in% target_labels) & word.length>0]$text), collapse = "\n")
        write_file(csv.text, txt.path)
        
      }
    }
  }
}




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



max.token<-1e5

j=2
for (j in c(2:nrow(journals))){
  journal<-journals[j]
  article.file<-sprintf("../Data/CSC/wos.journals/%s.rda", journal$Journal_name)
  articles<-readRDS(article.file)
  articles<-articles[document_type %in% c("Article", "Data Paper", "Early Access")]
  
  articles<-articles[!is.na(doi)]
  articles<-articles[doi!="NA"]
  articles[, c("doi.prefix", "doi.suffix") := {
    parts <- stri_split_fixed(doi, "/", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  target.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/CSV/%s", 
                         journal$Journal_name)
  articles$txt.path<-sprintf("%s/%s", 
                             target.folder, gsub("\\.PDF", "\\.TXT", articles$pdf))
  total.count<-30
  count<-0
  text<-c()
  for (i in c(1:nrow(articles))){
    print(paste(i, nrow(articles), journal$Journal_name))
    item<-articles[i]
    articles$pdf<-sprintf("%s.PDF", 
                          URLencode(toupper(articles$doi.suffix), reserved = T))
    txt.path<-item$txt.path
    if (file.exists(txt.path)){
      count<-count+1
      text<-c(text, read_file(txt.path))
      if (count>=total.count){
        break()
      }
    }
  }
}

  #View(data.table(x=text))
  full.text<-str_c(text, collapse = "\n")
  doc_text<-str_c(prompt_template_en, full.text, collapse = "\n")
  #doc_text<-read_file("~/Downloads/test.txt")
  write_file(doc_text, "~/Downloads/test.txt")
  str_count(doc_text, "\\b\\w+\\b")
  str_count(doc_text, "\\w+") / 0.5
  
  gen_config <- list(
    temperature = 0.2,
    max_output_tokens = 100000L
  )
  models<-c("gemini-2.5-pro", "gemini-2.5-flash")
  
  for (i in c(1:length(models))){
    
    mstr<-models[i]
    print(mstr)
    model <- google_genai$GenerativeModel(mstr,
                                          generation_config=gen_config)
    print(model$count_tokens(doc_text))
    
    print(system.time({
      response <- model$generate_content(doc_text, 
                                         request_options = dict(timeout = 3600L))
    }))
    print(response)
  }
  raw_text <- response$text
  raw_text<-gsub("```csv\n", "", raw_text)
  df <- read_csv(raw_text, col_types = cols(.default = "c"))
  View(df)
  
  
  
  
  # 你可以使用 httr 包在 R 里调用 OpenAI 的对话接口
  library(httr)
  library(jsonlite)
  
  # 设置你的 OpenAI API key
  api_key <- Sys.getenv("openai.key")
  
  # 定义对话请求函数
  openai_chat <- function(prompt, model = "gpt-5") {
    url <- "https://api.openai.com/v1/chat/completions"
    
    # 构建请求体
    body <- list(
      model = model,
      messages = list(
        list(role = "user", content = prompt)
      )
    )
    print(system.time({
    res <- POST(
      url,
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      body = toJSON(body, auto_unbox = TRUE)
    )}))
    
    if (status_code(res) == 200) {
      content <- content(res, as = "text", encoding = "UTF-8")
      result <- fromJSON(content)
      return(result$choices$message$content)
    } else {
      stop("OpenAI API 请求失败: ", status_code(res))
    }
  }
  prompt<-"你好，帮我写一个R语言的线性回归示例。"
  doc_text<-read_file("~/Downloads/test.txt")
  prompt<-doc_text
  # 示例调用
  reply <- openai_chat(doc_text)
  cat(reply)
