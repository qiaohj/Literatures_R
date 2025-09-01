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
library(jsonlite)

setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

args = commandArgs(trailingOnly=TRUE)
api.index<-as.numeric(args[1])
if (is.na(api.index)){
  api.index<-1
}

apis<-c()
for (ikey in c(1:53)){
  apis<-c(apis, Sys.getenv(sprintf("gemini.key.%d", ikey)))
}
gemini.key<-apis[api.index]

Sys.setenv("http_proxy"="http://127.0.0.1:7897")
Sys.setenv("https_proxy"="http://127.0.0.1:7897")
Sys.setenv("all_proxy"="http://127.0.0.1:7897")

if (F){
  rep<-GET("https://google.com")
  rep
  py_run_string("import os; print(os.environ.get('http_proxy'))")
  py_run_string("import requests; print(requests.get('https://google.com').status_code)")
  
}

use_condaenv("rag.literature", required = TRUE)

google_genai <- import("google.generativeai")
google_genai$configure(api_key = gemini.key)

safety_settings <- list(
  dict(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_SEXUAL", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_HATE_SPEECH", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE")
)

gen_config <- list(
  temperature = 0.1,
  top_p = 0.95,
  max_output_tokens = 100000L
)

models<-c("gemini-2.5-pro", "gemini-2.5-flash")
mstr<-models[2]



prompt<-read_file("../Data/PROMPT/read.paper.md")

target.journals<-readRDS("../Data/ENM/clean.journal.rda")
target.journals<-target.journals[sample(nrow(target.journals), nrow(target.journals))]
#target.journals<-target.journals[Title=="ACTA TROPICA"]
i=1
for (i in c(1:nrow(target.journals))){
  item<-target.journals[i]
  folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/ENM/pdf/%s", item$Title)
  target<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/ENM/LLM.Parse/%s", item$Title)
  if (dir.exists(target)){
    #next()
  }else{
    dir.create(target)
  }
  pdfs<-list.files(folder, pattern="\\.PDF", full.name=T)
  if (length(pdfs)==0){
    next()
  }
  j=1
  for (j in c(1:length(pdfs))){
    pdf_path<-pdfs[j]
    xml_path<-gsub("\\.PDF", "\\.XML", pdf_path)
    display_name<-gsub("\\.PDF", "", basename(pdf_path))
    target.file<-sprintf("%s/%s.raw.rda", target, display_name)
    if (file.exists(target.file)){
      if (file.size(target.file)<100){
        #file.remove(target.file)
      }
      next()
    }
    saveRDS(NULL, target.file)
    if (file.exists(xml_path)){
      upload.path<-xml_path
    }else{
      pdfinfo<-tryCatch({pdf_info(pdf_path)},
                        
                        error = function(e) {
                          #message("Error: ", e$message)
                          return(NULL)
                        })
      if (is.null(pdfinfo)){
        file.copy(pdf_path, xml_path)
        upload.path<-xml_path
      }else{
        upload.path<-pdf_path
      }
    }
    model <- google_genai$GenerativeModel(mstr,
                                          generation_config=gen_config,
                                          safety_settings = safety_settings,
                                          system_instruction = prompt)
    
    
    print(sprintf("%d/%d %d/%d: api.index: %d, %s %s ", i, nrow(target.journals),
                  j, length(pdfs), api.index, item$Title, display_name))
    tryCatch({
      if (!is.null(upload.path)){
        uploaded_file <- google_genai$upload_file(path = upload.path, display_name = display_name)
        message(sprintf("File uploaded successfully. Name: %s", uploaded_file$name))
        print(system.time({
          message(sprintf("2. Generating content with Gemini (%s)...", mstr))
          response <- model$generate_content(uploaded_file)
        }))
      }else{
        print(system.time({
          message(sprintf("2. Generating content with Gemini (%s)...", mstr))
          response <- model$generate_content(full_string)
        }))
      }
      
      
      rrr<-py_to_r(response$to_dict())
      
      saveRDS(rrr, target.file)
      extracted_text <- response$text
      saveRDS(extracted_text, sprintf("%s/%s.txt.rda", target, display_name))
      write_file(extracted_text, sprintf("%s/%s.json", target, display_name))
      parsed_data <- fromJSON(gsub("json", "", gsub("`", "", extracted_text)))
      saveRDS(parsed_data, sprintf("%s/%s.json.rda", target, display_name))
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
