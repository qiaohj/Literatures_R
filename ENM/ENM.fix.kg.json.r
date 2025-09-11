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
apis<-fread("../gemini.keys")
Sys.setenv("http_proxy"="http://127.0.0.1:7897")
Sys.setenv("https_proxy"="http://127.0.0.1:7897")
Sys.setenv("all_proxy"="http://127.0.0.1:7897")

system_instruction<-read_file("LLM.API/PROMPT/fix.json.md")


use_condaenv("rag.literature", required = TRUE)

google_genai <- import("google.generativeai")

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

target.journals<-readRDS("../Data/ENM/clean.journal.rda")
target.journals<-target.journals[sample(nrow(target.journals), nrow(target.journals))]
#target.journals<-target.journals[Title=="ACTA TROPICA"]
i=1
result<-list()
for (i in c(1:nrow(target.journals))){
  item<-target.journals[i]
  folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/ENM/pdf/%s", item$Title)
  target<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/ENM/LLM.Parse/%s", item$Title)
  pdfs<-list.files(folder, pattern="\\.PDF", full.name=T)
  if (length(pdfs)==0){
    next()
  }
  j=1
  for (j in c(1:length(pdfs))){
    pdf_path<-pdfs[j]
    
    display_name<-gsub("\\.PDF", "", basename(pdf_path))
    raw.file<-sprintf("%s/%s.kg.rda", target, display_name)
    txt.file<-sprintf("%s/%s.kg.txt.rda", target, display_name)
    json.file<-sprintf("%s/%s.kg.json.rda", target, display_name)
    json.file.text<-gsub("\\.rda", "", json.file)
    json.file.text<-gsub("LLM.Parse", "LLM.Parse.KG.JSON", json.file.text)
    
    if (!file.exists(txt.file) & file.exists(raw.file)){
      file.remove(raw.file)
      #stop("Error 1")
    }
    if (file.exists(json.file.text)){
      print(json.file.text)
      status<-tryCatch({
        print("Checking json")
        parsed_data <- fromJSON(json.file.text)
        saveRDS(parsed_data, json.file)
        print("DONE to check the json")
        return("DONE")
      },
      error = function(e) {
        #print(json.file.text)
        tryCatch({
          api.index <- sample(1:nrow(apis), 1)
          gemini.key<-apis[api.index]$gemini.api
          google_genai$configure(api_key = gemini.key)
          
          model <- google_genai$GenerativeModel(mstr,
                                                generation_config=gen_config,
                                                safety_settings = safety_settings,
                                                system_instruction = system_instruction)
          json.content<-read_file(json.file.text)
          #stop("error")
          print(system.time({
            message(sprintf("2. Generating content with Gemini (%s)...", mstr))
            response <- model$generate_content(json.content)
          }))
          extracted_text <- response$text
          extracted_text<-gsub("`", "", gsub("json", "", extracted_text))
          write_file(extracted_text, json.file.text)
          print("Trying to re-parse json")
          parsed_data <- fromJSON(extracted_text)
          print("DONE")
          saveRDS(parsed_data, json.file)
        },
        error=function(e){
          stop("error")
        })
        #message("Error: ", e$message)
        return("DONE")
      })
      if (status=="DONE"){
        file.remove(json.file.text)
      }
      
      
    }
    if (!file.exists(txt.file) & file.exists(raw.file)){
      next()
    }
    if (!file.exists(raw.file)){
      next()
    }
    if (file.size(txt.file)/file.size(raw.file)<0.79){
      #stop("Error 3")
      #file.remove(txt.file)
      #file.remove(raw.file)
      #file.remove(json.file)
    }
    if (file.exists(txt.file) & !file.exists(json.file)){
      
      json.dir<-dirname(json.file.text)
      if (!dir.exists(json.dir)){
        dir.create(json.dir, recursive = T)
      }
      if (file.exists(json.file.text)){
        #json.text<-read_file(json.file.text)
      }else{
        json.text<-readRDS(txt.file)
        json.text<-gsub("json", "", gsub("`", "", json.text))
        write_file(json.text, json.file.text)
      }
      next()
    }
  }
}
