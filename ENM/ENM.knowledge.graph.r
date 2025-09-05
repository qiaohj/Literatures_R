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

#args = commandArgs(trailingOnly=TRUE)
#api.index<-as.numeric(args[1])
#if (is.na(api.index)){
#  api.index<-97
#}

apis<-fread("../gemini.keys")
#gemini.key<-apis[api.index]$gemini.api

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
clean_text <- function(text) {
  if (is.null(text) || nchar(text) == 0) {
    return("")
  }
  cleaned_text <- iconv(text, from = "", to = "UTF-8", sub = " ") # 将无效字符替换为空格
  cleaned_text <- str_replace_all(cleaned_text, "[\\x00-\\x1f\\x7f-\\x9f]", "")
  cleaned_text <- str_replace_all(cleaned_text, "\\s+", " ")
  cleaned_text <- str_trim(cleaned_text)
  
  return(cleaned_text)
}


system_instruction<-read_file("ENM/knowledge.graphy.md")


target.journals<-readRDS("../Data/ENM/clean.journal.rda")
target.journals<-target.journals[sample(nrow(target.journals), nrow(target.journals))]
#target.journals<-target.journals[Title=="DEEP-SEA RESEARCH PART II-TOPICAL STUDIES IN OCEANOGRAPHY"]
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
  pdfs<-pdfs[sample(length(pdfs), length(pdfs))]
  for (j in c(1:length(pdfs))){
    
    pdf_path<-pdfs[j]
    xml_path<-gsub("\\.PDF", "\\.XML", pdf_path)
    display_name<-gsub("\\.PDF", "", basename(pdf_path))
    json.file<-sprintf("%s/%s.json.rda", target, display_name)
    if (!file.exists(json.file)){
      next()
    }
    target.file<-sprintf("%s/%s.kg.rda", target, display_name)
    if (file.exists(target.file)){
      if (file.size(target.file)<100){
        #print(pdf_path)
        
        #file.remove(target.file)
      }
      next()
    }
    #next()
    saveRDS(NULL, target.file)
    json<-readRDS(json.file)
    json.pack<-list("DOI"=display_name,
                    "CONTENT"=list(
                      Title=ifelse(is.null(json$Title), "", json$Title),
                      Abstract=ifelse(is.null(json$Abstract), "", json$Abstract),
                      Introduction=ifelse(is.null(json$Introduction), "", json$Introduction),
                      Method=ifelse(is.null(json$Method), "", json$Method),
                      Result=ifelse(is.null(json$Result), "", json$Result),
                      Discussion=ifelse(is.null(json$Discussion), "", json$Discussion),
                      Conclusion=ifelse(is.null(json$Conclusion), "", json$Conclusion)
                    )
    )
    json.pack<-toJSON(json.pack)
    tryCatch({
      api.index<-random_integer <- sample(1:nrow(apis), 1)
      print(sprintf("%d/%d %d/%d: api.index: %d, %s %s", i, nrow(target.journals),
                    j, length(pdfs), api.index, item$Title, display_name))
      gemini.key<-apis[api.index]$gemini.api
      google_genai$configure(api_key = gemini.key)
      model <- google_genai$GenerativeModel(mstr,
                                            generation_config=gen_config,
                                            safety_settings = safety_settings,
                                            system_instruction = system_instruction)
  
      print(system.time({
        message(sprintf("2. Generating content with Gemini (%s)...", mstr))
        response <- model$generate_content(json.pack)
      }))
      
      rrr<-py_to_r(response$to_dict())
      
      saveRDS(rrr, target.file)
      extracted_text <- response$text
      saveRDS(extracted_text, sprintf("%s/%s.kg.txt.rda", target, display_name))
      write_file(extracted_text, sprintf("%s/%s.kg.json", target, display_name))
      parsed_data <- fromJSON(gsub("json", "", gsub("`", "", extracted_text)))
      saveRDS(parsed_data, sprintf("%s/%s.kg.json.rda", target, display_name))
      
    },
    error = function(e) {
      message("Error: ", e$message)
      if (grepl("429", e$message, ignore.case = TRUE) || grepl("exceeded your current quota", e$message, ignore.case = TRUE)){
        apis<<-apis[-api.index]
        print(sprintf("remove api index %d from apis, %d api.keys left.", api.index, nrow(apis)))
        file.remove(target.file)
        if (nrow(apis)==0){
          stop("no api left. stop");
        }
      }
    },
    warning = function(w) {
      message("Warning: ", w$message)
    },
    finally = {
      
    })
    
    
    
  }
}
