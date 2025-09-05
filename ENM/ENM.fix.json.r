library(data.table)
library(xml2)
library(stringr)
library(stringi)
library(zoo)
library(readr)
library(pdftools)
library(jsonlite)

setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
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
    raw.file<-sprintf("%s/%s.raw.rda", target, display_name)
    txt.file<-sprintf("%s/%s.txt.rda", target, display_name)
    json.file<-sprintf("%s/%s.json.rda", target, display_name)
    json.file.text<-gsub("\\.rda", "", json.file)
    json.file.text<-gsub("LLM.Parse", "LLM.Parse.JSON", json.file.text)
    
    if (file.exists(txt.file) & !file.exists(raw.file)){
      #stop("Error 1")
    }
    if (file.exists(json.file.text)){
      status<-tryCatch({
        parsed_data <- fromJSON(json.file.text)
        saveRDS(parsed_data, json.file)
        "DONE"
      },
      error = function(e) {
        print(json.file.text)
        message("Error: ", e$message)
        return("Error")
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
      print(json.file.text)
      parsed_data <- fromJSON(gsub("json", "", gsub("`", "", json.text)))
      saveRDS(parsed_data, json.file)
    }
    next()
    
  }
}
