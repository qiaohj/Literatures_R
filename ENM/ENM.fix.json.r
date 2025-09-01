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
    if (file.exists(txt.file) & !file.exists(raw.file)){
      stop("Error 1")
    }
    if (!file.exists(txt.file) & file.exists(raw.file)){
      stop("Error 2")
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
      json.file.text<-gsub("\\.rda", "", json.file)
      if (file.exists(json.file.text)){
        json.text<-read_file(json.file.text)
      }else{
        json.text<-readRDS(txt.file)
        json.text<-gsub("json", "", gsub("`", "", json.text))
        write_file(json.text, json.file.text)
      }
      
      print(json.file.text)
      parsed_data <- fromJSON(gsub("json", "", gsub("`", "", json.text)))
      saveRDS(parsed_data, json.file)
    }
    next()
    pdf.txt<-tryCatch({pdf_text(pdf_path)},
                      error = function(e) {
                        message("Error: ", e$message)
                        return(NULL)
                      })
    if (is.null(pdf.txt)){
      xml_doc <- read_xml(pdf_path)
      
      text_nodes <- xml_find_all(xml_doc, "//*")
      all_text_content <- xml_text(text_nodes)
      
      pdf.txt <- str_c(all_text_content, collapse = "\n")
    }
    nchar.raw.text<-nchar(pdf.txt)
    nchar.llm.text<-nchar(readRDS(txt.file))
    n.item<-data.table(nchar.raw.text,
                     nchar.llm.text,
                     pdf_path=pdf_path,
                     txt.file=txt.file,
                     json.file=json.file,
                     raw.file=raw.file)
    result[[length(result)+1]]<-n.item
  }
}
