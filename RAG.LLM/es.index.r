library(elastic)
library(data.table)
library(fs)
library(httr)
library(pdftools)
library(xml2)
library(stringr)
library(zoo)
library(ggplot2)
library(scales)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

es_host <- "localhost"
es_port <- 9200
prev.crossref.year<-2024
crossref.year<-2025
journal<-"METHODS IN ECOLOGY AND EVOLUTION"
source("RAG.LLM/create.es.index.r")
source("RAG.LLM/xml2csv.r")
source("RAG.LLM/grobid_pdf.r")

journals<-readRDS("../Data/JCR/Target.Journals.rda")
journals<-journals[sample(nrow(journals), nrow(journals))]

for (k in c(1:nrow(journals))){
  journal<-journals[k]$journal
  journal_folder_path <- sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s", journal)
  journal_xml_path <- sprintf("/media/huijieqiao/WD22T_11/literatures/Data/GROBID.XML/%s", journal)
  
  
  
  error.df<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/LOG/%s.%d.rda", journal, crossref.year)
  if (!file.exists(error.df)){
    next()
  }
  pdf_files <- dir_ls(journal_folder_path, recurse = TRUE, regexp = "\\.PDF$")
  if (length(pdf_files)==0){
    next()
  }
  xml_files <- dir_ls(journal_xml_path, recurse = TRUE, regexp = "\\.XML$")
  csv.target<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSV/%d/%s.rda", crossref.year, journal)
  if (file.exists(csv.target)){
    next()
  }
  saveRDS(NULL, csv.target)
  bulk_list <- list()
  count_list<-list()
  
  for (i in c(1:length(pdf_files))){
    pdf<-pdf_files[i]
    file.name<-gsub("\\.PDF", "", basename(pdf))
    if (file.name %in% names(bulk_list)){
      next()
    }
    
    print(sprintf("%d/%d: %s", i, length(pdf_files), pdf))
    xml<-gsub("/PDF/", "/GROBID.XML/", pdf)
    xml<-gsub("\\.PDF", "\\.XML", xml)
    if (!file.exists(xml)){
      grobid_pdf(pdf, xml)
    }
    if (!file.exists(xml)){
      next()
    }
    csv<-xml2csv(xml)
    
    pdf_count<-sum(nchar(pdf_text(pdf)), na.rm=T)
    active_csv<-csv[active_section %in% c("Title", "Abstract", "Introduction", "Methods", "Results",
                                          "Discussion", "Conclusion", "Unknown")]
    csv_count<-sum(active_csv$word.length, na.rm = T)
    item<-data.table(pdf_count=pdf_count, filename=file.name, csv_count=csv_count, indexed=F)
    
    #if (between(csv_count/pdf_count, 0.2, 2)){
    if (T){
      bulk_data<-active_csv[, c("active_section", "file.name", "text")]
      bulk_data$journal<-journal
      colnames(bulk_data)[2]<-"filename"
      bulk_list[[file.name]]<-bulk_data
      item$indexed<-T
    }
    count_list[[file.name]]<-item
    
  }
  
  saveRDS(bulk_list, csv.target)
  saveRDS(count_list, gsub("\\.rda", "\\.N.rda", csv.target))
  
  bulk_df<- rbindlist(bulk_list)
  
  
  tryCatch({
    conn <- connect(host = es_host, port = es_port)
    print(conn)
  }, error = function(e) {
    stop("Can't connect to Elasticsearch.")
  })
  es_index_name<-tolower(gsub(" ", "_", journal))
  create_es_index(conn, es_index_name)
  
  
  
  if (length(bulk_df) > 0) {
    res <- docs_bulk(conn, x = bulk_df, index = es_index_name)
  }
}

if (F){
  ppp<-pdf_text("/media/huijieqiao/WD22T_11/literatures/Data/PDF/METHODS IN ECOLOGY AND EVOLUTION/2041-210X.13400.PDF")
}
