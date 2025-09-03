library(elastic)
library(data.table)
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
journal<-"BIOSCIENCE JOURNAL"
source("RAG.LLM/create.es.index.r")
source("RAG.LLM/xml2csv.r")
source("RAG.LLM/grobid_pdf.r")

es_host <- "localhost"
es_port <- 9200
categories<-c("Biodiversity.Conservation.2025.csv", "Ecology.2025.csv", "Environmental.Sciences.2025.csv",
              "Evolutionary.Biology.2025.csv", "MULTIDISCIPLINARY SCIENCES.2025.csv",
              "Paleontology.2025.csv", "Plant.Sciences.2025.csv", "Zoology.2025.csv")
journals<-list()
for (category in categories){
  filePath<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/JCR/Target.Journals//%s", category)
  lines <- readLines(filePath, warn = FALSE)
  
  modified_lines <- gsub("\"%", "%\"", lines, fixed = TRUE)
  
  writeLines(modified_lines, filePath)
  
  item<-fread(filePath, quote="\"", head=T)
  journals[[length(journals)+1]]<-item
}
journals<-rbindlist(journals, fill=T)

colnames(journals)[1]<-"journal"
journals$journal<-toupper(journals$journal)
journals$journal<-gsub("&", "AND", journals$journal)
journals$journal<-toupper(gsub("ANDAMP;", "AND", journals$journal))
#journals<-journals[sample(nrow(journals), nrow(journals))]
#journals[grepl("NATURE", journals$journal)]
#journals[Category=="ECOLOGY"]
#journals<-journals[journals$journal=="NATURE PLANTS"]
k=1
for (k in c(1:nrow(journals))){
  journal<-journals[k]$journal
  tryCatch({
    conn <- connect(host = es_host, port = es_port)
    print(conn)
  }, error = function(e) {
    stop("Can't connect to Elasticsearch.")
  })
  n<-get_count_for_journal(conn, "full_journals", journal)
  if (n>0){
    next()
  }
  csv.source<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSV/%d/%s.rda", crossref.year, journal)
  if (!file.exists(csv.source)){
    next()
  }
  
  print(sprintf("loading %d/%d: %s", k, nrow(journals), journal))
  bulk_list<-readRDS(csv.source)
  if (is.null(bulk_list)){
    next()
  }
  bulk_df<- rbindlist(bulk_list)
  print(sprintf("adding %s to index", journal))
  if (length(bulk_df) > 0) {
    tryCatch({
      conn <- connect(host = es_host, port = es_port)
      #print(conn)
    }, error = function(e) {
      stop("Can't connect to Elasticsearch.")
    })
    res <- docs_bulk(conn, x = bulk_df, index = "full_journals")
    #print(res)
  }
}
