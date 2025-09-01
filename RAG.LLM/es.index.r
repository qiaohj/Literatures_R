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
if (F){
  journals<-readRDS("../Data/JCR/Target.Journals.rda")
}
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
journals<-journals[sample(nrow(journals), nrow(journals))]
#journals[grepl("NATURE", journals$journal)]
#journals[Category=="ECOLOGY"]
#journals<-journals[journals$journal=="NATURE PLANTS"]
k=1
for (k in c(1:nrow(journals))){
  journal<-journals[k]$journal
  journal_folder_path <- sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s", journal)
  journal_xml_path <- sprintf("/media/huijieqiao/WD22T_11/literatures/Data/GROBID.XML/%s", journal)
  if (!dir.exists(journal_xml_path)){
    dir.create(journal_xml_path)
  }
  els_xml_path <- sprintf("/media/huijieqiao/WD22T_11/literatures/Data/XML/%s", journal)
  if (!dir.exists(els_xml_path)){
    dir.create(els_xml_path)
  }
  error.df<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/LOG/%s.%d.rda", journal, crossref.year)
  if (!file.exists(error.df)){
    next()
  }
  if (!dir.exists(journal_folder_path)){
    next()
  }
  
  pdf_files <- list.files(journal_folder_path, full.name=T, pattern = "\\.PDF$")
  
  if (length(pdf_files)==0){
    next()
  }
  if (dir.exists(els_xml_path)){
    els_xml_files<-list.files(els_xml_path, full.name=T, pattern = "\\.PDF$")
  }else{
    els_xml_files<-""
  }
  xml_files <- list.files(journal_xml_path, full.name=T, pattern = "\\.XML$")
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
    if (nrow(csv)==0){
      next()
    }
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
  if (length(els_xml_files)>0){
    for (i in c(1:length(els_xml_files))){
      els_xml<-els_xml_files[i]
      file.name<-gsub("\\.XML", "", basename(els_xml))
      if (file.name %in% names(bulk_list)){
        next()
      }
      
      print(sprintf("%d/%d: %s", i, length(els_xml_files), els_xml))
      if (!file.exists(els_xml)){
        next()
      }
      
      csv<-elsevier.xml2csv(els_xml)
      if (nrow(csv)==0){
        next()
      }
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
  }
  
    
    
  
  
  saveRDS(bulk_list, csv.target)
  saveRDS(count_list, gsub("\\.rda", "\\.N.rda", csv.target))
  next()
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
