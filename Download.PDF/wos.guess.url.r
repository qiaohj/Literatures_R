#usethis::edit_r_environ()

library("rwosstarter")
library("data.table")
library("pdftools")
library("stringr")
library("stringi")
library("RCurl")
library("rvest")
library("httr")
library("pdftools")
library("scihubr")

setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
source("Download.PDF/wos.functions.r")
source("Download.PDF/getArticles.r")
source("Download.PDF/download.pdf.r")
source("Download.PDF/read_html.r")
source("../tokens.r")
crossref.year<-2025
journal<-"INTERNATIONAL JOURNAL OF MOLECULAR SCIENCES"
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
target.journals<-readRDS("../Data/CSC/target.journals_20250730.rda")
missing.file<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/missing.pdf/%s.csv", journal)

articles<-fread(missing.file)
articles[, c("doi.prefix", "doi.suffix") := {
  parts <- stri_split_fixed(doi, "/", n = 2)
  list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
}]
articles$pdf.path.hold<-sprintf("%s/%s/%s.PDF", 
        "/media/huijieqiao/WD22T_11/literatures/Data/PDF", journal, 
        URLencode(toupper(articles$doi.suffix), reserved = T))

articles.raw<-getArticles(target.journals[Journal_name==journal], all_journal_folders)

article.raw<-articles.raw[1]
resource<-article.raw$resource_primary_url

templates<-list("FRONTIERS IN PLANT SCIENCE"="https://www.frontiersin.org/articles/%s/full",
                "PLANTS-BASEL"="https://doi.org/%s",
                "AGRONOMY-BASEL"="https://doi.org/%s",
                "FOODS"="https://doi.org/%s",
                "MOLECULES"="https://doi.org/%s",
                "INTERNATIONAL JOURNAL OF MOLECULAR SCIENCES"="https://doi.org/%s",
                "SCIENTIFIC REPORTS"="https://www.nature.com/articles/%s",
                "NATURE"="https://www.nature.com/articles/%s",
                "NATURE COMMUNICATIONS"="https://www.nature.com/articles/%s",
                "NATURE PLANTS"="https://www.nature.com/articles/%s")
i=1
for (i in c(1:nrow(articles))){
  item<-articles[i]
  doi<-item$doi
  template<-templates[[journal]]
  if (is.na(doi)){
    next()
  }
  if (doi!=""){
    
    print(paste(i, nrow(articles), journal))
    if (is.null(template)){
      url<-""
    }else{
      url<-sprintf(template, doi)
    }
    if (journal %in% c("SCIENTIFIC REPORTS", "NATURE",
                       "NATURE COMMUNICATIONS", "NATURE PLANTS")){
      url<-sprintf(template, item$doi.suffix)
    }
    if (journal=="FRONTIERS IN PLANT SCIENCE"){
      url<-gsub("fple", "fpls", url)
    }
    filename<-item$pdf.path.hold
    if (file.exists(filename)){
      next()
    }
    if (journal %in% c("PLANTS-BASEL", "AGRONOMY-BASEL", "FOODS", "MOLECULES",
                       "INTERNATIONAL JOURNAL OF MOLECULAR SCIENCES")){
      response <- GET(url)
      url <- response$url
    }
    
    code.frame<-download.pdf(article.raw$publisher, url, item$doi.prefix, item$doi.suffix, 
                             wiley.api[4], elsevier.api[4], filename, 
                             journal)
    if (code.frame$code>0){
      file.copy(item$pdf.path.hold, item$pdf.path)
      #Sys.sleep(code.frame$sleep)
    }
    print(code.frame)
  }
}
