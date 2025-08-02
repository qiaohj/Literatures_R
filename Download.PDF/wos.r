#usethis::edit_r_environ()

library("rwosstarter")
library("data.table")
library("stringi")
library("pdftools")
library("httr")
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
source("Download.PDF/wos.functions.r")
source("Download.PDF/getArticles.r")
source("Download.PDF/download.pdf.r")
source("Download.PDF/read_html.r")
source("../tokens.r")
token.index<-3
#wiley.api<-wiley.api[token.index]
#elsevier.api<-elsevier.api[token.index]
#rwosstarter::wos_get_records()
if (F){
  target.journals<-fread("../Data/CSC/target.journals_20250730.csv")
  target.journals[is.na(ISSN), ISSN:=""]
  target.journals[is.na(eISSN), eISSN:=""]
  journals<-readRDS("../Data/JCR/Target.Journals.rda")
  for (i in c(1:nrow(target.journals))){
    item<-target.journals[i]
    if (item$Journal_name %in% c("PROCEEDINGS OF THE FLORIDA STATE HORTICULTURAL SOCIETY",
                                 "JOURNAL OF HORTICULTURAL SCIENCE",
                                 "ZEITSCHRIFT FUR PFLANZENKRANKHEITEN UND PFLANZENSCHUTZ JOURNAL OF PLANT DISEASES AND PROTECTION")){
      next()
    }
    
    journals.item<-journals[journal==item$Journal_name]
    if (nrow(journals.item)==0 & item$ISSN=="" & item$eISSN==""){
      next()
    }
    target.journals[i, ISSN:=journals.item$ISSN]
    target.journals[i, eISSN:=journals.item$eISSN]
    
  }
  target.journals[is.na(ISSN), ISSN:=""]
  target.journals[is.na(eISSN), eISSN:=""]
  saveRDS(target.journals, "../Data/CSC/target.journals_20250730.rda")
  fwrite(target.journals, "../Data/CSC/target.journals_20250730.csv")
}
limit<-NULL
target.journals<-readRDS("../Data/CSC/target.journals_20250730.rda")

for (i in c(1:nrow(target.journals))){
  item<-target.journals[i]
  
  if (item$ISSN=="" & item$eISSN==""){
    next()
  }
  if (item$ISSN=="" & item$eISSN!=""){
    issn.str<-item$eISSN
  }
  if (item$ISSN!="" & item$eISSN==""){
    issn.str<-item$ISSN
  }
  if (item$ISSN!="" & item$eISSN!=""){
    issn.str<-sprintf("(%s OR %s)", item$ISSN, item$eISSN)
  }
  query <- sprintf("TS=((seed* OR fruit* OR diaspore* OR propagule*) AND (size* OR mass* OR weight* OR germinat* OR dorman*)) AND (IS=%s)",
                   issn.str)
  
  query <- utils::URLencode(query, reserved = TRUE)
  n_refs <- wos_search(query, database)
  
  if (n_refs == 0) {
    next()
  }
  folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/seeds/%s", item$Journal_name)
  if (!dir.exists(folder)){
    dir.create(folder)
  }
  refs <- data.frame()
  n_records_per_page <- 50
  
  pages <- seq(1, ceiling(n_refs/n_records_per_page), by = 1)
  for (page in pages) {
    target<-sprintf("%s/%d.rda", folder, page)
    
    if (file.exists(target)){
      next()
    }
    tryCatch({
      print(target)
      request <- paste0(api_url(), "/documents", "?db=", database, 
                        "&q=", query, "&limit=", n_records_per_page, "&page=", 
                        page)
      response <- httr::GET(url = request, config = httr::add_headers(accept = "application/json", 
                                                                      `X-ApiKey` = get_token()))
      httr::stop_for_status(response)
      print(sprintf("%s/%s requests remaining today. %d/%d pages. %s", response$headers$`x-ratelimit-remaining-day`,
                    response$headers$`x-ratelimit-limit-day`,
                    page, length(pages), item$Journal_name))
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      content <- jsonlite::fromJSON(content)
      saveRDS(content, target)
      
    },
    error = function(e) {
      message("Error: ", e$message)
      
    },
    warning = function(w) {
      message("Warning: ", w$message)
      
    },
    finally = {
      
    })
    if (length(pages) > 1) 
      Sys.sleep(sample(seq(0, sleep, by = 0.01), 1))
    
  }
}

i=1
for (i in c(1:nrow(target.journals))){
  item<-target.journals[i]
  print(sprintf("%d/%d: %s", i, nrow(target.journals), item$Journal_name))
  folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/seeds/%s", item$Journal_name)
  contents<-list.files(folder, pattern="\\.rda")
  if (length(contents)<=0){
    next()
  }
  all<-list()
  for (j in c(1:length(contents))){
    content<-readRDS(sprintf("%s/%s", folder, contents[j]))
    content <- content$hits
    uid <- list_to_df(get_field(content, "uid"))
    document_type <- list_to_df(get_field(content, "sourceTypes"))
    title <- list_to_df(get_field(content, "title"))
    authors <- unlist(lapply(get_field(content$names, "authors"), 
                             function(x) {
                               if (!is.data.frame(x)) {
                                 NA
                               }
                               else {
                                 paste0(x[, 1], collapse = " | ")
                               }
                             }))
    published_year <- list_to_df(get_field(content$source, 
                                           "publishYear"))
    published_month <- list_to_df(get_field(content$source, 
                                            "publishMonth"))
    source <- list_to_df(get_field(content$source, "sourceTitle"))
    volume <- list_to_df(get_field(content$source, "volume"))
    issue <- list_to_df(get_field(content$source, "issue"))
    pages <- list_to_df(get_field(content$source$pages, 
                                  "range"))
    no_article <- list_to_df(get_field(content$source, "articleNumber"))
    supplement_number <- list_to_df(get_field(content$source, 
                                              "supplement"))
    special_issue <- list_to_df(get_field(content$source, 
                                          "specialIssue"))
    book_editors <- unlist(lapply(get_field(content$names, 
                                            "bookEditors"), function(x) {
                                              if (!is.data.frame(x)) {
                                                NA
                                              }
                                              else {
                                                paste0(x[, 1], collapse = " | ")
                                              }
                                            }))
    keywords <- list_to_df(get_field(content$keywords, "authorKeywords"))
    doi <- list_to_df(get_field(content$identifiers, "doi"))
    eissn <- list_to_df(get_field(content$identifiers, "eissn"))
    issn <- list_to_df(get_field(content$identifiers, "issn"))
    isbn <- list_to_df(get_field(content$identifiers, "isbn"))
    pmid <- list_to_df(get_field(content$identifiers, "pmid"))
    citations <- unlist(lapply(get_field(content, "citations"), 
                               function(x) {
                                 ifelse(is.na(x$count), NA, x$count)
                               }))
    data <- data.table(uid, document_type, title, authors, 
                       published_year, published_month, source, volume, 
                       issue, pages, no_article, supplement_number, special_issue, 
                       book_editors, keywords, doi, eissn, issn, isbn, 
                       pmid, citations)
    data$journal<-item$Journal_name
    all[[length(all)+1]]<-data
  }
  all.df<-rbindlist(all)
  saveRDS(all.df, sprintf("../Data/CSC/wos.journals/%s.rda", item$Journal_name))
}

#get PDF and download links.
crossref.year<-2025
#WEED SCIENCE
i=1

target.journals<-readRDS("../Data/CSC/target.journals_20250730.rda")
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
result<-list()
for (i in c(1:nrow(target.journals))){
  item<-target.journals[i]
  if (item$Note.Qiao=="[no exist in jcr categories]"){
    #next()
  }
  journal.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/pdf/%s", item$Journal_name)
  if (!dir.exists(journal.folder)){
    dir.create(journal.folder)
  }
  article.file<-sprintf("../Data/CSC/wos.journals/%s.rda", item$Journal_name)
  if (!file.exists(article.file)){
    result.item<-data.table(journal=item$Journal_name, 
                            wos=0, 
                            crossref=0,
                            with.pdf=0
    )
  }else{
    articles<-readRDS(article.file)
    articles[, c("doi.prefix", "doi.suffix") := {
      parts <- stri_split_fixed(doi, "/", n = 2)
      list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
    }]
    articles$pdf<-sprintf("%s.PDF", 
                          URLencode(toupper(articles$doi.suffix), reserved = T))
    articles<-articles[, c("uid", "journal", "doi", "pdf", "title")]
    crossref.articles<-getArticles(item, all_journal_folders)
    if (nrow(crossref.articles)==0){
      result.item<-data.table(journal=item$Journal_name, 
                              wos=nrow(articles), 
                              crossref=0,
                              with.pdf=0
      )
    }else{
      crossref.articles<-crossref.articles[, c("pdf", "url", "resource_primary_url", "publisher")]
      articles.crossref<-merge(articles, crossref.articles, by="pdf", all.x=T)
      articles.crossref$with.pdf<-F
      articles.crossref[, c("doi.prefix", "doi.suffix") := {
        parts <- stri_split_fixed(doi, "/", n = 2)
        list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
      }]
      
      for (j in c(1:nrow(articles.crossref))){
        pdf<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s", item$Journal_name, articles.crossref[j]$pdf)
        target<-sprintf("%s/%s", journal.folder, articles.crossref[j]$pdf)
        pdf.exist<-file.exists(pdf)
        print(sprintf("J: %d/%d; A: %d/%d; %s @ %s, exist (%d)", 
                      i, nrow(target.journals), 
                      j, nrow(articles.crossref), 
                      item$Journal_name, articles.crossref[j]$publisher,
                      pdf.exist))
        if (pdf.exist){
          if (!file.exists(target)){
            file.copy(pdf, journal.folder)
          }
          articles.crossref[j, with.pdf:=T]
        }else{
          if (!is.na(articles.crossref[j]$resource_primary_url)){
            
            publisher<-articles.crossref[j]$publisher
            url<-articles.crossref[j]$resource_primary_url
            doi.prefix<-articles.crossref[j]$doi.prefix
            doi.suffix<-articles.crossref[j]$doi.suffix
            journal<-item$Journal_name
            if (publisher=="Elsevier BV"){
              next()
            }
            
            code.frame<-
              tryCatch({
                download.pdf(publisher, url, doi.prefix, doi.suffix, 
                             wiley.api[1], elsevier.api[4], 
                             pdf, journal)
                
                
              },
              error = function(e) {
                message("Error: ", e$message)
                next()
              },
              warning = function(w) {
                message("Warning: ", w$message)
                
              },
              finally = {
                
              })
            print(sprintf("Download stauts code: %d", code.frame$code))
            
            if (code.frame$code>0){
              file.copy(pdf, journal.folder)
              articles.crossref[j, with.pdf:=T]
              if (journal!="HORTSCIENCE"){
                Sys.sleep(code.frame$sleep)
              }
              
            }
          }
        }
      }
    }
    
    result.item<-data.table(journal=item$Journal_name, 
                            wos=nrow(articles), 
                            crossref=nrow(articles.crossref[!is.na(resource_primary_url)]),
                            with.pdf=nrow(articles.crossref[with.pdf==T])
    )
  }
  print(result.item)
  result[[length(result)+1]]<-result.item
  fwrite(articles.crossref[with.pdf==F], 
         sprintf("/media/huijieqiao/WD22T_11/literatures/Data/CSC/missing.pdf/%s.csv", item$Journal_name))
}
result.df<-rbindlist(result)
result.df$differ<-result.df$wos-result.df$with.pdf
fwrite(result.df, 
       "/media/huijieqiao/WD22T_11/literatures/Data/CSC/target.20250802.csv")
