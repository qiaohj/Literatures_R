library("data.table")
library("rwosstarter")
library("data.table")
library("pdftools")
library("stringr")
library("stringi")
library("RCurl")
library("rvest")
library("httr")
library("pdftools")
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
source("Download.PDF/wos.functions.r")
source("Download.PDF/getArticles.r")
source("Download.PDF/download.pdf.r")
source("Download.PDF/read_html.r")
source("../tokens.r")

if (F){
  jcr<-fread("/media/huijieqiao/WD22T_11/literatures/Data/JCR/wos-core_masterlistJournals-October2024.csv")
  jcr$Title<-toupper(jcr$`Journal title`)
  jcr$Title<-gsub("&", "AND", jcr$Title)
  jcr$Title<-toupper(gsub("ANDAMP;", "AND", jcr$Title))
  jcr<-jcr[, c("Title", "ISSN", "eISSN")]
  jcr<-unique(jcr)
  fwrite(jcr, "../Data/Natal.Dispersal/jcr.csv")
  
  correct<-fread("../Data/Natal.Dispersal/correct.csv")
  correct$Correct<-toupper(correct$Correct)
  correct$Correct<-gsub("&", "AND", correct$Correct)
  correct$Correct<-toupper(gsub("ANDAMP;", "AND", correct$Correct))
  correct$Source<-NULL
  
  jcr$Correct<-jcr$Title
  jcr<-rbindlist(list(jcr, correct), fill=T)
  jcr<-unique(jcr)
  
  
  journals<-fread("../Data/Natal.Dispersal/natal.dispersal.csv")
  colnames(journals)<-c("Title", "Count")
  
  journals[!Title %in% jcr$Title]
  
  journals$ISSN<-""
  journals$eISSN<-""
  journals$Title<-toupper(journals$Title)
  i=1
  for (i in c(1:nrow(journals))){
    item<-jcr[Title==journals[i]$Title]
    
    if (nrow(item)==0){
      print(journals[i]$Title)
      next()
    }
    #item<-item[, c("Title", "ISSN", "eISSN")]
    item<-unique(item)
    journals[i, ISSN:=item$ISSN]
    journals[i, eISSN:=item$eISSN]
    journals[i, Title:=item$Correct]
  }
  
  journals<-journals[, .(Count=sum(Count)), 
                     by=list(Title, ISSN, eISSN)]
  journals<-journals[Title != "NOT A RECURRING JOURNAL"]
  saveRDS(journals, "../Data/Natal.Dispersal/clean.journal.rda")
  
  
  #TS=( (natal OR juvenile OR young OR offspring OR breeding) NEAR/3 (dispersal* OR move*) ) AND TS=(distance* OR kernel* OR range OR scale OR length)
  
  
  limit<-NULL
  target.journals<-readRDS("../Data/Natal.Dispersal/clean.journal.rda")
  i=1
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
    query <- sprintf("TS=( (natal OR juvenile OR young OR offspring OR breeding) NEAR/3 (dispersal* OR move*) ) AND TS=(distance* OR kernel* OR range OR scale OR length) AND (IS=%s)",
                     issn.str)
    database<-"WOS"
    query <- utils::URLencode(query, reserved = TRUE)
    n_refs <- wos_search(query, database)
    
    if (n_refs == 0) {
      next()
    }
    folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Natal.Dispersal/WOS/%s", item$Title)
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
                      page, length(pages), item$Title))
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
        Sys.sleep(sample(seq(0, 10, by = 0.01), 1))
      
    }
  }
  
  i=1
  for (i in c(1:nrow(target.journals))){
    item<-target.journals[i]
    print(sprintf("%d/%d: %s", i, nrow(target.journals), item$Title))
    folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Natal.Dispersal/WOS/%s", item$Title)
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
      data$journal<-item$Title
      all[[length(all)+1]]<-data
    }
    all.df<-rbindlist(all)
    saveRDS(all.df, sprintf("../Data/Natal.Dispersal/WOS.RDA/%s.rda", item$Title))
  }
  
}
#get PDF and download links.
crossref.year<-2025
#WEED SCIENCE


all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
target.journals<-readRDS("../Data/Natal.Dispersal/clean.journal.rda")
result<-list()
#target.journals<-target.journals[Title=="SEED SCIENCE AND TECHNOLOGY"]
i=1
for (i in c(nrow(target.journals):1)){
  item<-target.journals[i]
  journal.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Natal.Dispersal/pdf/%s", item$Title)
  if (!dir.exists(journal.folder)){
    dir.create(journal.folder)
  }
  article.file<-sprintf("../Data/Natal.Dispersal/WOS.RDA/%s.rda", item$Title)
  if (!file.exists(article.file)){
    result.item<-data.table(journal=item$Title, 
                            wos=0, 
                            type.matched=0,
                            with.doi=0,
                            crossref=0,
                            with.pdf=0
    )
  }else{
    articles<-readRDS(article.file)
    wos<-nrow(articles)
    articles<-articles[document_type %in% c("Article", "Data Paper", "Early Access")]
    type.matched<-nrow(articles)
    articles<-articles[!is.na(doi)]
    articles<-articles[doi!="NA"]
    with.doi<-nrow(articles)
    articles[, c("doi.prefix", "doi.suffix") := {
      parts <- stri_split_fixed(doi, "/", n = 2)
      list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
    }]
    articles$pdf<-sprintf("%s.PDF", 
                          URLencode(toupper(articles$doi.suffix), reserved = T))
    
    articles<-articles[, c("uid", "journal", "doi", "pdf", "title")]
    
    missing.file<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Natal.Dispersal/missing.pdf/%s.csv", item$Title)
    if (!file.exists(missing.file)){
      missing.df<-data.table(uid=articles$uid, code=999, note="")
    }else{
      missing.df<-fread(missing.file)
      if (nrow(missing.df)==0){
        missing.df<-data.table(uid=articles$uid, code=999, note="")
      }
      if ("code" %in% colnames(missing.df)){
        
      }else{
        missing.df$code<-999
        missing.df$note<-""
      }
    }
    #articles<-articles[uid %in% wos.uid]
    
    crossref.articles<-getArticles(item, all_journal_folders)
    if (nrow(crossref.articles)==0){
      result.item<-data.table(journal=item$Title, 
                              wos=wos,
                              type.matched=type.matched,
                              with.doi=with.doi,
                              crossref=0,
                              with.pdf=0
      )
    }else{
      crossref.articles<-crossref.articles[, c("pdf", "url", "resource_primary_url", "publisher")]
      articles.crossref<-merge(articles, crossref.articles, by="pdf", all.x=T)
      if (nrow(articles.crossref)==0){
        result.item<-data.table(journal=item$Title, 
                                wos=wos,
                                type.matched=type.matched,
                                with.doi=with.doi,
                                crossref=0,
                                with.pdf=0
        )
      }else{
        articles.crossref$pdf.path<-sprintf("%s/%s", journal.folder, articles.crossref$pdf)
        articles.crossref$with.pdf<-file.exists(articles.crossref$pdf.path)
        articles.crossref[, c("doi.prefix", "doi.suffix") := {
          parts <- stri_split_fixed(doi, "/", n = 2)
          list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
        }]
        articles.crossref<-unique(articles.crossref)
        articles.crossref<-merge(articles.crossref, missing.df[, c("uid", "code", "note")], by="uid", all.x=T)
        articles.crossref$code<-as.numeric(articles.crossref$code)
        articles.crossref$note<-as.character(articles.crossref$note)
        articles.crossref[is.na(code), code:=1]
        articles.crossref[is.na(note), note:=""]
        
        articles.crossref[is.na(code), code:=1]
        articles.crossref[is.na(note), note:=""]
        
        articles.crossref<-articles.crossref[sample(nrow(articles.crossref), nrow(articles.crossref))]
        
        for (j in c(1:nrow(articles.crossref))){
          
          dir.pdf<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s", item$Title)
          if (!dir.exists(dir.pdf)){
            dir.create(dir.pdf)
          }
          dir.xml<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/XML/%s", item$Title)
          if (!dir.exists(dir.xml)){
            dir.create(dir.xml)
          }
          pdf<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/%s", item$Title, articles.crossref[j]$pdf)
          xml<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/XML/%s/%s", item$Title, articles.crossref[j]$pdf)
          
          target<-articles.crossref[j]$pdf.path
          pdf.exist<-file.exists(pdf)
          xml.exist<-file.exists(xml)
          
          if (pdf.exist | xml.exist){
            if (!file.exists(target)){
              if (pdf.exist){
                file.copy(pdf, journal.folder)
              }
              if (xml.exist){
                file.copy(xml, journal.folder)
              }
            }
          }else{
            
            if (!is.na(articles.crossref[j]$resource_primary_url)){
              
              print(sprintf("J: %d/%d; A: %d/%d; %s @ %s, exist (%d)", 
                            i, nrow(target.journals), 
                            j, nrow(articles.crossref), 
                            item$Title, articles.crossref[j]$publisher,
                            pdf.exist))
              publisher<-articles.crossref[j]$publisher
              url<-articles.crossref[j]$resource_primary_url
              doi.prefix<-articles.crossref[j]$doi.prefix
              doi.suffix<-articles.crossref[j]$doi.suffix
              journal<-item$Title
              xml.download<-ifelse(publisher=="Elsevier BV", T, F)
              if (xml.download==T){
                xml.dir<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/XML/%s", item$Title)
                if (!dir.exists(xml.dir)){
                  dir.create(xml.dir)
                }
              }
              code.frame<-
                tryCatch({
                  download.pdf(publisher, url, doi.prefix, doi.suffix, 
                               wiley.api[2], elsevier.api[2], 
                               pdf, journal, xml=xml.download)
                  
                  
                },
                error = function(e) {
                  message("Error: ", e$message)
                  return(data.table(code=-1, note=e$message, sleep=10))
                },
                warning = function(w) {
                  message("Warning: ", w$message)
                  return(data.table(code=-1, note=w$message, sleep=10))
                },
                finally = {
                  
                })
              print(sprintf("Download stauts code: %d", code.frame$code))
              
              if (code.frame$code>0){
                print(code.frame)
                if (xml.download){
                  file.copy(xml, journal.folder)
                }else{
                  file.copy(pdf, journal.folder)
                }
                
                Sys.sleep(code.frame$sleep)
                #Sys.sleep(1)
              }
              articles.crossref[j, code:=code.frame$code]
              articles.crossref[j, note:=code.frame$note]
            }
          }
        }
      }
      articles.crossref$with.pdf<-file.exists(sprintf("%s/%s", journal.folder, articles.crossref$pdf))
      result.item<-data.table(journal=item$Title, 
                              wos=wos, 
                              type.matched=type.matched,
                              with.doi=with.doi,
                              crossref=nrow(articles.crossref[!is.na(resource_primary_url)]),
                              with.pdf=nrow(articles.crossref[with.pdf==T])
      )
    }
  }
  
  
  result.item$differ<-result.item$with.doi-result.item$with.pdf
  print(result.item)
  
  articles.crossref<-articles.crossref[with.pdf==F]
  publishers<-unique(articles.crossref[!is.na(publisher)]$publisher)
  publishers<-paste(publishers, collapse =", ")
  result.item$publisers<-publishers
  result[[length(result)+1]]<-result.item
  setorderv(articles.crossref, "doi", 1)
  fwrite(articles.crossref, 
         missing.file)
}

result.df<-rbindlist(result)
fwrite(result.df, 
       "/media/huijieqiao/WD22T_11/literatures/Data/Natal.Dispersal/target.20250813.csv")
