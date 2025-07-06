library(data.table)
library(pdftools)
library(stringr)
library(stringi)
library(RCurl)
library(rvest)
library(httr)
library(pdftools)
args = commandArgs(trailingOnly=TRUE)
index<-as.numeric(args[1])
if (is.na(index)){
  index<-1
}
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
source("../tokens.r")
source("Download.PDF/read_html.r")
categories<-list.files("../Data/JCR/Target.Journals/", pattern="\\.csv")
categories<-gsub("\\.csv", "", categories)
category<-"Ecology.2025"
crossref.year<-2024
getISSN.folder<-function(issn){
  issn.items<-strsplit(issn, "-")[[1]]
  return(sprintf("%s/%s-%s", issn.items[1], issn.items[1], issn.items[2]))
}
categories<-categories[index]
category<-categories[1]
for (category in categories){
  journal.conf<-fread(sprintf("../Data/JCR/Target.Journals/%s.csv", category), header=T)
  journal.conf$journal<-toupper(journal.conf$`Journal name`)
  journal.conf$journal<-gsub("&", "AND", journal.conf$journal)
  
  #journals<-readRDS(sprintf("../Data/CrossRef_Full/%d/journals.rda", crossref.year))
  i=1
  all_articles<-list()
  for (i in c(1:nrow(journal.conf))){
    conf.item<-journal.conf[i]
    print(sprintf("%s (%d/%d), %s",  
                  conf.item$journal, i, nrow(journal.conf), category))
    
    target_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s", conf.item$journal)
    article_item1<-NULL
    article_item2<-NULL
    if (conf.item$ISSN!=""){
      issnitem<-getISSN.folder(conf.item$ISSN)
      folders<-sprintf("../Data/CrossRef_By_Journal/%d/%s", crossref.year, issnitem)
      if (dir.exists(folders)){
        article_item1<-readRDS(sprintf("%s/articles.rda", folders))
        journal_item1<-readRDS(sprintf("%s/journals.rda", folders))
      }
    }
    
    if (conf.item$eISSN!=""){
      issnitem<-getISSN.folder(conf.item$eISSN)
      folders<-sprintf("../Data/CrossRef_By_Journal/%d/%s", crossref.year, issnitem)
      if (dir.exists(folders)){
        article_item2<-readRDS(sprintf("%s/articles.rda", folders))
        journal_item2<-readRDS(sprintf("%s/journals.rda", folders))
      }
    }
    if (is.null(article_item1) & !is.null(article_item2)){
      article_item<-article_item2
      journal_item<-journal_item2
    }
    if (!is.null(article_item1) & is.null(article_item2)){
      article_item<-article_item1
      journal_item<-journal_item1
    }
    if (!is.null(article_item1) & !is.null(article_item2)){
      article_item<-rbindlist(list(article_item1, article_item2))
      journal_item<-rbindlist(list(journal_item1, journal_item2))
    }
    article_item$abstract<-NULL
    #article_item[doi=="10.1002/ecy.2993"]
    article_item[, c("doi.prefix", "doi.suffix") := {
      parts <- stri_split_fixed(doi, "/", n = 2)
      list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
    }]
    article_item<-unique(article_item)
    article_item$pdf<-sprintf("%s/%s.PDF", 
                              target_folder, article_item$doi.suffix)
    article_item$exist<-file.exists(article_item$pdf)
    article_item<-article_item[exist==F]
    article_item$target_folder<-target_folder
    article_item$pdf<- sprintf("%s/%s.PDF", target_folder, URLencode(article_item$doi.suffix, reserved = TRUE))
    article_item$exist<-file.exists(article_item$pdf)
    article_item<-article_item[exist==F]       
    article_item$pdf<- sprintf("%s/%s.PDF", target_folder, tolower(URLencode(article_item$doi.suffix, reserved = TRUE)))
    article_item$exist<-file.exists(article_item$pdf)
    article_item<-article_item[exist==F]
    article_item$pdf<- sprintf("%s/%s.PDF", target_folder, toupper(URLencode(article_item$doi.suffix, reserved = TRUE)))
    article_item$exist<-file.exists(article_item$pdf)
    article_item<-article_item[exist==F]
    all_articles[[length(all_articles)+1]]<-article_item
  }
  all_articles<-rbindlist(all_articles)
  
  
  j=1
  all_articles<-all_articles[sample(nrow(all_articles), nrow(all_articles))]
  all_articles<-all_articles[type=="journal-article"]
  #all_articles[grepl("1067413613050031", pdf)]
  all_articles<-all_articles[sample(nrow(all_articles), nrow(all_articles))]
  #all_articles[grepl("biomedcentral", resource_primary_url)]
  #all_articles<-all_articles[grepl("biomedcentral", resource_primary_url)]
  #all_articles<-all_articles[grepl("wiley", resource_primary_url)]
  for (j in c(1:nrow(all_articles))){
    item<-all_articles[j]
    if (!dir.exists(item$target_folder)){
      dir.create(item$target_folder)
    }
    print(sprintf("%d/%d, %s, %s, %s, %s", j, nrow(all_articles), 
                  category,
                  item$publisher, item$resource_primary_url, item$pdf))
    
    filename<-item$pdf
    if (file.exists(filename)){
      next()
    }
    publisher<-item$publisher
    if (publisher=="Oxford University Press (OUP)"){
      
    }
    #next()
    url<-item$resource_primary_url
    doi.prefix<-item$doi.prefix
    doi.suffix<-item$doi.suffix
    
    tryCatch({
      no.service<-T
      if (endsWith(tolower(url), ".pdf")){
        no.service<-F
        download.file(url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      
      if (grepl("nature\\.com", url)){
        no.service<-F
        pdf.url <- sprintf("%s.pdf", url)
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      if (publisher=="Wiley"){
        next()
        no.service<-F
        url <- sprintf("https://api.wiley.com/onlinelibrary/tdm/v1/articles/%s/%s", doi.prefix, doi.suffix)
        token <- wiley.api
        headers <- c("Wiley-TDM-Client-Token" = token)
        
        binary_data <- getBinaryURL(url, httpheader = headers, followlocation = TRUE)
        writeBin(binary_data, filename)
        Sys.sleep(10)
        next()
      }
      if (publisher=="Smithsonian Institution"){
        no.service<-F
        pdf.url<-gsub("/part/", "/partpdf/", url)
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
        
      }
      if (publisher=="Elsevier BV"){
        no.service<-F
        base_url <- "https://api.elsevier.com/content/article/doi"
        request_url <- sprintf("%s/%s/%s", base_url, doi.prefix, doi.suffix)
        response <- GET(
          url = request_url,
          add_headers(
            `X-ELS-APIKey` = elsevier.api,
            `Accept` = "application/pdf"
          ),
          write_disk(path = filename, overwrite = TRUE),
          timeout(90) 
        )
        next()
      }
      if (F){
        if (publisher=="The Royal Society"){
          #https://royalsocietypublishing.org/doi/10.1098/rspb.2020.2714
          #https://royalsocietypublishing.org/doi/pdf/10.1098/rspb.2020.2714?download=true
          
          no.service<-F
          pdf.url<-sprintf("%s?download=true", gsub("/doi/", "/doi/pdf/",  url))
          
          
          download.file(pdf.url, 
                        destfile = filename, method="wget",
                        extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
          next()
        }
      }
      if (publisher=="MDPI AG"){
        no.service<-F
        pdf.url<-sprintf("%s/pdf", url)
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      if (grepl("\\.plos\\.", url)){
        no.service<-F
        webpage <- safe_read_html(url)
        if (is.null(webpage)){
          next()
        }
        pdf.url <- webpage %>%
          html_element("meta[name='citation_pdf_url']") %>%
          html_attr("content")
        if (is.na(pdf.url)){
          next()
        }
        
        if (!startsWith(pdf.url, "http")){
          pdf.url<-sprintf("%s%s", get_base_url(url), pdf.url)
        }
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      
      if (grepl("\\.biomedcentral\\.", url)){
        no.service<-F
        webpage <- safe_read_html(url)
        if (is.null(webpage)){
          next()
        }
        pdf.url <- webpage %>%
          html_element("meta[name='citation_pdf_url']") %>%
          html_attr("content")
        if (is.na(pdf.url)){
          next()
        }
        
        if (!startsWith(pdf.url, "http")){
          pdf.url<-sprintf("%s%s", get_base_url(url), pdf.url)
        }
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      
      if (F){
        if (publisher=="Universidad Nacional Autonoma de Mexico"){
          no.service<-F
          webpage <- safe_read_html(url)
          
          pdf.url <- webpage %>%
            html_nodes("a.obj_galley_link.PDF") %>%
            html_attr("href")
          
          download.file(pdf.url, 
                        destfile = filename, method="wget",
                        extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
          next()
        }
      }
      
      if (publisher %in% c(
                           "FapUNIFESP (SciELO)", 
                           "Springer Science and Business Media LLC",
                           "Resilience Alliance, Inc.",
                           "Biodiversity Heritage Library",
                           "Inter-Research Science Center",
                           "Pleiades Publishing Ltd",
                           "Copernicus GmbH",
                           "Oles Honchar Dnipropetrovsk National University",
                           "Norwegian Polar Institute",
                           "Masaryk University Press",
                           "Frontiers Media SA")){
        
        no.service<-F
        webpage <- safe_read_html(url)
        if (is.null(webpage)){
          next()
        }
        pdf.url <- webpage %>%
          html_element("meta[name='citation_pdf_url']") %>%
          html_attr("content")
        if (!startsWith(pdf.url, "http")){
          pdf.url<-sprintf("%s%s", get_base_url(url), pdf.url)
        }
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      if (publisher %in% c("ALOKI Ltd")){
        no.service<-F
        download.file(url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      if (grepl("bioone", url)){
        next()
      }
      if (F){
        if (publisher=="Penerbit Universiti Sains Malaysia"){
          if (!grepl("pdf",url)){
            url<-sprintf("%s/pdf", url)
          }
          download.file(url, 
                        destfile = filename, method="wget",
                        extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
          next()
        }
      }
      if (publisher %in% c("Humboldt Field Research Institute",
                           "Pensoft Publishers",
                           "CSIRO Publishing",
                           "Regional Euro-Asian Biological Invasions Centre Oy (REABIC)",
                           "Universidad Nacional Autonoma de Mexico",
                           "SciELO Agencia Nacional de Investigacion y Desarrollo (ANID)",
                           "Osterreichische Akademie der Wissenschaften",
                           "Informa UK Limited",
                           "Southwestern Association of Naturalists",
                           "Cambridge University Press (CUP)",
                           "JSTOR",
                           "O-Kratkoe Ltd",
                           "The Royal Society",
                           "Universidad Nacional Agraria la Molina",
                           "University of Wisconsin Press",
                           "Oxford University Press (OUP)",
                           "U.S. Fish and Wildlife Service",
                           "Schweizerbart",
                           "Universidad de Costa Rica",
                           "Penerbit Universiti Sains Malaysia")){
        no.service<-F
        next()
      }
      if (no.service){
        #asdf
      }
      
    },
    error = function(e) {
      message("Error: ", e$message)
      return(NA)
    },
    warning = function(w) {
      message("Warning: ", w$message)
      
    },
    finally = {
      
    })
    if (F){
      text <- pdf_text(item$pdf)
      file.remove(item$pdf)
      file.size(item$pdf)
      
      all_articles[publisher=="Penerbit Universiti Sains Malaysia"]
      all_articles[grepl("plos", resource_primary_url)][2]$resource_primary_url
      url<-"https://royalsocietypublishing.org/doi/pdf/10.1098/rspb.2021.0376?download=true"
      download.file(url, 
                    destfile = "~/Downloads/1.pdf", method="curl",
                    extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
      
      response <- GET(url)
      status_code(response) 
      html_content <- content(response, as = "text")
      
      writeLines(html_content, "~/Downloads/1.html", useBytes = TRUE)
      
    }
    #downloadPDF(item$resource_primary_url, item$doi.prefix, item$doi.suffix, item$publisher, filename)
    
    
  }
  
}
