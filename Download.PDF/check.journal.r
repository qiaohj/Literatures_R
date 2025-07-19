library(data.table)
library(pdftools)
library(stringr)
library(stringi)
library(RCurl)
library(rvest)
library(httr)
library(pdftools)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
source("Download.PDF/read_html.r")
source("../tokens.r")
getISSN.folder<-function(issn){
  issn.items<-strsplit(issn, "-")[[1]]
  return(sprintf("%s/%s-%s", issn.items[1], issn.items[1], issn.items[2]))
}

args = commandArgs(trailingOnly=TRUE)
api_index<-as.numeric(args[1])
if (is.na(api_index)){
  api_index<-1
}
wiley.api<-wiley.api[api_index]
elsevier.api<-elsevier.api[api_index]

#category<-"Ecology.2025"
#journal.name<-"GLOBAL CHANGE BIOLOGY" (DONE 2025)
#journal.name<-"NATURE ECOLOGY AND EVOLUTION" (DONE 2025)
#journal.name<-"METHODS IN ECOLOGY AND EVOLUTION" (DONE 2025)
#journal.name<-"GLOBAL ECOLOGY AND BIOGEOGRAPHY" (DONE)
#journal.name<-"ECOLOGY LETTERS" (DONE)
#journal.name<-"ECOLOGY"
#journal.name<-"JOURNAL OF ECOLOGY"
#journal.name<-"OIKOS" (DONE)
#journal.name<-"OECOLOGIA" (DONE)
#journal.name<-"AMERICAN NATURALIST"
#journal.name<-"THEORETICAL ECOLOGY" (DONE)
#journal.name<-"ECOLOGICAL MODELLING" (DONE)
#journal.name<-"ECOGRAPHY"
#journal.name<-"FUNCTIONAL ECOLOGY" (DONE 2025)
#journal.name<-"JOURNAL OF ANIMAL ECOLOGY"
#journal.name<-"TRENDS IN ECOLOGY AND EVOLUTION"
#journal.name<-"FRONTIERS IN ECOLOGY AND THE ENVIRONMENT"
#journal.name<-"JOURNAL OF APPLIED ECOLOGY"
#journal.conf<-fread(sprintf("../Data/JCR/Target.Journals/%s.csv", category), header=T)
#journal.conf$journal<-toupper(journal.conf$`Journal name`)
#journal.conf$journal<-gsub("&", "AND", journal.conf$journal)
#journal<-journal.conf[journal==journal.name]



if (F){
  jcr_journals<-list.files("../Data/JCR/Target.Journals", pattern="\\.csv")
  journals<-list()
  for (f in jcr_journals){
    journal.conf<-fread(sprintf("../Data/JCR/Target.Journals/%s", f), header=T)
    journal.conf$journal<-toupper(journal.conf$`Journal name`)
    journal.conf$journal<-gsub("&", "AND", journal.conf$journal)
    journals[[length(journals)+1]]<-journal.conf
    
    
  }
  journals<-rbindlist(journals, fill=T)
  journals$Category<-NULL
  journals<-journals[, c("ISSN", "eISSN", "journal")]
  journals<-unique(journals)
  journals.N<-journals[,.(N=.N), by=list(journal)]
  journals[journal=="GLOBAL CHANGE BIOLOGY"]
  saveRDS(journals, "../Data/JCR/Target.Journals.rda")
}
journals<-readRDS("../Data/JCR/Target.Journals.rda")

crossref.year<-2025
i=1
journals<-journals[sample(nrow(journals), nrow(journals))]
journal.names<-c("JOURNAL OF ECOLOGY", "ECOLOGY", "OIKOS", "OECOLOGIA",
  "SEED SCIENCE AND TECHNOLOGY", "SEED SCIENCE RESEARCH", "JOURNAL OF SEED SCIENCE",
  "PLANT PHYSIOLOGY", "NEW PHYTOLOGIST", "ANNALS OF BOTANY",
  "FRONTIERS IN PLANT SCIENCE")
journal.index<-11
done<-c(1, 3, 4, 6)
t.journal.name<-"METHODS IN ECOLOGY AND EVOLUTION"
conf.item<-journals[journal==t.journal.name]
for (i in c(1:nrow(journals))){
  conf.item<-journals[i]
  #conf.item<-journals[journal==journal.names[journal.index]]
  journal.name<-conf.item$journal
  print(journal.name)
  target_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/GROBID.XML/%s", journal.name)
  middle_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Middle.PDF/%s", journal.name)
  if (!dir.exists(target_folder)){
    dir.create(target_folder)
  }else{
    #next()
  }
  if (!dir.exists(middle_folder)){
    dir.create(middle_folder)
  }
  
  grobid_url <- "http://172.16.120.92:8070/api/processFulltextDocument"
  
  
  pdf_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s", conf.item$journal)
  if (F){
    all_journal_folders<-list.dirs(sprintf("../Data/CrossRef_By_Journal/%d/", crossref.year))
    saveRDS(all_journal_folders, sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
  }
  all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
  conf.item$ISSN_1<-toupper(ifelse(conf.item$ISSN=="", "XXXXXXXXXXXXXXXX", conf.item$ISSN))
  conf.item$ISSN_2<-toupper(ifelse(conf.item$eISSN=="", "XXXXXXXXXXXXXXXX", conf.item$eISSN))
  
  
  folders<-all_journal_folders[grepl(conf.item$ISSN_1, toupper(all_journal_folders)) | 
                                 grepl(conf.item$ISSN_2, toupper(all_journal_folders))]
  article_item<-list()
  for (f in folders){
    article_item[[length(article_item)+1]]<-readRDS(sprintf("%s/articles.rda", f))
  }
  article_item<-rbindlist(article_item)
  if (nrow(article_item)==0){
    next()
  }
  article_item$abstract<-NULL
  #article_item[doi=="10.1002/ecy.2993"]
  article_item[, c("doi.prefix", "doi.suffix") := {
    parts <- stri_split_fixed(doi, "/", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  if (journal.name=="GLOBAL ECOLOGY AND BIOGEOGRAPHYxxxx"){
    article_item[grepl("j.1466-8238", tolower(doi.suffix))]$doi.suffix<-
      gsub("j.1466-8238", "j.1466-822X", article_item[grepl("j.1466-8238", tolower(doi.suffix))]$doi.suffix)
  }
  
  article_item<-unique(article_item)
  article_item$pdf<-sprintf("%s/%s.PDF", 
                            pdf_folder, URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item<-article_item[type=="journal-article"]
  article_item$xml<-sprintf("%s/%s.XML", 
                            target_folder, URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item<-article_item[!file.exists(article_item$xml)]
  setorderv(article_item, "pdf", -1)
  article_item<-article_item[sample(nrow(article_item), nrow(article_item))]
  if (nrow(article_item)==0){
    next()
  }
  for (j in c(1:nrow(article_item))){
    print(paste(j, nrow(article_item), article_item[j]$pdf, i, nrow(journals), journal.name))
    
    pdf<-basename(article_item[j]$pdf)
    target<-sprintf("%s/%s", target_folder, gsub("\\.PDF", "\\.XML", pdf))
    if (file.exists(target)){
      next()
    }
    if (!file.exists(article_item[j]$pdf)){
      next()
    }
    text<-NULL
    text<-tryCatch({
      pdf_text(article_item[j]$pdf)
    },
    error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    },
    warning = function(w) {
      message("Warning: ", w$message)
      
    },
    finally = {
      
    })
    if (is.null(text)){
      file.rename(article_item[j]$pdf, 
                  sprintf("%s/%s", middle_folder, pdf))
      print("MOVED")
      next()
    }
    text<-unlist(text)
    text<-paste(text, collapse = '')
    
    
    text<-gsub(" ", "", text)
    
    if (grepl("AcceptedArticle", text)){
      file.rename(article_item[j]$pdf, 
                  sprintf("%s/%s", middle_folder, pdf))
      print("MOVED")
      next()
    }
    pdf_path<-article_item[j]$pdf
    res <- POST(
      grobid_url,
      body = list(
        input = upload_file(pdf_path),
        segmentSentences=0,
        includeRawAffiliations=1,
        consolidatFunders=1)
    )
    
    
    if (status_code(res) == 200) {
      xml_content <- content(res, as = "text", encoding = "UTF-8")
      writeLines(xml_content, target)
    } else {
      cat("Error:", status_code(res), "\n")
      if (status_code(res)==500){
        #file.rename(article_item[j]$pdf, 
        #            sprintf("%s/%s", middle_folder, pdf))
        #print("MOVED")
      }
    }
    
    
  }
  if (F){
    #article_item$published
    article_item<-article_item[!file.exists(article_item$xml)]
    output<-article_item[, c("doi", "issue", "volume", "page", "resource_primary_url", "pdf", "title")]
    output$pdf<-gsub(sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s/", journal.names[journal.index]), "",
                             output$pdf)
    fwrite(output, sprintf("~/Downloads/Missing.PDF/%s.csv", journal.names[journal.index]))
  }
  if (journal.name %in% c("TROPICAL PLANT PATHOLOGY")){
    next()
  }
  
  all_articles<-article_item
  all_articles$exist<-file.exists(all_articles$pdf)
  all_articles<-all_articles[exist==F]
  
  all_articles<-all_articles[type=="journal-article"]
  #all_articles<-all_articles[sample(nrow(all_articles), nrow(all_articles))]
  #all_articles<-all_articles[publisher=="JSTOR"]
  
  error.pdf<-sprintf("../Data/Error.PDF/%s.%d.rda", journal.name, crossref.year)
  error.df<-NULL
  if (file.exists(error.pdf)){
    error.df<-readRDS(error.pdf)
    if (nrow(error.df)>0){
      #all_articles<-all_articles[!doi %in% error.df$doi]
    }
  }
  j=1
  #next()
  #all_articles$doi.suffix<-
  #  gsub("j.1365-2699", "j.1466-822x", all_articles$doi.suffix)
  if (F){
    
    all_articles.bak<-all_articles
    all_articles<-all_articles[grepl("wiley", all_articles$resource_primary_url)]
    all_articles<-all_articles[grepl("-", all_articles$doi.suffix)]
    all_articles$doi.suffix<-sprintf("%s.1", all_articles$doi.suffix)
    all_articles$doi.suffix <- sub("^j\\.(.*?)\\.(\\d{4}-\\d{4})\\.(.*?)$", "j.\\2.\\1.\\3", all_articles$doi.suffix)
    
    all_articles$doi.suffix<-str_replace(all_articles$doi.suffix, 
                                         pattern = "(?<=j\\.\\d{4}-\\d{4}\\.)\\d{4}",
                                         replacement = function(year) {
                                           as.character(as.integer(year) + 1)
                                         })
    all_articles$doi.suffix<-str_replace(all_articles$doi.suffix, "\\.(0*)(\\d+)\\.x$", function(m) {
      # 提取去掉前导0的数字部分
      num <- sub("^0+", "", str_match(m, "\\.(0*)(\\d+)\\.x$")[,3])
      paste0(".", num, ".x")
    })
    
    all_articles$doi.suffix<-gsub("0030-1299", "1600-0706", all_articles$doi.suffix)
    
    all_articles<-error.df
    
  }
  all_articles<-all_articles[title!="Volume Information"]
  start<-1
  if (nrow(all_articles)==0){
    next()
  }
  all_articles<-all_articles[sample(nrow(all_articles), nrow(all_articles))]
  for (j in c(start:nrow(all_articles))){
    item<-all_articles[j]
    
    print(sprintf("%d/%d, %s (%d/%d), %s, %s, %s", j, nrow(all_articles), 
                  conf.item$journal, i, nrow(journals),
                  item$publisher, item$resource_primary_url, item$pdf))
    
    filename<-item$pdf
    if (file.exists(filename)){
      next()
    }
    publisher<-item$publisher
    if (publisher=="JSTOR"){
      
      #next()
      #url<-sprintf("https://www.jstor.org/stable/pdf/%s.pdf", item$doi.suffix)
      #download.file(url, 
      #              destfile = filename, method="wget",
      #              extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
      #next()
    }
    
    #next()
    url<-item$resource_primary_url
    doi.prefix<-item$doi.prefix
    doi.suffix<-item$doi.suffix
    
    tryCatch({
      no.service<-T
      if (publisher=="MDPI AG"){
        next()
        no.service<-F
        pdf.url<-sprintf("%s/pdf", url)
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
        next()
      }
      
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
        Sys.sleep(10)
        
        next()
      }
      if (publisher=="Wiley" | grepl("\\.wiley\\.", url)){
        #next()
        no.service<-F
        
        
        url <- sprintf("https://api.wiley.com/onlinelibrary/tdm/v1/articles/%s/%s", doi.prefix, URLencode(doi.suffix, reserved = T))
        curl_str<-sprintf('curl -L -H "Wiley-TDM-Client-Token: %s" %s -o %s', wiley.api, url, filename)
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
        request_url <- sprintf("%s/%s/%s", base_url, doi.prefix, URLencode(doi.suffix, reserved = T))
        response <- GET(
          url = request_url,
          add_headers(
            `X-ELS-APIKey` = elsevier.api,
            `Accept` = "application/pdf"
          ),
          write_disk(path = filename, overwrite = TRUE),
          timeout(90) 
        )
        Sys.sleep(10)
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
      if (grepl("link\\.springer-ny\\.com", url)){
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
        Sys.sleep(10)
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
                           "Penerbit Universiti Sains Malaysia",
                           "Inter-Research Science Center")){
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
  
  
  if (!is.null(error.df)){
    error.df<-unique(rbindlist(list(error.df, all_articles)))
  }else{
    error.df<-all_articles
  }
  saveRDS(error.df, error.pdf)
}
