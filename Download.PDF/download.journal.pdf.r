
source("Download.PDF/download.pdf.r")

download.journal.pdf<-function(conf.item, crossref.year, wiley.api, elsevier.api){
  journal.name<-conf.item$journal
  print(journal.name)
  download.pdf.file<-sprintf("../Data/Error.PDF/%s.%d.rda", journal.name, crossref.year)
  if (!file.exists(download.pdf.file)){
    return(0)
  }
  all_articles<-readRDS(download.pdf.file)
  if (journal.name %in% c("CURRENT SCIENCE")){
    all_articles[, resource_primary_url:=gsub("http://", "https://", resource_primary_url)]
  }
  all_articles<-
    all_articles<-all_articles[sample(nrow(all_articles), nrow(all_articles))]
  if (nrow(all_articles)==0){
    return(0)
  }
  for (j in c(1:nrow(all_articles))){
    item<-all_articles[j]
    
    print(sprintf("%d/%d, %s (%d/%d), %s, %s, %s", j, nrow(all_articles), 
                  conf.item$journal, i, nrow(journals),
                  item$publisher, item$resource_primary_url, item$pdf))
    
    filename<-item$pdf.path
    if (file.exists(filename)){
      print("Exist! Skip!")
      next()
    }
    publisher<-item$publisher
    url<-item$resource_primary_url
    doi.prefix<-item$doi.prefix
    doi.suffix<-item$doi.suffix
    journal<-conf.item$journal
    code<-
    tryCatch({
      download.pdf(publisher, url, doi.prefix,doi.suffix, wiley.api, elsevier.api, filename, journal)
      
    },
    error = function(e) {
      message("Error: ", e$message)
      return(-7)
    },
    warning = function(w) {
      message("Warning: ", w$message)
      
    },
    finally = {
      
    })
    print(sprintf("Download stauts code: %d", code))
    if (is.null(code)){
      code<-0
    }
    if (code>0){
      Sys.sleep(code)
    }
  }
}
