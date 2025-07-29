
check.journal.pdf<-function(conf.item, crossref.year){
  #conf.item<-journals[journal==journal.names[journal.index]]
  journal.name<-conf.item$journal
  print(journal.name)
  target_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/GROBID.XML/%s", journal.name)
  middle_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Middle.PDF/%s", journal.name)
  if (!dir.exists(target_folder)){
    dir.create(target_folder)
  }
  
  if (!dir.exists(middle_folder)){
    dir.create(middle_folder)
  }
  grobid_url <- "http://172.16.120.92:8070/api/processFulltextDocument"
  pdf_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s", conf.item$journal)
  article_item<-getArticles(conf.item, all_journal_folders)
  if (nrow(article_item)==0){
    return(1)
  }
  article_item$pdf<-sprintf("%s/%s.PDF", 
                            pdf_folder, URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item<-article_item[type=="journal-article"]
  article_item$xml<-sprintf("%s/%s.XML", 
                            target_folder, URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item<-article_item[!file.exists(article_item$xml)]
  setorderv(article_item, "pdf", -1)
  article_item<-article_item[sample(nrow(article_item), nrow(article_item))]
  if (nrow(article_item)==0){
    return(1)
  }
  for (j in c(1:nrow(article_item))){
    pdf.file<-article_item[j]$pdf
    print(paste(j, nrow(article_item), pdf.file, i, nrow(journals), journal.name))
    
    pdf<-basename(pdf.file)
    target<-sprintf("%s/%s", target_folder, gsub("\\.PDF", "\\.XML", pdf))
    
    if (file.exists(target)){
      print("XML Exist SKIP")
      next()
    }
    if (!file.exists(pdf.file)){
      print("NO PDF SKIP")
      next()
    }
    text<-NULL
    
    text<-tryCatch({
      print("Reading PDF")
      pdf_text(pdf.file)
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
    if (is.null(text) & file.size(pdf.file)<1e5){
      file.rename(pdf.file, 
                  sprintf("%s/%s", middle_folder, pdf))
      print("MOVE NULL and SMALL FILE")
      next()
    }
    text<-unlist(text)
    text<-paste(text, collapse = '')
    text<-gsub(" ", "", text)
    
    if (grepl("AcceptedArticle", text)){
      file.rename(pdf.file, 
                  sprintf("%s/%s", middle_folder, pdf))
      print("MOVE AcceptedArticle")
      next()
    }
    pdf_path<-pdf.file
    print("Pasting PDF")
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
        #file.rename(pdf.file, 
        #            sprintf("%s/%s", middle_folder, pdf))
        #print("MOVED")
      }
    }
  }
  error.pdf<-sprintf("../Data/Error.PDF/%s.%d.rda", journal.name, crossref.year)
  article_item<-article_item[!file.exists(article_item$pdf)]
  
  saveRDS(article_item, error.pdf)
  print(sprintf("%d papers need to be downloaded", nrow(article_item)))
}
