

download.pdf<-function(publisher, url, doi.prefix, doi.suffix, 
                       wiley.api="", elsevier.api="", filename, journal){
  code<- -5
  if (journal %in% no_open_access){
    return(-6)
  }
  if (grepl("link\\.springer-ny\\.com", url)){
    return(-2)
  }
  if (publisher %in% cannot.download.journal.list){
    return(-1)
  }
  if (grepl("nature\\.com", url)){
    pdf.url <- sprintf("%s.pdf", url)
    code<-download.pdf.url(pdf.url, url, host="nature.com", headers=NULL, filename=filename)
    return(code)
  }
  if (publisher=="American Society for Horticultural Science"){
    #https://journals.ashs.org/view/journals/jashs/96/1/article-p42.xml
    #https://journals.ashs.org/downloadpdf/view/journals/jashs/96/1/article-p42.pdf
    pdf.url<-"https://journals.ashs.org/hort/hort/published/rest/pdf-watermark/v1/journals/jashs/96/1/article-p42.pdf/watermark-pdf/"
    
    return(-1)
    
    pdf.url<-gsub("journals.ashs.org/view/journals", "journals.ashs.org/downloadpdf/view/journals", url)
    pdf.url<-gsub("\\.xml", "\\.pdf", pdf.url)
    code<-download.pdf.url(pdf.url, url, host="ashs.org", headers=NULL, filename=filename)
    return(code)
  }
  if (publisher %in% html.download.journal.list){
    code<-download.html(url, host=NULL, filename=filename)
  }
  if (publisher=="Scientific Societies"){
    
  }
  
  if (publisher=="PeerJ"){
    pdf.url<-sprintf("%s.pdf",url)
    code<-download.pdf.url(pdf.url, url, host="peerj.com", headers=NULL, filename=filename)
  }
  
  if (publisher=="MDPI AG"){
    pdf.url<-sprintf("%s/pdf", url)
    code<-download.pdf.url(pdf.url, url, host="mdpi.com", headers=NULL, filename=filename)
  }
  if (endsWith(tolower(url), ".pdf")){
    code<-download.pdf.url(url, url, host="google.com", headers=NULL, filename=filename)
  }
  if (grepl("\\.plos\\.", url)){
    code<-download.html(url, host="plos.org", filename)
    return(-10)
  }
  if (grepl("\\.biomedcentral\\.", url)){
    code<-download.html(url, host="biomedcentral.com", filename)
  }
  if (publisher=="Smithsonian Institution"){
    pdf.url<-gsub("/part/", "/partpdf/", url)
    code<- download.pdf.url(pdf.url, url, host="si.edu", headers=NULL, filename=filename)
    
  }
  
  if (publisher=="American Association for the Advancement of Science (AAAS)"){
    return(-1)
    pdf.url<-sprintf("https://www.science.org/doi/pdf/%s?download=true",
                     gsub("https://www.science.org/doi/", "", url))
    code<-download.pdf.url(pdf.url, url, host="science.org", headers=NULL, filename=filename)
  }
  if (publisher=="Proceedings of the National Academy of Sciences"){
    return(-1)
    pdf.url<-sprintf("https://pnas.org/doi/pdf/%s?download=true",
                     gsub("https://pnas.org/doi/", "", url))
    code<-download.pdf.url(pdf.url, url, host="pnas.org", headers=NULL, filename=filename)
  }
  
  if (publisher=="Wiley" | grepl("\\.wiley\\.", url)){
    code<-download.wiley(wiley.api, url, doi.prefix, doi.suffix, filename)
  }
  if (publisher=="Elsevier BV"){
    code<-download.elsevier(elsevier.api, url, doi.prefix, doi.suffix, filename)
  }
  return(code)
}

download.pdf.url<-function(pdf.url, url, host=NULL, headers=NULL, filename=NULL){
  if (is.null(headers)){
    if (!is.null(host)){
    headers <- c(
      Accept = "text/html,application/xhtml+xml,application/pdf,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
      `Accept-Encoding` = "gzip, deflate, br",
      `Accept-Language` = "en-US,en;q=0.5",
      Connection = "keep-alive",
      Referer = url,
      `Sec-Fetch-Dest` = "document",
      `Sec-Fetch-Mode` = "navigate",
      `Sec-Fetch-Site` = "same-origin",
      `Sec-Fetch-User` = "?1",
      `Upgrade-Insecure-Requests` = "1",
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:95.0) Gecko/20100101 Firefox/95.0"
    )
    }else{
      headers <- c(
        Accept = "text/html,application/xhtml+xml,application/pdf,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
        `Accept-Encoding` = "gzip, deflate, br",
        `Accept-Language` = "en-US,en;q=0.5",
        Connection = "keep-alive",
        host = host,
        Referer = url,
        `Sec-Fetch-Dest` = "document",
        `Sec-Fetch-Mode` = "navigate",
        `Sec-Fetch-Site` = "same-origin",
        `Sec-Fetch-User` = "?1",
        `Upgrade-Insecure-Requests` = "1",
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:95.0) Gecko/20100101 Firefox/95.0"
      )
    }
  }
  resp <- httr::GET(pdf.url, httr::add_headers(.headers = headers), 
                    httr::write_disk(filename, overwrite = TRUE), 
                    httr::progress(),
                    httr::config(proxy = ""))
  
  content_type <- resp$headers$`content-type`
  if (httr::status_code(resp) == 200 && grepl("application/pdf", content_type, ignore.case = TRUE)) {
    message("PDF Downloaded successfully: ", filename)
    return(1)
  } else {
    content.text<-content(resp)
    if (is.null(content.text)){
      content.text<-""
    }
    if (is.null(content_type)){
      content_type<-""
    }
    message(sprintf("Download failed: HTTP %s, Content-Type=%s, Content=%s", 
                    httr::status_code(resp), content_type, content.text))
    unlink(filename)
    return(0)
  }
  return(-4)
}
download.wiley<-function(wiley.api, url, doi.prefix, doi.suffix, filename){
  print(wiley.api)
  pdf.url <- sprintf("https://api.wiley.com/onlinelibrary/tdm/v1/articles/%s/%s", doi.prefix, URLencode(doi.suffix, reserved = T))
  token <- wiley.api
  headers <- c("Wiley-TDM-Client-Token" = token)
  download.pdf.url(pdf.url, url, host="wiley.com", headers=headers, filename=filename)
}
download.elsevier<-function(elsevier.api, url, doi.prefix, doi.suffix, filename){
  print(elsevier.api)
  base_url <- "https://api.elsevier.com/content/article/doi"
  pdf.url <- sprintf("%s/%s/%s", base_url, doi.prefix, URLencode(doi.suffix, reserved = T))
  headers<-c(`X-ELS-APIKey` = elsevier.api,
             `Accept` = "application/pdf")
  download.pdf.url(pdf.url, url, host="elsevier.com", headers=headers, filename=filename)
}
download.html<-function(url, host=host, filename=filename){
  webpage <- safe_read_html(url)
  if (is.null(webpage)){
    return(-6)
  }
  pdf.url <- webpage %>%
    html_element("meta[name='citation_pdf_url']") %>%
    html_attr("content")
  if (is.na(pdf.url)){
    return(-3)
  }
  
  if (!startsWith(pdf.url, "http")){
    pdf.url<-sprintf("%s%s", get_base_url(url), pdf.url)
  }
  download.pdf.url(pdf.url, url, host=host, headers=NULL, filename=filename)
  
}


