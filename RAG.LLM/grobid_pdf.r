grobid_url <- "http://172.16.120.92:8070/api/processFulltextDocument"

grobid_pdf<-function(pdf, xml){
  res <- POST(
    grobid_url,
    body = list(
      input = upload_file(pdf),
      segmentSentences=0,
      includeRawAffiliations=1,
      consolidatFunders=1)
  )
  
  
  if (status_code(res) == 200) {
    xml_content <- content(res, as = "text", encoding = "UTF-8")
    writeLines(xml_content, xml)
  } else {
    cat("Error:", status_code(res), "\n")
    if (status_code(res)==500){
      #file.rename(article_item[j]$pdf, 
      #            sprintf("%s/%s", middle_folder, pdf))
      #print("MOVED")
    }
  }
}