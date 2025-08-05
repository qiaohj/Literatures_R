pdf2xml<-function(pdf.path, xml.path, grobid_url="http://172.16.120.92:8070/api/processFulltextDocument"){
  if (file.exists(xml.path)){
    return(xml.path)
  }
  print("Pasting PDF")
  res <- POST(
    grobid_url,
    body = list(
      input = upload_file(pdf.path),
      segmentSentences=0,
      includeRawAffiliations=1,
      consolidatFunders=1)
  )
  
  
  if (status_code(res) == 200) {
    xml_content <- content(res, as = "text", encoding = "UTF-8")
    writeLines(xml_content, xml.path)
    return(xml.path)
  } else {
    error.msg<-sprintf("Error: %d", status_code(res))
    return(error.msg)
    if (status_code(res)==500){
      #file.rename(pdf.path, 
      #            sprintf("%s/%s", middle_folder, pdf))
      #print("MOVED")
    }
  }
}


