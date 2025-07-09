#docker pull lfoppiano/grobid:0.8.2
#创建一个临时的config，用于修改
#docker run -d --name grobid-temp lfoppiano/grobid:0.8.2
#docker cp grobid-temp:/opt/grobid/grobid-home/config/. /Users/huijieqiao/GIT/Literatures_R/RAG.LLM/grobid_config
#docker stop grobid-temp
#docker rm grobid-temp
#docker run --rm --init -p 8070:8070 -p 8071:8071 -v /Users/huijieqiao/GIT/Literatures_R/RAG.LLM/grobid_config:/opt/grobid/grobid-home/config lfoppiano/grobid:0.8.2
#http://localhost:8070

library(httr)
setwd("~/GIT/Literatures_R")
grobid_url <- "http://localhost:8070/api/processFulltextDocument"

pdfs<-list.files("/Users/huijieqiao/PDF/TEST", pattern="\\.pdf", full.names = T)
pdf_path<-pdfs[1]

for (pdf_path in pdfs){
  print(pdf_path)
  res <- POST(
    grobid_url,
    body = list(
      input = upload_file(pdf_path),
      segmentSentences=1,
      consolidateHeader=1,
      includeRawAffiliations=1,
      consolidatFunders=1,
      includeRawCitations=1)
  )
  target<-gsub("\\.pdf", "\\.xml", pdf_path)
  
  if (status_code(res) == 200) {
    xml_content <- content(res, as = "text", encoding = "UTF-8")
    writeLines(xml_content, target)
  } else {
    cat("Error:", status_code(res), "\n")
  }
}
