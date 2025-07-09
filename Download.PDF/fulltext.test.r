library(fulltext)
library(httr)
ft_get("10.1098/rspb.2020.2714")
pdf_path<-"/media/huijieqiao/WD22T_11/literatures/Temp/xxx.pdf"
grobid_url <- "http://172.16.120.92:8070/api/processFulltextDocument"
pdf_path<-"/media/huijieqiao/WD22T_11/literatures/Data/PDF/NATURE ECOLOGY AND EVOLUTION/S41559-021-01445-9.PDF"
res <- POST(
  grobid_url,
  body = list(
    input = upload_file(pdf_path),
    segmentSentences=0,
    includeRawAffiliations=1,
    consolidatFunders=1)
)

res <- POST(
  grobid_url,
  body = list(
    input = upload_file(pdf_path),
    segmentSentences=0,
    includeRawAffiliations=1,
    consolidatFunders=1)
)


xml_content <- content(res, as = "text", encoding = "UTF-8")
writeLines(xml_content, "/media/huijieqiao/WD22T_11/literatures/Temp/xxx.xml")
