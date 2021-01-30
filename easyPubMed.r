library(easyPubMed)

YEAR<-"2018"
keyword<-"science"
target<-sprintf("../pubmed/%s", keyword)
dir.create(target, showWarnings = F)
#https://www.ncbi.nlm.nih.gov/books/NBK25500/#chapter1.Searching_a_Database
ml_query <- sprintf("%s[journal] AND %s[PD]", keyword, YEAR)
out4 <- batch_pubmed_download(pubmed_query_string = ml_query, batch_size = 100, 
                              dest_dir=target,
                              dest_file_prefix=YEAR,
                              api_key="26140c4104533c4c89a8741a0ca7f4673d08")

target_file<-sprintf("../pubmed/%s/%s", keyword, out4[1])
PM_tab <- table_articles_byAuth(target_file, autofill = TRUE, included_authors = "all")
#PM_tab$address <- substr(PM_tab$address, 1, 12)
PM_tab[50:70, c("pmid", "jabbrv", "year", "lastname", "address")]
