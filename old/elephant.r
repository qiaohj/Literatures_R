library(easyPubMed)
setwd("D:/Experiments/PubMed_R/Script")
target<-sprintf("../pubmed/%s", "elephant")
dir.create(target, showWarnings = F)
#https://www.ncbi.nlm.nih.gov/books/NBK25500/#chapter1.Searching_a_Database
ml_query <- "(elephant[title]) AND China"
out4 <- batch_pubmed_download(pubmed_query_string = ml_query, batch_size = 100, 
                              dest_dir=target,
                              dest_file_prefix="elephant",
                              api_key="26140c4104533c4c89a8741a0ca7f4673d08")

target_file<-sprintf("../pubmed/%s/%s", "elephant", out4[1])
PM_tab1 <- table_articles_byAuth(target_file, autofill = TRUE, included_authors = "all")

target_file<-sprintf("../pubmed/%s/%s", "elephant", out4[2])
PM_tab2 <- table_articles_byAuth(target_file, autofill = TRUE, included_authors = "all")
PM_tab<-rbind(PM_tab1, PM_tab2)

write.csv(PM_tab, "../pubmed/elephant/pubmed.csv")
