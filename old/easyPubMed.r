library(easyPubMed)
library(dplyr)
setwd("Y:/Experiments/PubMed/PubMed_R")


keywords<-c("elephant", "elephants", "Elephantidae")
PM_tabs<-NULL
for (keyword in keywords){
  print(keyword)
  target<-sprintf("../Objects/Elephant/%s", keyword)
  dir.create(target, showWarnings = F)
  #https://www.ncbi.nlm.nih.gov/books/NBK25500/#chapter1.Searching_a_Database
  #ml_query <- sprintf("%s+%s[title] AND %s[PD]", keyword, "China", YEAR)
  ml_query <- sprintf("%s+%s", keyword, "China")
  out4 <- batch_pubmed_download(pubmed_query_string = ml_query, batch_size = 100, 
                                dest_dir=target,
                                dest_file_prefix=YEAR,
                                api_key="")
  for (t in ou", target, t)
    PM_tab <- table_articles_byAuth(target_file, autofill = TRUE, included_authors = "all")
    if (is.null(PM_tabs)){
      PM_tabs<-PM_tab
    }else{
      PM_tabs<-bind_rows(PM_tabs, PM_tab)
    }
  }
}
dim(PM_tabs)
write.csv(PM_tabs, "../Objects/Elephant/pubmed_20201119.csv")
length(unique(PM_tabs$doi))
#PM_tab$address <- substr(PM_tab$address, 1, 12)
PM_tab[50:70, c("pmid", "jabbrv", "year", "lastname", "address")]
PM_tab$abstract
