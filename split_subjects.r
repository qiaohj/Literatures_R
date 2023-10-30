library(data.table)
setDTthreads(20)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
categories<-list.dirs("../Data/CrossRef_By_Category", full.names = F)
categories<-categories[sample(length(categories), length(categories))]
for (category in categories){
  print(category)
  target<-sprintf("../Data/CrossRef_By_Category/%s/article_subject_splitted.rda", category)
  if (file.exists(target)){
    next
  }
  saveRDS(NULL, target)
  article_subject<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/article_subject.rda", category))
  setindexv(article_subject, "article_DOI")
  out <- article_subject[, list(Subject = trimws(unlist(strsplit(Subject, ",")))), 
                         by=article_DOI]
  
  out<-unique(out)
  saveRDS(out, target)
}
