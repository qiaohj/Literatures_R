library(data.table)
setwd("/media/huijieqiao/WD22T1/pubmed/Script")
setDTthreads(20)
reference_list<-readRDS("../Data/CrossRef_Full/references.rda")

categories<-list.dirs("../Data/CrossRef_By_Category", full.names = F)
categories<-categories[sample(length(categories), length(categories))]
category<-"WeedScience"
for (category in categories){
  print(category)
  target<-sprintf("../Data/CrossRef_By_Category/%s/article_references.rda", category)
  if (file.exists(target)){
    next
  }
  saveRDS(NULL, target)
  articles<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/articles.rda", category))
  i=1
  all_references<-list()
  for (i in c(1:length(reference_list))){
    print(paste(i, length(reference_list), category))
    ref_item<-reference_list[[i]]
    item<-ref_item[article_DOI %in% articles$doi]
    item<-item[!is.na(ref_DOI)]
    if (nrow(item)>0){
      all_references[[length(all_references)+1]]<-item
    }
    
  }
  all_references<-rbindlist(all_references)
  saveRDS(all_references, target)
}