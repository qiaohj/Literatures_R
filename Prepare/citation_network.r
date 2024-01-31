library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
setDTthreads(30)
full_ref<-readRDS("../Data/CrossRef_Full/references.rda")

category<-"Ecology"
article_ref<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/article_references.rda", category))
ref_id<-unique(c(article_ref$article_DOI, article_ref$ref_DOI))
ref_id<-unique(ref_id)
i=1

seq<-seq(1, length(full_ref), by=1000)
artile_list<-list()

for (i in seq){
  print(paste(i, length(full_ref)))
  if (i+999<=length(full_ref)){
    item<-rbindlist(full_ref[c(i:(i+999))])
  }else{
    item<-rbindlist(full_ref[c(i:length(full_ref))])
  }
  print(nrow(item))
  artiles<-item[ref_DOI %in% ref_id]
  print(nrow(artiles))
  if (nrow(artiles)>0){
    artile_list[[length(artile_list)+1]]<-artiles
  }
}
artile_df<-rbindlist(artile_list)
saveRDS(artile_df, sprintf("../Data/CrossRef_By_Category/%s/citation_network.rda", category))

