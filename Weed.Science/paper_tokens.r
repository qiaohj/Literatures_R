library(data.table)
library(RWeka)
library(ggrepel)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
source("functions.r")
category<-"WeedScience"
references<-readArticle(category)
mydic<-readRDS("../Data/word_wrapper/irregular.verbs.rda")
token_title<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/token_title.rda", category))
token_abstract<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/token_abstract.rda", category))
tokens<-rbindlist(list(token_title, token_abstract))
crop_crop_sum<-readRDS("../Weed/Data/crop_crop_sum.rda")
crop_crop_sum$N_ref<-0
crop_references<-list()
for (i in c(1:nrow(crop_crop_sum))){
  token<-crop_crop_sum[i]$token
  ref<-unique(tokens[Word == token]$doi)
  if (length(ref)>0){
    crop_references[[crop_crop_sum[i]$crop.name]]<-data.table(crop=crop_crop_sum[i]$crop, doi=ref)
  }
  print(paste(crop_crop_sum[i]$crop.name, length(ref)))
  crop_crop_sum[i]$N_ref<-length(ref)
}
crop_references<-rbindlist(crop_references)
saveRDS(crop_references, "../Weed/Data/crop_references.rda")
saveRDS(crop_crop_sum, "../Weed/Data/crop_crop_sum.rda")
setorderv(crop_crop_sum, "N_ref", -1)

crop_crop_sum<-readRDS("../Weed/Data/crop_crop_sum.rda")

141.1764705882353

1206231657

ggplot(crop_crop_sum)+
  geom_point(aes(x=area_sum_v, y=N_ref, size=crop_sum_v))+
  geom_text_repel(aes(x=area_sum_v, y=N_ref, label=crop.name))
