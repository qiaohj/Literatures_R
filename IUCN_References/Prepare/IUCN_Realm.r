library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
groups<-c("Amphibians", "Birds", "Mammals", "Odonata", "Reptiles")
for (group in groups){
  sp_list<-fread(sprintf("../Data_IUCN_References/IUCN_PDF/%s/Species_List/assessments.csv", group))
  realms<-str_split(sp_list$realm, "\\|")
  all_realms<-list()
  
  for (i in c(1:nrow(sp_list))){
    print(paste(i, nrow(sp_list), group))
    item<-data.table(internalTaxonId=sp_list[i]$internalTaxonId,
                     assessmentId=sp_list[i]$internalTaxonId,
                     species.name=sp_list[i]$scientificName,
                     realm=realms[i][[1]])
    all_realms[[i]]<-item

  }
  all_realms<-rbindlist(all_realms)
  saveRDS(all_realms, sprintf("../Data_IUCN_References/REALMS/%s_IUCN.rda", group))
}
