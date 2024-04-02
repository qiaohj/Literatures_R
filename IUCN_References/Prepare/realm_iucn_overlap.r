library(sf)
library(data.table)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
groups<-c("Amphibians", "Birds", "Mammals", "Odonata", "Reptiles")
group<-groups[2]
realm<-read_sf("../Shape/Ecoregions2017/Ecoregions2017.shp")
if (group %in% c("Amphibians")){
  iucn<-read_sf("../Shape/iucn_species_Ranges/AMPHIBIANS/AMPHIBIANS.shp")
  iucn$species_name<-iucn$binomial
}

if (group %in% c("Mammals")){
  iucn<-read_sf("../Shape/iucn_species_Ranges/MAMMALS1/MAMMALS.shp")
  iucn$species_name<-iucn$binomial
}

if (group %in% c("Birds")){
  #iucn<-read_sf("../Shape/iucn_species_Ranges/Birds/BOTW/BOTW.gdb")
  iucn<-read_sf("../Shape/iucn_species_Ranges/Birds/All_Species.shp")
  iucn$species_name<-iucn$SCINAME
}

if (group %in% c("Odonata")){
  iucn<-read_sf("../Shape/iucn_species_Ranges/Odonata/data_0.shp")
  iucn$species_name<-iucn$BINOMIAL
}
if (group %in% c("Reptiles")){
  iucn<-read_sf("../Shape/iucn_species_Ranges/Reptiles/modeled_reptiles.shp")
  iucn$species_name<-iucn$Binomial
}


realms<-unique(realm$REALM)
r<-realms[5]
results<-list()
for (r in realms){
  if (r=="N/A"){
    next()
  }
  print(paste(r, group))
  realm_p<-realm[which(realm$REALM==r),]
  realm_p<-st_union(realm_p)
  index<-st_intersects(realm_p, iucn)
  splist<-unique(iucn[index[[1]],]$species_name)
  item<-data.table(realm=r, species.name=splist)
  results[[length(results)+1]]<-item
  if (F){
    plot(realm_p)
    plot(splist$geometry, add=T, col="red")
  }
}
results<-rbindlist(results)
saveRDS(results, sprintf("../Data_IUCN_References/REALMS/%s.rda", group))
