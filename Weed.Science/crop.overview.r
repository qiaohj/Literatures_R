library(terra)
library(data.table)
library(sf)
library(ggplot2)

setwd("/media/huijieqiao/WD22T_11/literatures/Script")

crops<-fread("../Weed/mapspam/crop_list.csv")
template<-"../Weed/mapspam/TIFF/spam2010V2r0_global_%s_%s_%s.tif"

#v: Variables
#**************
#*_A_*		physical area
#*_H_*		harvested area
#*_P_*		production
#*_Y_*		yield
#*_V_agg_*	value of production, aggregated to all crops, food and non-food (see below)

variable<-"P"

#t: Technologies
#******************
#*_TA	all technologies together, ie complete crop
#*_TI	irrigated portion of crop
#*_TH	rainfed high inputs portion of crop
#*_TL	rainfed low inputs portion of crop
#*_TS	rainfed subsistence portion of crop
#*_TR	rainfed portion of crop (= TA - TI, or TH + TL + TS)

technology<-"A"

countries<-read_sf("../Weed/WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp")

crop_labels<-unique(toupper(crops$`SPAM name`))
crop_labels<-crop_labels[crop_labels!=""]
iso3<-unique(countries$ISO_A3)
iso3<-iso3[iso3!=-99]
results<-list()
for (crop in crop_labels){
  tif<-sprintf(template, variable, crop, technology)
  r<-rast(tif)
  for (iso in iso3){
    print(paste(crop, iso))
    country<-countries[which(countries$ISO_A3==iso),]
    r_item<-crop(r, country)
    r_item<-mask(r_item, country)
    country_area<-expanse(r_item, unit="km")[1,2]
    vitem<-values(r_item)
    vitem[vitem==0]<-NA
    values(r_item)<-vitem
    v<-sum(vitem, na.rm=T)
    
    
    area<-expanse(r_item, unit="km")[1,2]
    result_item<-data.table(crop=crop, iso3=iso, country=country[1,]$FORMAL_EN,
                            v=v, area=area, country_area=country_area)
    results[[length(results)+1]]<-result_item
  }
}
results<-rbindlist(results)
hist(results[crop=="WHEA"]$v)


crop_iso_sum<-results[, .(iso_sum_v=sum(v)), by=c("iso3", "country")]

crop_crop_sum<-results[, .(crop_sum_v=sum(v),
                           area_sum_v=sum(area)), by=c("crop")]
setorderv(crop_crop_sum, "crop_sum_v", -1)
setorderv(crop_crop_sum, "area_sum_v", -1)


result_full<-merge(merge(results, crop_iso_sum, by=c("iso3", "country")), crop_crop_sum, by="crop")
result_full<-merge(result_full, crops[, c("crop", "crop.name")], by="crop")
result_full<-result_full[v>0]


crop_iso_sum<-result_full[, .(iso_sum_v=sum(v),
                              iso_sum_area=sum(area)), 
                          by=c("iso3", "country")]
setorderv(crop_iso_sum, "iso_sum_v", -1)
crop_iso_sum[, cum.iso_sum_v := cumsum(iso_sum_v)]
sum_v<-sum(crop_iso_sum$iso_sum_v)
crop_iso_sum$per<-crop_iso_sum$iso_sum_v/sum_v * 100
crop_iso_sum$cum.per<-crop_iso_sum$cum.iso_sum_v/sum_v * 100
crop_crop_sum<-result_full[, .(crop_sum_v=sum(v),
                               area_sum_v=sum(area)), by=c("crop", "crop.name")]


saveRDS(crop_iso_sum, "../Weed/Data/crop_iso_sum.rda")

saveRDS(result_full, "../Weed/Data/crop_iso_details.rda")

crop_crop_sum$token<-""
for (i in c(1:nrow(crop_crop_sum))){
  name<-crop_crop_sum[i]$crop.name
  tokens<-get_Tokens(mydic, name, "", "")
  if (nrow(tokens)>1){
    crop_crop_sum[i]$token<-tokens[3]$Word
  }else{
    crop_crop_sum[i]$token<-tokens[1]$Word
  }
}

saveRDS(crop_crop_sum, "../Weed/Data/crop_crop_sum.rda")

crop_crop_sum$crop.name
