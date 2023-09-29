library(raster)
library(data.table)
setwd("/media/huijieqiao/WD22T_50/CMIP6/Rscript")
models<-c("CNRM-ESM2-1", "EC-Earth3-Veg", "GFDL-ESM4", "MIROC6", "MRI-ESM2-0")
ssps<-c("SSP119", "SSP245", "SSP585")
vars<-c("pr", "tasmin", "tasmax")
years<-c(1850:2100)
months<-c(1:12)
biovar_base<-"../Bioclimatic_varlables/%s/%s/%d/%s_%s_%d_raw_based.tif"
monthly_base<-"../Raster/%s/%s/%s/%d/%s_%s_%s_%d_%d_raw.tif"

monthly_list<-data.table(expand.grid(model=models, ssp=ssps, var=vars, year=years, month=months))
biovar_list<-data.table(expand.grid(model=models, ssp=ssps, year=years))
monthly_str<-sprintf(monthly_base, monthly_list$model, monthly_list$ssp, monthly_list$var, monthly_list$year,
                     monthly_list$model, monthly_list$ssp, monthly_list$var, monthly_list$year, monthly_list$month)
biovar_str<-sprintf(biovar_base, biovar_list$model, biovar_list$ssp, biovar_list$year,
                    biovar_list$model, biovar_list$ssp, biovar_list$year)

fulllist<-rbindlist(list(monthly_list, biovar_list), fill=T)
fulllist$str<-c(monthly_str, biovar_str)

fulllist$status<-""
for (i in c(8764:nrow(fulllist))){
  print(paste(i, nrow(fulllist), fulllist[i]$str))
  if (file.exists(fulllist[i]$str)){
    r<-raster(fulllist[i]$str)
  }else{
    fulllist[i]$status<-"not exist"
  }
}

missingsets<-fulllist[status!="" & model!="GFDL-ESM4"]
missingsets<-missingsets[!(model %in% c("EC-Earth3-Veg") & ssp %in% c("SSP585") & var %in% c("tasmin", "tasmax"))]
missingsets_monthly<-missingsets[!is.na(var)]
setorderv(missingsets_monthly, c("model", "ssp", "var", "year", "month"))
missingsets_biovar<-missingsets[is.na(var)]
missingsets_biovar<-missingsets_biovar[!(model %in% c("EC-Earth3-Veg") & ssp %in% c("SSP585"))]
fulllist[model=="MIROC6" & ssp=="SSP119" & is.na(var) & year==1850]$status<-""
saveRDS(fulllist, "../Data/process_status.rda")
