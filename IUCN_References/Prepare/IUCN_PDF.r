library(data.table)
library(curl)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")

group<-"Amphibians"
sp_list<-fread(sprintf("../Data/IUCN_PDF/%s/Species_List/assessments.csv", group))
colnames(sp_list)
template<-"https://www.iucnredlist.org/species/pdf/%d"
#sp_list<-sp_list[sample(nrow(sp_list), nrow(sp_list))]
for (i in c(1:nrow(sp_list))){
  
  
  item<-sp_list[i]
  url<-sprintf(template, item$assessmentId)
  
  target<-sprintf("../Data/IUCN_PDF/%s/PDFs/%d.pdf", group, item$assessmentId)
  if (file.exists(target)){
    next()
  }
  #print(paste(i, nrow(sp_list), group))
  tryCatch(
    {
      curl_download(url, target)
    },
    error = function(e) {
      #cat("Error occurred: ", conditionMessage(e), "\n")
      
      print(sprintf("https://www.iucnredlist.org/species/%d/%d", 
                    item$internalTaxonId, item$assessmentId))
      
    }
  )
  #Sys.sleep(runif(1))
}



library(pdftools)
library(data.table)
library(curl)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")

group<-"Odonata"
sp_list<-fread(sprintf("../Data/IUCN_PDF/%s/Species_List/assessments.csv", group))
colnames(sp_list)
template<-"https://www.iucnredlist.org/species/pdf/%d"
i=100
pattern <- "( \\d{4}\\.|^[0-9]{4}\\.)"

taxa_list<-fread(sprintf("../Data/IUCN_PDF/%s/Species_List/taxonomy.csv", group))
taxa_list<-taxa_list[, c("internalTaxonId", "scientificName",
                         "className", "orderName", "familyName",
                         "authority")]
sp_list<-sp_list[,c("assessmentId", "internalTaxonId", 
                    "redlistCategory",
                    "yearPublished", "assessmentDate")]

time_obj <- as.POSIXct(sp_list$assessmentDate, format = "%Y-%m-%d %H:%M:%S")

# 提取年份
sp_list$assessmentYear <- format(time_obj, "%Y")
sp_list<-merge(sp_list, taxa_list, by=c("internalTaxonId"))
sp_list$N_references<- -1
sp_list$N_references_with_IUCN<- -1
sp_list$years_b_1990<- -1
sp_list$years_1991_2000<- -1
sp_list$years_2001_2010<- -1
sp_list$years_p_2010<- -1

item<-sp_list[assessmentId==127507675]
references_list<-list()
for (i in c(1:nrow(sp_list))){
  print(paste(i, nrow(sp_list)))
  item<-sp_list[i]
  if (item$N_references!=-1){
    next()
  }
  target<-sprintf("../Data/IUCN_PDF/%s/PDFs/%d.pdf", group, item$assessmentId)
  if (!file.exists(target)){
    asdf
    next()
  }
  #xxx<-pdf_data(target)
  text<-pdf_text(target)
  texts<-strsplit(text, "\n")
  texts<-unlist(texts)
  texts<-texts[texts!=""]
  texts<-trimws(texts)
  start_index<-which(texts=="Bibliography")
  end_index<-which(texts=="Citation")
  if (length(start_index)==0 | length(end_index)==0){
    print("error")
    asdf
  }
  
  Bibliography<-texts[(start_index+1):(end_index-1)]
  Bibliography<-Bibliography[!grepl("©", Bibliography)]
  Bibliography<-Bibliography[!grepl(item$assessmentId, Bibliography)]
  #Bibliography[grepl("Accessed:", Bibliography)]
  Bibliography<-Bibliography[!grepl("IUCN\\.", Bibliography)]
  Bibliography<-Bibliography[!grepl("Accessed:", Bibliography)]
  
  years<-grepl(pattern, Bibliography) | grepl("in press", tolower(Bibliography))
  references<-c()
  ref<-""
  if (length(years[years==T])>0){
    for (j in c(1:length(years))){
      if (years[j]==T){
        if (ref!=""){
          references<-c(references, ref)
          ref<-""
        }
        ref<-Bibliography[j]
      }else{
        if (ref==""){
          ref<-Bibliography[j]
        }else{
          ref<-sprintf("%s %s", ref, Bibliography[j])
        }
      }
      if (j==length(years) & ref!=""){
        references<-c(references, ref)
      }
    }
    references<-references[grepl(pattern, references) | grepl("in press", tolower(references))]
    
    references_year<-references
    references_year<-gsub("in press", " 2023.", references_year, ignore.case=T)
    yearstr<-str_extract(references_year, pattern)
    yearstr<-yearstr[!is.na(yearstr)]
    yearstr<-gsub("\\.", "", yearstr)
    yearstr<-trimws(yearstr)
    yearstr<-as.numeric(yearstr)
    if (length(references)!=length(yearstr)){
      asdf
      View(data.table(references, yearstr))
    }
    references<-data.table(internalTaxonId=sp_list[i]$internalTaxonId,
                           assessmentId=sp_list[i]$assessmentId,
                           redlistCategory=sp_list[i]$redlistCategory,
                           yearPublished=sp_list[i]$yearPublished,
                           assessmentDate=sp_list[i]$assessmentDate,
                           assessmentYear=sp_list[i]$assessmentYear,
                           scientificName=sp_list[i]$scientificName,
                           className=sp_list[i]$className,
                           orderName=sp_list[i]$orderName,
                           familyName=sp_list[i]$familyName,
                           authority=sp_list[i]$authority,
                           reference=references,
                           reference_year=yearstr)
    references_list[[length(references_list)+1]]<-references
  }
  
  sp_list[i]$N_references_with_IUCN<-length(years)
  sp_list[i]$N_references<-length(years[years==T])
  
  sp_list[i]$years_b_1990<-length(yearstr[yearstr<=1990])
  sp_list[i]$years_1991_2000<-length(yearstr[between(yearstr, 1991, 2000)])
  sp_list[i]$years_2001_2010<-length(yearstr[between(yearstr, 2001, 2010)])
  #years_2011_2020<-length(yearstr[between(yearstr, 2011, 2020)])
  sp_list[i]$years_p_2010<-length(yearstr[yearstr>=2011])
}

sp_list[N_references==-1]
fwrite(sp_list, sprintf("../Data/IUCN_PDF/Result/assessments_ref_%s.csv", group))
saveRDS(sp_list, sprintf("../Data/IUCN_PDF/Result/assessments_ref_%s.rda", group))
references_df<-rbindlist(references_list)
fwrite(references_df, sprintf("../Data/IUCN_PDF/Result/references_%s.csv", group))
saveRDS(references_df, sprintf("../Data/IUCN_PDF/Result/references_%s.rda", group))
