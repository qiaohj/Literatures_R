library(data.table)
library(curl)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")

sp_list<-fread("../Data/IUCN_PDF/Mammals/Species_List/assessments.csv")
colnames(sp_list)
template<-"https://www.iucnredlist.org/species/pdf/%d"
for (i in c(1:nrow(sp_list))){
  
  
  item<-sp_list[i]
  url<-sprintf(template, item$assessmentId)
  
  target<-sprintf("../Data/IUCN_PDF/Mammals/PDFs/%d.pdf", item$assessmentId)
  if (file.exists(target)){
    next()
  }
  print(paste(i, nrow(sp_list)))
  tryCatch(
    {
      curl_download(url, target)
    },
    error = function(e) {
      cat("Error occurred: ", conditionMessage(e), "\n")
    }
  )
  #Sys.sleep(runif(1))
}

library(pdftools)

i=100
pattern <- "\\d{4}\\."
taxa_list<-fread("../Data/IUCN_PDF/Mammals/Species_List/taxonomy.csv")
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
sp_list$years_b_1990<- -1
sp_list$years_1991_2000<- -1
sp_list$years_2001_2010<- -1
sp_list$years_p_2010<- -1

for (i in c(1:nrow(sp_list))){
  print(paste(i, nrow(sp_list)))
  item<-sp_list[i]
  if (item$N_references!=-1){
    next()
  }
  target<-sprintf("../Data/IUCN_PDF/Mammals/PDFs/%d.pdf", item$assessmentId)
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
  years<-grepl(pattern, Bibliography)
  sp_list[i]$N_references<-length(years[years==T])
  yearstr<-str_extract(Bibliography, pattern)
  yearstr<-yearstr[!is.na(yearstr)]
  yearstr<-gsub("\\.", "", yearstr)
  yearstr<-as.numeric(yearstr)
  sp_list[i]$years_b_1990<-length(yearstr[yearstr<=1990])
  sp_list[i]$years_1991_2000<-length(yearstr[between(yearstr, 1991, 2000)])
  sp_list[i]$years_2001_2010<-length(yearstr[between(yearstr, 2001, 2010)])
  #years_2011_2020<-length(yearstr[between(yearstr, 2011, 2020)])
  sp_list[i]$years_p_2010<-length(yearstr[yearstr>=2011])
  
  
}
sp_list[N_references==-1]
fwrite(sp_list, "../Data/IUCN_PDF/Mammals/Species_List/assessments_ref.csv")
