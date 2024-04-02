library(jsonlite)
library(data.table)
library(stringr)
library(stringi)
options(warn=2)

setwd("/media/huijieqiao/WD22T_11/literatures/Script")
groups<-c("Amphibians", "Birds", "Mammals", "Odonata", "Reptiles")
args = commandArgs(trailingOnly=TRUE)

group<-as.numeric(args[1])
if (is.na(group)){
  group<-groups[3]
}else{
  group<-groups[group]
}
sp_list<-fread(sprintf("../Data_IUCN_References/IUCN_PDF/%s/Species_List/assessments.csv", group))
taxa_list<-fread(sprintf("../Data_IUCN_References/IUCN_PDF/%s/Species_List/taxonomy.csv", group))
taxa_list<-taxa_list[, c("internalTaxonId", "scientificName",
                         "className", "orderName", "familyName",
                         "authority", "genusName", "speciesName")]
sp_list<-sp_list[,c("assessmentId", "internalTaxonId", 
                    "redlistCategory",
                    "yearPublished", "assessmentDate", "realm")]

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

references_list<-list()
i=4330
#nos<-c()
for (i in c(1:nrow(sp_list))){
  
  item<-sp_list[i]
  if (item$assessmentId==177177761){
    #asdf
  }
  #next()
  if (item$N_references!=-1){
    next()
  }
  target<-sprintf("../Data_IUCN_References/IUCN_API/%s/APIs/%d.json", group, item$assessmentId)
  if (!file.exists(target)){
    asdf
    next()
  }
  print(paste(i, nrow(sp_list), group, target))
  d<-fromJSON(target)
  if (length(d$references)==0){
    next()
  }
  references<-d$references
  
  
  #references<-references[references!=""]
  
  
  references_dt<-data.table(internalTaxonId=sp_list[i]$internalTaxonId,
                            assessmentId=sp_list[i]$assessmentId,
                            redlistCategory=sp_list[i]$redlistCategory,
                            yearPublished=sp_list[i]$yearPublished,
                            assessmentDate=sp_list[i]$assessmentDate,
                            assessmentYear=sp_list[i]$assessmentYear,
                            scientificName=sp_list[i]$scientificName,
                            className=sp_list[i]$className,
                            orderName=sp_list[i]$orderName,
                            familyName=sp_list[i]$familyName,
                            genusName=sp_list[i]$genusName,
                            speciesName=sp_list[i]$speciesName,
                            authority=sp_list[i]$authority,
                            realm=sp_list[i]$realm,
                            reference=references)
  #references_dt$is_IUCN<-(grepl("IUCN\\.", references) & grepl("Accessed:", references))
  references_dt$is_IUCN<-grepl("IUCN\\.", references)
  references_list[[length(references_list)+1]]<-references_dt
}
references_df<-rbindlist(references_list)
#fwrite(references_df, sprintf("../Data_IUCN_References/IUCN_PDF/Result/references_%s.csv", group))
saveRDS(references_df, sprintf("../Data_IUCN_References/References/references_%s_api.rda", group))



if (F){
  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  df_pdf<-readRDS(sprintf("../Data_IUCN_References/IUCN_PDF/Result/references_%s.rda", groups[1]))
  groups<-c("Amphibians", "Birds", "Mammals", "Odonata", "Reptiles")
  group<-"Mammals"
  for (group in groups){
    print(group)
    df_api<-readRDS(sprintf("../Data_IUCN_References/References/references_%s_api.rda", group))
    
    df<-df_api
    df$reference<-trimws(df$reference)
    
    #View(df[assessmentId=="208081433"])
    df$year_score<-10
    pattern <- " \\d{4}\\.| \\d{4}\\)| \\d{4}[a-zA-Z]\\.|^\\d{4}\\.| \\d{4} \\[In"
    yearstr<-df$reference
    yearstr<-gsub("in press", " 2024.", yearstr, ignore.case=T)
    yearstr<-gsub("in prep", " 2024.", yearstr, ignore.case=T)
    
    yearstr<-str_extract(yearstr, pattern)
    df$reference_yearstr<-yearstr
    yearstr<-gsub("[a-zA-Z]\\.", "", yearstr)
    yearstr<-gsub("\\)", "", yearstr)
    yearstr<-gsub("\\[In", "", yearstr)
    yearstr<-gsub("\\.", "", yearstr)
    yearstr<-trimws(yearstr)
    yearstr<-as.numeric(yearstr)
    df$reference_year<-yearstr
    
    df[is.na(reference_year)]$year_score<-9
    yearstr<-df[is.na(reference_year)]$reference
    index<-str_locate(fixed(yearstr), "Access")[,2]
    substr<-str_sub(fixed(yearstr), index, str_length(yearstr))
    df[is.na(reference_year)]$reference_yearstr<-substr
    years<-str_extract(substr, "\\.\\d{4}| \\d{4}|\\-\\d{4}")
    years<-gsub("\\.", "", years)
    years<-gsub("\\-", "", years)
    years<-as.numeric(trimws(years))
    df[is.na(reference_year)]$reference_year<-years
    
    df[is.na(reference_year)]$year_score<-8
    yearstr<-df[is.na(reference_year)]$reference
    years<-str_extract(yearstr, " \\d{4} \\.")
    df[is.na(reference_year)]$reference_yearstr<-years
    years<-gsub("\\.", "", years)
    years<-as.numeric(trimws(years))
    df[is.na(reference_year)]$reference_year<-years
    
    df[is.na(reference_year)]$year_score<-7
    yearstr<-df[is.na(reference_year)]$reference
    years<-str_extract(yearstr, "\\(\\d{4}\\)")
    df[is.na(reference_year)]$reference_yearstr<-years
    years<-gsub("\\(", "", years)
    years<-gsub("\\)", "", years)
    years<-as.numeric(trimws(years))
    df[is.na(reference_year)]$reference_year<-years
    
    df[is.na(reference_year)]$year_score<-6
    yearstr<-df[is.na(reference_year)]$reference
    years<-str_extract(yearstr, "\\[\\d{4}\\]")
    df[is.na(reference_year)]$reference_yearstr<-years
    years<-gsub("\\[", "", years)
    years<-gsub("\\]", "", years)
    years<-as.numeric(trimws(years))
    df[is.na(reference_year)]$reference_year<-years
    
    df[is.na(reference_year)]$year_score<-5
    yearstr<-df[is.na(reference_year)]$reference
    years<-str_extract(yearstr, "\\d{4}\\)")
    df[is.na(reference_year)]$reference_yearstr<-years
    years<-gsub("\\)", "", years)
    years<-as.numeric(trimws(years))
    df[is.na(reference_year)]$reference_year<-years
    
    
    df[is.na(reference_year)]$year_score<-4
    yearstr<-df[is.na(reference_year)]$reference
    years<-str_extract(yearstr, " \\d{4}")
    df[is.na(reference_year)]$reference_yearstr<-years
    years<-as.numeric(trimws(years))
    df[is.na(reference_year)]$reference_year<-years
    
    df[is.na(reference_year)]$year_score<-3
    yearstr<-df[is.na(reference_year)]$reference
    years<-str_extract(yearstr, "\\d{4}")
    df[is.na(reference_year)]$reference_yearstr<-years
    years<-as.numeric(trimws(years))
    df[is.na(reference_year)]$reference_year<-years
    
    
    df[is.na(reference_year)]$year_score<-2
    df$is_IUCN<-grepl("iucn", tolower(df$reference))
    if (F){
      grepl("iucn", "iucnasdf")
    }
    df[!between(reference_year, 1700, 2025)]$reference_year<-NA
    df[is.na(reference_year)]$year_score<-0
    df[is.na(reference_year)]$reference
    df$leng<-str_length(df$reference)
    setorderv(df, c("leng","reference"), c(1, 1))
    #View(df)
    #View(df[is.na(reference_year)])
    #View(df[!between(reference_year, 1700, 2025)])
    
    df[is.na(reference_year)]$reference_yearstr<-NA
    
    start_index<-str_locate(df$reference, fixed(df$reference_yearstr))[,2]
    ref_temp<-trimws(str_sub(df$reference, start_index+1))
    end_index<-str_locate(ref_temp, "\\.")[,1]
    ref_temp<-trimws(str_sub(ref_temp, 1, end_index-1))
    df$reference_title<-cleanFun(ref_temp)
    
    index<-stri_locate_last(df$reference, regex="<i>.*?</i>")
    df$reference_container<-str_sub(fixed(df$reference), index[,1]+3, index[,2]-4)
    
    #extract doi
    doi_str<-df$reference
    doi_str<-tolower(doi_str)
    pattern<-"(?<![a-zA-Z])doi(?![a-zA-Z])"
    doi_start<-str_locate(fixed(doi_str), pattern)[,1]
    doi_str<-trimws(str_sub(doi_str, doi_start, length(doi_str)))
    df$reference_doi<-doi_str
    
    df$reference_rest<-df$reference
    
    df[reference_doi==""]$reference_doi<-NA
    df[!is.na(reference_doi)]$reference_rest<-str_replace(
      fixed(tolower(df[!is.na(reference_doi)]$reference_rest)), 
      fixed(tolower(df[!is.na(reference_doi)]$reference_doi)),
      ""
    )
    
    df[reference_title==""]$reference_title<-NA
    df[!is.na(reference_title)]$reference_rest<-str_replace(
      fixed(tolower(df[!is.na(reference_title)]$reference_rest)), 
      fixed(tolower(df[!is.na(reference_title)]$reference_title)),
      ""
    )
    df[reference_container==""]$reference_container<-NA
    df[!is.na(reference_container)]$reference_rest<-str_replace(
      fixed(tolower(df[!is.na(reference_container)]$reference_rest)), 
      fixed(tolower(df[!is.na(reference_container)]$reference_container)),
      ""
    )
    df$reference_clean<-tolower(trimws(cleanFun(df$reference)))
    df$reference_title_clean<-tolower(trimws(cleanFun(df$reference_title)))
    df$reference_container_clean<-tolower(trimws(cleanFun(df$reference_container)))
    df$reference_rest_clean<-tolower(trimws(cleanFun(df$reference_rest)))
    df$reference_doi_clean<-tolower(trimws(cleanFun(df$reference_doi)))
    
    
    saveRDS(df, sprintf("../Data_IUCN_References/References/references_%s_cleaned.rda", group))
  }
}
