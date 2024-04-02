library(pdftools)
library(data.table)
library(stringr)
options(warn=2)

setwd("/media/huijieqiao/WD22T_11/literatures/Script")
groups<-c("Amphibians", "Birds", "Mammals", "Odonata", "Reptiles")
args = commandArgs(trailingOnly=TRUE)

group<-as.numeric(args[1])
if (is.na(group)){
  group<-groups[1]
}else{
  group<-groups[group]
}
sp_list<-fread(sprintf("../Data_IUCN_References/IUCN_PDF/%s/Species_List/assessments.csv", group))
taxa_list<-fread(sprintf("../Data_IUCN_References/IUCN_PDF/%s/Species_List/taxonomy.csv", group))
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

references_list<-list()
i=1
#nos<-c()
for (i in c(1:nrow(sp_list))){
  print(paste(i, nrow(sp_list), group))
  item<-sp_list[i]
  if (item$assessmentId==177177761){
    #asdf
  }
  #next()
  if (item$N_references!=-1){
    next()
  }
  target<-sprintf("../Data_IUCN_References/IUCN_PDF/%s/PDFs/%d.pdf", group, item$assessmentId)
  if (!file.exists(target)){
    asdf
    next()
  }
  
  d<-pdf_data(target)
  
  
  
  inline_dist<-18
  line_dist<-23
  
  
  all_d<-rbindlist(d)
  #if (all_d[1]$text!="The"){
  #  nos<-c(nos, item$assessmentId)
  #}
  #next()
  start_line<-all_d[space==FALSE & text=="Bibliography", which = TRUE]
  end_line<-all_d[space==FALSE & text=="Citation", which = TRUE]
  
  if (length(start_line)!=1 | length(end_line)!=1){
    asdf
  }
  
  ref_items<-all_d[c((start_line+1):(end_line-1))]
  ref_items$line_space<-0
  ref_items[c(2:nrow(ref_items))]$line_space<-abs(ref_items[c(2:nrow(ref_items))]$y - 
    ref_items[c(1:(nrow(ref_items)-1))]$y)
  
  ref<-""
  is_new_page<-F
  line_space<-0
  references<-list()
  for (j in c(1:nrow(ref_items))){
    
    if (ref_items[j]$line_space>inline_dist){
      references[[length(references)+1]]<-data.table(ref=trimws(ref), 
                                                     line_space=line_space,
                                                     is_new_page=is_new_page)
      ref<-""
      if (is_new_page==T){
        is_new_page<-F
      }
    }
    ref<-paste(ref, ref_items[j]$text)
    if (j==nrow(ref_items)){
      references[[length(references)+1]]<-data.table(ref=trimws(ref), 
                                                     line_space=line_space,
                                                     is_new_page=is_new_page)
      is_new_page<-F
    }
    if (ref_items[j]$line_space>500){
      is_new_page<-T
      line_space<-ref_items[j]$line_space
    }
  }
  references<-rbindlist(references)
  #references[is_new_page==T]
  #View(ref_items)
  #Bibliography<-references
  #Bibliography<-Bibliography[!grepl("©", Bibliography)]
  #Bibliography<-Bibliography[!grepl(item$assessmentId, Bibliography)]
  #Bibliography[grepl("Accessed:", Bibliography)]
  #Bibliography<-Bibliography[!(grepl("IUCN\\.", Bibliography) & grepl("Accessed:", Bibliography))]
  references<-references[!grepl("©", ref)]
  references<-references[!grepl(item$assessmentId, ref)]
  
  #handle page break
  
  
  #references<-references[references!=""]
  references_year<-references$ref
  references_year<-gsub("in press", " 2023.", references_year, ignore.case=T)
  references_year<-gsub("Meester, J.A.J. 1972 .", 
                        "Meester, J.A.J. 1972.", references_year, ignore.case=T)
  references_year<-gsub("Geremia, C., Wallen, R. and White, P.J. 2014 .",
                        "Geremia, C., Wallen, R. and White, P.J. 2014.", references_year, ignore.case=T)
  
  references_year<-gsub("in prep", " 2023.", references_year, ignore.case=T)
  references_year<-gsub("2013-2017", " 2017.", references_year, ignore.case=T)
  references_year<-gsub("2005-07", " 2005.", references_year, ignore.case=T)
  references_year<-gsub("1994 (unpublished)\\.", " 1994.", references_year, ignore.case=T)
  
  references_year<-gsub("IUCN SSC. Review of the Regional Conservation Strategy for the Cheetah and African Wild Dog in Southern Africa. Gland, Switzerland.",
                        " 9999.", references_year, ignore.case=T)
  references_year<-gsub("Szabo, M.P.J., Labruna, M.B., Pereira, M.C. and Duarte, J.M.B. Ticks (Acari: Ixodidae) on wild marsh-deer (Blastocerus dichotomus) from southeast Brazil: infestations before and after habitat loss. Journal Medical Entomology, Laham 40(3): 268-274.",
                        "9999.", references_year, ignore.case=T)
  
  
  
  pattern <- " \\d{4}\\.| \\d{4}\\)| \\d{4}[a-zA-Z]\\.|^\\d{4}\\.| \\d{4} \\[In"
  references$yearstr<-str_extract(references_year, pattern)
  references$next_line_new_page<-F
  if (nrow(references)>1){
    references[c(1:(nrow(references)-1))]$next_line_new_page<- references[c(2:nrow(references))]$is_new_page
    index<-references[is.na(yearstr) & (is_new_page==T), which=T]
    if (length(index)>0){
      references[index-1]$ref<-paste(references[index-1]$ref, references[index]$ref)
      references<-references[-index]
    }
    
    index<-references[is.na(yearstr) & (next_line_new_page==T), which=T]
    if (length(index)>0){
      references[index+1]$ref<-paste(references[index]$ref, references[index+1]$ref)
      references<-references[-index]
    }
    
  }
  yearstr<-references$yearstr
  yearstr<-gsub("[a-zA-Z]\\.", "", yearstr)
  yearstr<-gsub("\\)", "", yearstr)
  yearstr<-gsub("\\[In", "", yearstr)
  yearstr<-gsub("\\.", "", yearstr)
  
  yearstr<-trimws(yearstr)
  yearstr<-as.numeric(yearstr)
  if (length(yearstr[is.na(yearstr)]!=0)){
    #xxxx
  }
  if (nrow(references)!=length(yearstr)){
    #asdf
    #View(data.table(references$ref, yearstr))
  }
  
  references$year<-yearstr
  start_index<-str_locate(references$ref, fixed(references$yearstr))[,2]
  ref_temp<-trimws(str_sub(references$ref, start_index+1))
  end_index<-str_locate(ref_temp, "\\.")[,1]
  ref_temp<-trimws(str_sub(ref_temp, 1, end_index-1))
  references$reference_title<-ref_temp
  references[is.na(reference_title)]
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
                         authority=sp_list[i]$authority,
                         reference=references$ref,
                         reference_yearstr=references$yearstr,
                         reference_year=references$year,
                         reference_title=ref_temp)
  #references_dt$is_IUCN<-(grepl("IUCN\\.", references) & grepl("Accessed:", references))
  references_dt$is_IUCN<-grepl("IUCN\\.", references$ref)
  references_list[[length(references_list)+1]]<-references_dt
}
references_df<-rbindlist(references_list)
#fwrite(references_df, sprintf("../Data_IUCN_References/IUCN_PDF/Result/references_%s.csv", group))
saveRDS(references_df, sprintf("../Data_IUCN_References/IUCN_PDF/Result/references_%s.rda", group))
if (F){
  df<-readRDS(sprintf("../Data_IUCN_References/IUCN_PDF/Result/references_%s.rda", groups[3]))
  #View(df[assessmentId=="177177761"])
  df$year_score<-10
  yearstr<-df[is.na(reference_year)]$reference
  index<-str_locate(fixed(yearstr), "Access")[,2]
  substr<-str_sub(fixed(yearstr), index, str_length(yearstr))
  df[is.na(reference_year)]$reference_yearstr<-substr
  years<-str_extract(substr, "\\.\\d{4}| \\d{4}|\\-\\d{4}")
  years<-gsub("\\.", "", years)
  years<-gsub("\\-", "", years)
  years<-as.numeric(trimws(years))
  df[is.na(reference_year)]$reference_year<-years

  df[is.na(reference_year)]$year_score<-9
  yearstr<-df[is.na(reference_year)]$reference
  years<-str_extract(yearstr, " \\d{4} \\.")
  df[is.na(reference_year)]$reference_yearstr<-years
  years<-gsub("\\.", "", years)
  years<-as.numeric(trimws(years))
  df[is.na(reference_year)]$reference_year<-years
  
  df[is.na(reference_year)]$year_score<-8
  yearstr<-df[is.na(reference_year)]$reference
  years<-str_extract(yearstr, "\\(\\d{4}\\)")
  df[is.na(reference_year)]$reference_yearstr<-years
  years<-gsub("\\(", "", years)
  years<-gsub("\\)", "", years)
  years<-as.numeric(trimws(years))
  df[is.na(reference_year)]$reference_year<-years
  
  df[is.na(reference_year)]$year_score<-7
  yearstr<-df[is.na(reference_year)]$reference
  years<-str_extract(yearstr, "\\[\\d{4}\\]")
  df[is.na(reference_year)]$reference_yearstr<-years
  years<-gsub("\\[", "", years)
  years<-gsub("\\]", "", years)
  years<-as.numeric(trimws(years))
  df[is.na(reference_year)]$reference_year<-years
  
  df[is.na(reference_year)]$year_score<-6
  yearstr<-df[is.na(reference_year)]$reference
  years<-str_extract(yearstr, "\\d{4}\\)")
  df[is.na(reference_year)]$reference_yearstr<-years
  years<-gsub("\\)", "", years)
  years<-as.numeric(trimws(years))
  df[is.na(reference_year)]$reference_year<-years
  
  
  df[is.na(reference_year)]$year_score<-5
  yearstr<-df[is.na(reference_year)]$reference
  years<-str_extract(yearstr, " \\d{4}")
  df[is.na(reference_year)]$reference_yearstr<-years
  years<-as.numeric(trimws(years))
  df[is.na(reference_year)]$reference_year<-years
  
  df[is.na(reference_year)]$year_score<-4
  yearstr<-df[is.na(reference_year)]$reference
  years<-str_extract(yearstr, "\\d{4}")
  df[is.na(reference_year)]$reference_yearstr<-years
  years<-as.numeric(trimws(years))
  df[is.na(reference_year)]$reference_year<-years
  
  
  df[is.na(reference_year)]$year_score<-3
  
  exceptions<-c("Available (Accessed: at: 06", "Bibliography",
                "Citation", "35–43.", "Peru.", "(Accessed: 11 December 2023).",
                "(Accessed: 10 December 2020).",
                "(Accessed: 21 March 2019).",
                "(Accessed: 21 July 2022).", "Contract No. 8023.", 
                "07 January 2022).", "October 2021).",
                "7089-7110.", "1164.", "249.", "140.",
                "34-41.", "February 2021).", "January 2022, 2021).",
                "(Accessed: 28 April 2020).", "(Accessed: 30 January 2020).",
                "(Accessed: 08 December 2022).", "(Accessed: 10 December 2019).",
                "(Accessed: 15 November 2018).", "(Accessed: 07 December 2016).",
                "(Accessed: 09 December 2021).")
  df<-df[!(reference %in% exceptions)]
  df[!between(reference_year, 1700, 2023)]$reference_year<-NA
  df[is.na(reference_year)]$year_score<-0
  df[is.na(reference_year)]$reference
  df$leng<-str_length(df$reference)
  setorderv(df, c("leng","reference"), c(1, 1))
  View(df)
  View(df[is.na(reference_year)])
  View(df[!between(reference_year, 1700, 2023)])
}
