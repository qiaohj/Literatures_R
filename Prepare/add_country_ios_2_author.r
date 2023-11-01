library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
setDTthreads(20)
if (F){
  countries<-fread("../Data/GDPS.csv")
  countries<-countries[, c("country1", "iso3")]
  countries<-unique(countries)
  fwrite(countries, "../Data/country_iso.csv")
}
string<-authors[sample(nrow(authors), 100)]$affiliation
getCountry<-function(string){
  string_item<-string[grep(countries$country1, string)]
}
countries<-fread("../Data/country_iso.csv")
countries$country1<-toupper(countries$country1)
countries_full<-rbind(data.table(id=countries$country1, country=countries$country1, iso3=countries$iso3),
                      data.table(id=countries$iso3, country=countries$country1, iso3=countries$iso3))
countries_full<-countries_full[!id %in% c("AND")]
us_states<-fread("../Data/states_us.csv", fill=T)
us_states$states<-toupper(us_states$states)
categories<-list.dirs("../Data/CrossRef_By_Category", full.names = F)
categories<-categories[sample(length(categories), length(categories))]
category<-"WeedScience"
for (category in categories){
  print(category)
  target<-sprintf("../Data/CrossRef_By_Category/%s/authors_country_iso.rda", category)
  if (file.exists(target)){
    next
  }
  saveRDS(NULL, target)
  authors<-readRDS(sprintf("../Data/CrossRef_By_Category/%s/authors.rda", category))
  #setindexv(authors, "article_DOI")
  authors<-authors[affiliation!=""]
  authors$affiliation<-toupper(authors$affiliation)
  authors_affiliations <- authors[,list(item = toupper(trimws(unlist(strsplit(gsub("\\.", "", affiliation), ";|\\,| "))))), 
                         by=list(article_DOI, ORCID, authenticated_orcid, Given, Family,
                                 sequence, Sort, affiliation, affiliation_sort) ]
  authors_affiliations<-authors_affiliations[item!=""]
  authors_affiliations_countries<-merge(authors_affiliations, countries_full, by.x="item", by.y="id")
  authors_more<-merge(authors, authors_affiliations_countries,
                      by=c("article_DOI", "ORCID", "authenticated_orcid", "Given", "Family",
                              "sequence", "Sort", "affiliation", "affiliation_sort"),
                      all.x=T)
  countries2<-countries[country1!=""]
  authors_more_1<-authors_more[!is.na(iso3)]
  authors_more_1$country_type<-1
  authors_more_2<-authors_more[is.na(iso3)]
  authors_more_2$country_type<-0
  for (i in c(1:nrow(countries2))){
    
    position<-grepl(countries2[i]$country1, authors_more_2$affiliation)
    N<-length(position[position==T])
    print(paste(i, nrow(countries2), category, countries2[i]$country1, N, "record(s) found"))
    if (N>0){
      authors_more_2[position]$iso3<-countries2[i]$iso3
      authors_more_2[position]$country<-countries2[i]$country1
      authors_more_2[position]$country_type<-2
    }
  }
  authors_more_3<-authors_more_2[country_type==0]
  authors_more_2<-authors_more_2[country_type==2]
  i=1
  for (i in c(1:nrow(us_states))){
    
    position<-grepl(us_states[i]$state, authors_more_3$affiliation)
    N<-length(position[position==T])
    print(paste(i, nrow(us_states), category, us_states[i]$state, N, "record(s) found"))
    if (N>0){
      authors_more_3[position]$iso3<-"USA"
      authors_more_3[position]$country<-"UNITED STATES"
      authors_more_3[position]$country_type<-3
    }
  }
  authors_more_4<-authors_more_3[country_type==0]
  authors_more_3<-authors_more_3[country_type==3]
  country_alias<-fread("../Data/country_alias.csv")
  for (i in c(1:nrow(country_alias))){
    
    position<-grepl(country_alias[i]$text, authors_more_4$affiliation)
    N<-length(position[position==T])
    print(paste(i, nrow(country_alias), category, country_alias[i]$text, N, "record(s) found"))
    if (N>0){
      authors_more_4[position]$iso3<-country_alias[i]$iso3
      authors_more_4[position]$country<-country_alias[i]$country
      authors_more_4[position]$country_type<-4
      head(authors_more_4[position], 100)
    }
  }
  dim(authors_more_4[country_type==0])
  xx<-authors_more_4[country_type==0, .(N=.N), by=affiliation]
  setorderv(xx, "N", -1)
  head(xx, 20)
  if (F){
    xx[grepl("Bri", affiliation)]
    unique(authors_more[is.na(iso3)]$affiliation)
    fwrite(data.frame(text=unique(authors_more_3$affiliation), country="", iso3=""), "../Data/test.csv")
  }
  authors_country<-rbindlist(list(authors_more_1, authors_more_2, authors_more_3, authors_more_4))
  table(authors_country$country_type)
  saveRDS(authors_country, target)
}