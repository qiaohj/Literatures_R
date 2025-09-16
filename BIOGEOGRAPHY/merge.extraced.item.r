library(data.table)
library(stringr)
library(stringi)
library(zoo)
library(readr)
library(pdftools)
library(jsonlite)
library(ggplot2)

setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
source("Download.PDF/getArticles.r")
crossref.year<-2025
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
journals<-readRDS("../Data/JCR/Target.Journals.rda")



target.journals<-data.table(Title=c("ECOGRAPHY", "DIVERSITY AND DISTRIBUTIONS", 
                                    "GLOBAL ECOLOGY AND BIOGEOGRAPHY", "JOURNAL OF BIOGEOGRAPHY"))
i=1
authors.list<-list()
geographical_scope.list<-list()
data_sources.list<-list()
keywords.list<-list()
articles.list<-list()
for (i in c(1:nrow(target.journals))){
  item<-target.journals[i]
  folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/BIOGEOGRAPHY/Extracted.Items/%s", item$Title)
  conf.item<-journals[journal==item$Title]
  jsons<-list.files(folder, pattern="\\.json.rda", full.name=T)
  
  articles<-getArticles(conf.item, all_journal_folders)
  articles<-articles[, c("issue", "volume", "page", "title", "published", "doi.suffix")]
  articles$doi<-toupper(articles$doi.suffix)
  articles$journal<-item$Title
  articles.list[[length(articles.list)+1]]<-articles
  for (j in c(1:length(jsons))){
    json.f<-jsons[j]
    json<-readRDS(json.f)
    doi<-basename(json.f)
    doi<-gsub("\\.json\\.rda", "", doi)
    if (length(json$authors)>0){
      authors<-json$authors
      authors$doi<-doi
      authors$journal<-item$Title
      authors.list[[length(authors.list)+1]]<-authors
    }
    if (length(json$geographical_scope)>0){
      geographical_scope<-json$geographical_scope
      geographical_scope$doi<-doi
      geographical_scope$journal<-item$Title
      geographical_scope.list[[length(geographical_scope.list)+1]]<-geographical_scope
    }
    if (length(json$data_sources)>0){
      data_sources<-json$data_sources
      data_sources$doi<-doi
      data_sources$journal<-item$Title
      data_sources.list[[length(data_sources.list)+1]]<-data_sources
    }
    if (length(json$keywords)>0){
      keywords<-data.table(keyword=json$keywords, doi=doi, journal=item$Title)
      keywords.list[[length(keywords.list)+1]]<-keywords
    }
  }
}
library(zoo)
#https://data.worldbank.org/about/country-and-lending-groups
gdp<-fread("../Data/BIOGEOGRAPHY/gdp.type.csv", head=T)
year_cols <- paste0("year_", 1987:2024)
colnames(gdp)[3:40]<-year_cols
cols_to_process <- grep("^year_", names(gdp), value = TRUE)
gdp[, (cols_to_process) := lapply(.SD, function(x) ifelse(x == "..", NA_character_, x)), .SDcols = cols_to_process]
gdp_long_temp <- melt(gdp,
                     id.vars = c("country_iso3", "country_name"),
                     measure.vars = cols_to_process,
                     variable.name = "year",
                     value.name = "type")
gdp_long_temp[, year := as.integer(sub("year_", "", year))]
setorder(gdp_long_temp, `country_iso3`, year)
gdp_final <- copy(gdp_long_temp)

gdp_final[, type := {
  filled_type <- na.locf(type, na.rm = FALSE)
  na.locf(filled_type, fromLast = TRUE, na.rm = FALSE)
}, by = c("country_iso3", "country_name")]


if (F){
  gdp[, latest_gdp := apply(.SD, 1, function(x) {
    for (i in length(x):1) {
      if (!is.na(x[i])) {
        return(x[i])
      }
    }
    return(NA)
  }), .SDcols = year_cols]
  target_years_cols <- paste0("year_", 2010:2024)
  gdp[, avg_2010_2024 := apply(.SD, 1, mean, na.rm = TRUE), .SDcols = target_years_cols]
  gdp<-gdp[, c("Country Name", "Country Code", "latest_gdp", "avg_2010_2024")]
  colnames(gdp)<-c("country_name", "country_iso3", "latest_gdp", "avg_2010_2024")
}
articles.df<-rbindlist(articles.list)
articles.df$year <- as.numeric(format(as.Date(articles.df$published, format = "%Y-%m-%d"), "%Y"))

N.articles<-articles.df[year>=2010, .(N.article=.N), by=c("journal")]

authors.df<-rbindlist(authors.list)

N<-merge(N.articles, N.authors.article, by="journal")
N$per<-N$N.article.author/N$N.article

articles.df[!doi %in% authors.df$doi]

authors.df.full<-merge(authors.df, articles.df, by=c("doi", "journal"))
authors.df.full$year <- as.numeric(format(as.Date(authors.df.full$published, format = "%Y-%m-%d"), "%Y"))
authors.df.full[!country_iso3 %in% gdp$`Country Code`]
authors.df.full<-merge(authors.df.full, gdp_final, by=c("country_iso3", "year"))
authors.df.full<-authors.df.full[type!=""]
N.authors.article<-articles.df[year>=2010, .(N=length(unique(doi))), by=list(journal)]


N.authors.article<-authors.df.full[year>=2010, .(N=length(unique(doi))), by=list(journal, year)]

ggplot(articles.N[year>=2010])+geom_line(aes(x=year, y=N, color=journal))+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)

all_gdp_types <- unique(authors.df.full$type)
gdp_type_numeric_mapping <- c(
  "L" = 0,
  "LM" = 1,
  "UM" = 2,
  "H" = 3
)

dt_ebdi_results <- authors.df.full[, {
  N_Total <- .N
  ebdi_score <- NA_real_
  mean_gdp_score <- NA_real_
  if (N_Total > 0) {
    numeric_gdp_types <- gdp_type_numeric_mapping[type]
    mean_gdp_score <- mean(numeric_gdp_types, na.rm = TRUE)
    if (N_Total == 1) {
      ebdi_score <- 0
    } else {
      gdp_counts <- table(type)
      full_gdp_counts <- setNames(rep(0, length(all_gdp_types)), all_gdp_types)
      full_gdp_counts[names(gdp_counts)] <- gdp_counts
      proportions <- full_gdp_counts / N_Total
      sum_sq_proportions <- sum(proportions^2)
      ebdi_score <- 1 - sum_sq_proportions
    }
  }
  
  list(journal = unique(journal),
       year = unique(year),
       ebdi_score = ebdi_score,
       mean_gdp_score = mean_gdp_score,
       N_Total=N_Total)
}, by = "doi"]

dt_ebdi_results_co_author <- authors.df.full[is_corresponding_author==T, {
  N_Total <- .N
  ebdi_score <- NA_real_
  mean_gdp_score <- NA_real_
  if (N_Total > 0) {
    numeric_gdp_types <- gdp_type_numeric_mapping[type]
    mean_gdp_score <- mean(numeric_gdp_types, na.rm = TRUE)
    if (N_Total == 1) {
      ebdi_score <- 0
    } else {
      gdp_counts <- table(type)
      full_gdp_counts <- setNames(rep(0, length(all_gdp_types)), all_gdp_types)
      full_gdp_counts[names(gdp_counts)] <- gdp_counts
      proportions <- full_gdp_counts / N_Total
      sum_sq_proportions <- sum(proportions^2)
      ebdi_score <- 1 - sum_sq_proportions
    }
  }
  
  list(journal = unique(journal),
       year = unique(year),
       ebdi_score = ebdi_score,
       mean_gdp_score = mean_gdp_score,
       N_Total=N_Total)
}, by = "doi"]

ggplot(dt_ebdi_results[ebdi_score>0 & year>2010], 
       aes(x=year, y=ebdi_score, size=mean_gdp_score, color=journal))+
  #geom_point()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)+
  geom_smooth()

ggplot(dt_ebdi_results[ebdi_score>0 & year>2010], 
       aes(x=year, y=mean_gdp_score, color=journal))+
  #geom_point()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)+
  geom_smooth()

ggplot(dt_ebdi_results_co_author[ebdi_score>0 & year>2010], 
       aes(x=year, y=ebdi_score, size=mean_gdp_score, color=journal))+
  #geom_point()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)+
  geom_smooth()

ggplot(dt_ebdi_results_co_author[year>2010], 
       aes(x=year, y=mean_gdp_score, color=journal))+
  geom_point()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)+
  geom_smooth()


table(authors.df.full$type)
geographical_scope.df<-rbindlist(geographical_scope.list)
geographical_scope.df.full<-merge(geographical_scope.df, articles.df, by="doi")
geographical_scope.df.full$year <- as.numeric(format(as.Date(geographical_scope.df.full$published, format = "%Y-%m-%d"), "%Y"))


data_sources.df<-rbindlist(data_sources.list)
data_sources.df.full<-merge(data_sources.df, articles.df, by="doi")
data_sources.df.full$year <- as.numeric(format(as.Date(data_sources.df.full$published, format = "%Y-%m-%d"), "%Y"))

keywords.df<-rbindlist(keywords.list)
keywords.df.full<-merge(keywords.df, articles.df, by="doi")
keywords.df.full$year <- as.numeric(format(as.Date(keywords.df.full$published, format = "%Y-%m-%d"), "%Y"))

saveRDS(authors.df.full, "../Data/BIOGEOGRAPHY/authors.rda")
saveRDS(geographical_scope.df.full, "../Data/BIOGEOGRAPHY/geographical_scope.rda")
saveRDS(data_sources.df.full, "../Data/BIOGEOGRAPHY/data_sources.rda")
saveRDS(keywords.df.full, "../Data/BIOGEOGRAPHY/keywords.rda")
xxx<-authors.df.full[year>=2010, c("country_iso3", "doi", "journal", "type", "year")]
xxx[journal=="DIVERSITY AND DISTRIBUTIONS", journal:="DDI"]
xxx[journal=="ECOGRAPHY", journal:="E"]
xxx[journal=="GLOBAL ECOLOGY AND BIOGEOGRAPHY", journal:="GEB"]
xxx[journal=="JOURNAL OF BIOGEOGRAPHY", journal:="JBI"]

fwrite(xxx[sample(nrow(xxx), 1e4)], "../Data/BIOGEOGRAPHY/authors.df.full.csv")
table(authors.df.full$journal)
