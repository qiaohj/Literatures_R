library(data.table)
library(tm)
library(readr)
library(stringi)
library(RWeka)
library(ggplot2)
library(wordcloud)
library(SnowballC)
library(gridExtra)
library(dplyr)
library(rvest)
library(splitstackshape)
rm(list=ls())
setwd("/media/huijieqiao/WD22T_11/literatures/Script")

if (F){
  x <- read_html("http://www.englishpage.com/irregularverbs/irregularverbs.html")
  dic<-html_table(x, header = TRUE)[[1]]
  dic<-dic %>% bind_rows %>%
    rename(Infinitive = `Base Form`, Past = `Simple Past`, PP = `Past Participle`) %>%
    filter(!Infinitive %in% LETTERS)
  
  dic<-cSplit(dic, splitCols = c("Past", "PP"),
              sep = " / ", direction = "long", type.convert = F) %>%
    filter(complete.cases(.))
  dic<-  mutate_each(dic, list(~gsub(pattern = "\\s\\(.*\\)$|\\s\\[\\?\\]",
                                     replacement = "",
                                     x = .)))
  saveRDS(dic, "../Data/word_wrapper/irregular.verbs.rda")
}
source("functions.r")
category<-"missing_journals_ecology_biodiversity"
#category<-"Ecology"
#category<-"Biodiversity Conservation"

article_df<-readArticle(category, all=T)
table(article_df$type)

if (F){
  article_df_abstract<-article_df[!is.na(abstract)]
  system.time({
    token_title_listx<-article_df[, get_Tokens(mydic, title, "title", doi), by = seq_len(nrow(article_df))]
    token_abstract_listx<-article_df_abstract[, get_Tokens(mydic, abstract, "abstract", doi), by = seq_len(nrow(article_df_abstract))]
  })
}

mydic<-readRDS("../Data/word_wrapper/irregular.verbs.rda")



token_title_list<-list()
token_abstract_list<-list()
for (i in c(1:nrow(article_df))){
  if ((i %% 1e3)==0){
    print(paste(i, nrow(article_df), article_df[i]$doi, article_df[i]$container_title)) 
  }
  title<-article_df[i]$title
  if (is.na(title)){
    next()
  }
  if (title==""){
    next()
  }
  tokens<-get_Tokens(mydic, title, "title", article_df[i]$doi)
  if (is.null(tokens)){
    next()
  }
  token_title_list[[length(token_title_list)+1]]<-tokens
  abstract<-article_df[i]$abstract
  if (is.na(abstract)){
    next()
  }
  if (abstract==""){
    next()
  }
  tokens<-get_Tokens(mydic, abstract, "abstract", article_df[i]$doi)
  
  token_abstract_list[[length(token_abstract_list)+1]]<-tokens
  if (F){
    wordcloud(unigram$Word,
              unigram$Frequency,
              max.words=100, min.freq = 1,
              random.order=FALSE,
              colors=brewer.pal(8,"Dark2"), font = 3)
    
    subBigram <- bigram[1:10,]
    plotBiGram <- ggplot(subBigram, aes(x= reorder(Word, Frequency),y= Frequency )) + 
      geom_bar(stat="identity", fill="red") + 
      geom_text(aes(y= Frequency, label=Frequency), vjust=1) +
      coord_flip() + labs(x="Word", y="Frequency", title="Bigrams frequency")
    subTrigram <- trigram[1:10,]
    plotTriGram <- ggplot(subTrigram, aes(x= reorder(Word, Frequency),y= Frequency )) +
      geom_bar(stat="identity", fill="green") +
      geom_text(aes(y= Frequency, label=Frequency), vjust=1) +
      coord_flip() + labs(x="Word", y="Frequency", title="Trigrams frequency")
    
    grid.arrange(plotBiGram, plotTriGram, ncol=2)
  }
}
token_title_df<-rbindlist(token_title_list)
token_abstract_df<-rbindlist(token_abstract_list)
saveRDS(token_title_df, sprintf("../Data/CrossRef_By_Category/%s/token_title.rda", category))
saveRDS(token_abstract_df, sprintf("../Data/CrossRef_By_Category/%s/token_abstract.rda", category))


if (F){
  get_Tokens(mydic, "<html>&amp;<tr> &lt;abcd&rt;hello&amp; &lt; &rt;</tr>", "title")
  x<-"<jats:p> The receptiveness of patients towards involving medical trainees in their care is essential to clinical education. Data on patients’ attitude towards trainees and reasons for their attitude is currently lacking. Hence the aim was to explore the attitudes and factors influencing the attitudes of patients towards trainees at a tertiary centre for cardiovascular care. A cross-sectional survey was performed among consecutive patients from the cardiac clinics at our tertiary institution in 2014. Of the 723 patients included, nearly all (97.9%) believe that senior doctors make the final decision for their care, and the majority (94.1%) are willing to interact with trainees under supervision of senior doctors. However, less than 60% of patients have actually allowed trainees to participate in their care most or all of the time, with the most important reason for this being fear that care would be compromised ( n = 172). Top reasons why trainees were allowed include belief that it is important for trainees to get experience ( n = 538), that trainees obtained permission politely ( n = 360) and that trainees were professional ( n = 284). Multivariate analysis revealed that better education (odds ratio (OR) 2.055, 95% confidence interval (CI) 1.393–3.033, p &lt; 0.01), male gender (OR 1.556, 95% CI 1.058–2.338, p = 0.03) and less worry about cost of treatment (OR 1.605, 95% CI 1.058–2.433, p = 0.03) increased receptiveness towards trainees. The study demonstrated largely positive attitudes towards trainees being involved in one’s care. The trainee’s politeness and professionalism, as well as the patient’s perceived importance of trainee education, were important in determining such receptiveness. </jats:p>"
  get_Tokens(mydic, x, "title")
  token_title_df[Word=="genet"]$Word<-"gene"
  dim(token_title_df)
  token_title_df<-unique(token_title_df)
  
  token_doi_year<-token_title_df[, .(N_DOI=length(unique(doi)),
                                     N_container=length(unique(container_title))), 
                                 by=list(Year)]
  token_doi_year[Year==2000]
  token_doi<-token_title_df[Year>=2000, .(N=.N,
                                          N_container=length(unique(container_title))), 
                            by=list(Year, doi)]
  token_doi
  token_doi_year_sampled<-token_doi[Year>=2000,.SD[sample(.N, min(5000,.N))],by = Year]
  
  
  
  token_title_se<-token_title_df[doi %in% token_doi_year_sampled$doi,
                                 .(Frequency=sum(Frequency), N=.N, N_DOI=length(unique(doi))), 
                                 by=list(Word, N_token, Year)]
  
  skipped_word<-c("use", "ecolog")
  token_title_se<-token_title_se[!(Word %in% skipped_word)]
  token_title_se$MEAN_N<-token_title_se$N/token_title_se$N_DOI
  setorderv(token_title_se, c("N_token", "Year", "N"), c(1, 1, -1))
  
  token_title_top10_1<-token_title_se[((N_token==1)&!is.na(Year)), head(.SD, 100), 
                                      by=list(N_token, Year)]
  tail(token_title_top10_1, 20)
  
  length(unique(token_title_top10_1[Year>=2000]$Word))
  target_Word_1<-unique(token_title_top10_1[Year>=2000]$Word)
  token_title_se_top10_1<-token_title_se[Word %in% target_Word_1]
  token_title_se_top10_1<-token_title_se_top10_1[Year>=2000]
  token<-target_Word_1[1]
  token_title_se_top10_1$Word<-as.character(token_title_se_top10_1$Word)
  token_title_se_top10_1$Year<-as.numeric(token_title_se_top10_1$Year)
  lm_token<-list()
  for (token in target_Word_1){
    item<-token_title_se_top10_1[Word==token]
    model<-lm(N~Year, data=item)
    p_value<-summary(model)$coefficients[2,4]  
    estimate<-summary(model)$coefficients[2,1]  
    r2<-summary(model)$r.squared
    item<-data.table(Word=token, p_value, estimate, r2)
    lm_token[[length(lm_token)+1]]<-item
  }
  lm_token<-rbindlist(lm_token)
  lm_token$p_label<-""
  lm_token[p_value<=0.05]$p_label<-"*"
  lm_token[p_value<=0.01]$p_label<-"**"
  lm_token[p_value<=0.001]$p_label<-"***"
  hist(lm_token[p_label!=""]$estimate)
  quantiles<-quantile(lm_token[p_label!=""]$estimate, c(0.25, 0.75))
  lm_token$trend<-"unknown"
  lm_token[(p_label!="")&estimate<=-0.5]$trend<-"decrease"
  lm_token[(p_label!="")&estimate>=0.5]$trend<-"increase"
  lm_token[(p_label!="")&between(estimate, -0.5, 0.5)]$trend<-"stable"
  table(lm_token$trend)
  token_title_se_top10_1_with_trend<-merge(token_title_se_top10_1, lm_token, by="Word")
  ggplot(token_title_se_top10_1_with_trend, 
         aes(x=Year, y=N, group=Word, color=Word))+geom_line()+
    geom_smooth(method="lm")+
    facet_wrap(~trend, scale="free_y")+
    theme(legend.position = "none")
  
  setorderv(lm_token, "estimate", 1)
  lm_token[grepl("geno", Word)]
  token_abstract_se<-token_abstract_df[, .(Frequency=sum(Frequency)), by=list(Word, N_token, Year)]
  token_title_all<-token_title_df[, .(Frequency=sum(Frequency)), by=list(Word, N_token)]
  token_title_all_1<-token_title_all[N_token==1]
  quantile(token_title_all_1$Frequency, c(.999))
  nrow(token_title_all_1[Frequency>1000])
  wordcloud(token_title_all_1[Frequency>1000]$Word,
            token_title_all_1[Frequency>1000]$Frequency,
            max.words=100, min.freq = 1,
            random.order=FALSE,
            colors=brewer.pal(8,"Dark2"), font = 3)
  
  if (F){
    old<-readRDS("/Users/huijieqiao/git/literatures/Data/CrossRef_By_Category/Biodiversity Conservation/token_title_old.rda")
    new<-readRDS("/Users/huijieqiao/git/literatures/Data/CrossRef_By_Category/Biodiversity Conservation/token_title.rda")
    dim(old)
    dim(new)
    old
  }
}