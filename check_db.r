setwd("~/git/PubMed/PubMed_R")
library(dplyr)
library(ggplot2)
Article_Author_Full<-readRDS("../Objects/Article_Author_Full.rda")
head(Article_Author_Full)

Article_Author_Full_Filter<-Article_Author_Full %>% filter(between(Article_Author_Full$year, 1997, 2018))

unique_Artile<-distinct(Article_Author_Full_Filter, PMID, year)
N_Artile_Per_Year<-unique_Artile%>%count(year)
ggplot(N_Artile_Per_Year)+geom_point(aes(x=year, y=n))


IF_all<-read.csv("../Tables/IF_1997_2018.csv", head=T, sep=",", stringsAsFactors = F)
IF_all<-IF_all[,c("JCR.Abbreviated.Title", "Journal.Impact.Factor", "ISSN", "year", "Category")]
colnames(IF_all)<-c("ABBR", "IF", "Journal_ISSN", "year", "Category")
IF_all<-IF_all[which(!is.na(IF_all$IF)),]
unique_IF_all<-distinct(IF_all, Journal_ISSN, year)
N_unique_IF_all_Per_Year<-unique_IF_all%>%count(year)
ggplot(N_unique_IF_all_Per_Year)+geom_point(aes(x=year, y=n))

IF_all[which(IF_all$year==2010), ]
#drops <- c("AffiliationInfo")
#Article_Author_Full_Filter<-Article_Author_Full_Filter[ , !(names(Article_Author_Full_Filter) %in% drops)]
#Article_Author_Full_With_IF<-Article_Author_Full_With_IF[ , !(names(Article_Author_Full_With_IF) %in% drops)]
#Article_Author_Full_add_IF<-Article_Author_Full_add_IF[ , !(names(Article_Author_Full_add_IF) %in% drops)]

Article_Author_Full_With_IF<-inner_join(Article_Author_Full_Filter, IF_all, by=c("Journal_ISSN", "year"))
Article_Author_Full_add_IF<-left_join(Article_Author_Full_Filter, IF_all, by=c("Journal_ISSN", "year"))

Article_Author_Full_With_IF%>% filter(PMID==12108317)
Article_Author_Full%>% filter(PMID==12108317)
Article_Author_Full%>% filter(year==2010)

N_Journal_With_IF_in_Pubmed<-NULL
for (i in c(1997:2018)){
  issn_Artile<-distinct((Article_Author_Full%>% filter(year==i)), Journal_ISSN)
  issn_IF<-distinct((IF_all%>% filter(year==i)), Journal_ISSN)
  
  in_issn<-issn_Artile%>%filter(Journal_ISSN %in% issn_IF$Journal_ISSN)
  out_issn<-issn_Artile%>%filter(!(Journal_ISSN %in% issn_IF$Journal_ISSN))
  print(paste(i, nrow(issn_Artile), nrow(issn_IF), nrow(in_issn), nrow(out_issn)))
  item<-data.frame(year=i, issn_PubMed=nrow(issn_Artile), issn_IF=nrow(issn_IF),
                   withIF=nrow(in_issn), withoutIF=nrow(out_issn))
  if (is.null(N_Journal_With_IF_in_Pubmed)){
    N_Journal_With_IF_in_Pubmed<-item
  }else{
    N_Journal_With_IF_in_Pubmed<-bind_rows(N_Journal_With_IF_in_Pubmed, item)
  }
}

ggplot(N_Journal_With_IF_in_Pubmed) + geom_line(aes(x=year, y=withIF), color="red") + geom_line(aes(x=year, y=withoutIF), color="blue")

unique_Artile_Full_With_IF<-distinct(Article_Author_Full_With_IF, PMID, year)
unique_Artile_Full_add_IF<-distinct(Article_Author_Full_add_IF, PMID, year)
unique_Artile<-distinct(Article_Author_Full_Filter, PMID, year)

N_Artile_Per_Year<-unique_Artile%>%count(year)
ggplot(N_Artile_Per_Year)+geom_point(aes(x=year, y=n))

Journals_in_2005<-Article_Author_Full_add_IF %>% filter(year==2005)
Journals_in_2010<-Article_Author_Full_add_IF %>% filter(year==2008)
Journals_in_2005<-distinct(Journals_in_2005, Journal_ISSN)
Journals_in_2010<-distinct(Journals_in_2010, Journal_ISSN)
removed<-Journals_in_2005 %>% filter(!(Journals_in_2005$Journal_ISSN %in% Journals_in_2010$Journal_ISSN))

N_Artile_With_IF_Per_Year<-unique_Artile_Full_With_IF%>%count(year)
N_Artile_add_IF_Per_Year<-unique_Artile_Full_add_IF%>%count(year)

ggplot(N_Artile_With_IF_Per_Year)+geom_point(aes(x=year, y=n))
ggplot(N_Artile_add_IF_Per_Year)+geom_point(aes(x=year, y=n))


unique_Journal_With_IF<-distinct(Article_Author_Full_With_IF, Journal_ISSN, year)
N_Journal_With_IF_Per_Year<-unique_Journal_With_IF%>%count(year)
ggplot(N_Journal_With_IF_Per_Year)+geom_point(aes(x=year, y=n))


N_Author_With_Gender<-NULL
for (i in c(1997:2018)){
  print(i)
  item<-Article_Author_Full_add_IF%>% filter(year==i)
  N_all<-nrow(item)
  N_with_Gender<-nrow(item %>% filter(!is.na(gender)))
  
  item<-data.frame(year=i, N_all=N_all, N_with_Gender=N_with_Gender)
  if (is.null(N_Author_With_Gender)){
    N_Author_With_Gender<-item
  }else{
    N_Author_With_Gender<-bind_rows(N_Author_With_Gender, item)
  }
}
N_Author_With_Gender$N_without_Gender<-N_Author_With_Gender$N_all-N_Author_With_Gender$N_with_Gender
N_Author_With_Gender$ratio<-N_Author_With_Gender$N_with_Gender/N_Author_With_Gender$N_all
ggplot(N_Author_With_Gender) + geom_line(aes(x=year, y=N_with_Gender), color="red") +  
  geom_line(aes(x=year, y=N_without_Gender), color="blue")

ggplot(N_Author_With_Gender) + geom_line(aes(x=year, y=ratio), color="red")

N_Author_With_Gender_and_IF<-NULL
for (i in c(1997:2018)){
  print(i)
  item<-Article_Author_Full_With_IF%>% filter(year==i)
  N_all<-nrow(item)
  N_with_Gender<-nrow(item %>% filter(!is.na(gender)))
  
  item<-data.frame(year=i, N_all=N_all, N_with_Gender=N_with_Gender)
  if (is.null(N_Author_With_Gender_and_IF)){
    N_Author_With_Gender_and_IF<-item
  }else{
    N_Author_With_Gender_and_IF<-bind_rows(N_Author_With_Gender_and_IF, item)
  }
}
N_Author_With_Gender_and_IF$N_without_Gender<-N_Author_With_Gender_and_IF$N_all-N_Author_With_Gender_and_IF$N_with_Gender
N_Author_With_Gender_and_IF$ratio<-N_Author_With_Gender_and_IF$N_with_Gender/N_Author_With_Gender_and_IF$N_all
ggplot(N_Author_With_Gender_and_IF) + geom_line(aes(x=year, y=N_with_Gender), color="red") +  
  geom_line(aes(x=year, y=N_without_Gender), color="blue")

ggplot(N_Author_With_Gender_and_IF) + geom_line(aes(x=year, y=ratio), color="red")


Article_Author_Full_With_IF_2004_2018<-Article_Author_Full_With_IF %>% filter(between(Article_Author_Full_With_IF$year, 2004, 2018))

#unique_Gender_With_IF<-distinct(Article_Author_Full_With_IF_2004_2018, PMID, AuthorID, Sort, 
#                                Journal_ISSN, IF, proportion_male, proportion_female, year)

gender_sum<-Article_Author_Full_With_IF_2004_2018%>% group_by(year)%>% summarise(male=sum(proportion_male, na.rm = T),
                                                                       female=sum(proportion_female, na.rm = T))

gender_sum$female_ratio<-gender_sum$female/(gender_sum$male+gender_sum$female)
ggplot(gender_sum, aes(x=year, y=female_ratio))+geom_point()

Article_Author_Full_With_IF_Last <- Article_Author_Full_With_IF_2004_2018 %>% 
  group_by(PMID) %>%
  filter(Sort == max(Sort))

Article_Author_Full_With_IF_First <- Article_Author_Full_With_IF_2004_2018 %>% 
  filter(Sort == 1)


gender_sum_first<-Article_Author_Full_With_IF_First%>% group_by(year)%>% summarise(male=sum(proportion_male, na.rm = T),
                                                                 female=sum(proportion_female, na.rm = T))

gender_sum_first$female_ratio<-gender_sum_first$female/(gender_sum_first$male+gender_sum_first$female)

ggplot(gender_sum_first, aes(x=year, y=female_ratio))+geom_point()


gender_sum_last<-Article_Author_Full_With_IF_Last%>% group_by(year)%>% summarise(male=sum(proportion_male, na.rm = T),
                                                                             female=sum(proportion_female, na.rm = T))

gender_sum_last$female_ratio<-gender_sum_last$female/(gender_sum_last$male+gender_sum_last$female)

ggplot(gender_sum_last, aes(x=year, y=female_ratio))+geom_point()

gender_sum$type="ALL"
gender_sum_last$type="Last"
gender_sum_first$type="First"

gender_all<-bind_rows(gender_sum, bind_rows(gender_sum_first, gender_sum_last))
ggplot(gender_all, aes(x=year, y=female_ratio, color=factor(type)))+geom_point()+geom_line()


unique_Gender_With_IF_Category<-distinct(Article_Author_Full_With_IF_2004_2018, PMID, AuthorID, Sort, 
                                Journal_ISSN, IF, proportion_male, proportion_female, Category, year)
unique_Artile_Category<-distinct(unique_Gender_With_IF_Category %>% filter(!is.na(IF)), Journal_ISSN, Category, year, PMID, IF)

Journal_Category<-unique_Artile_Category %>% group_by(Category, year) %>% summarise(mean_IF=mean(IF),
                                                                                    max_IF=max(IF),
                                                                                    min_IF=min(IF))

Artile_Category<-unique_Artile_Category %>% count(Category, year)

for (cate in unique(Artile_Category$Category)){
  print(cate)
  item<-Artile_Category %>% filter(Category==cate)
  p<-ggplot(item, aes(x=year, y=n))+geom_point()+geom_line()
  ggsave(p, file=sprintf("../Figures/N_Artile_Year/%s.png", cate))
}

Artile_Category_Sum<-Artile_Category %>% group_by(Category) %>% summarise(sum_Artile=sum(n))
top_10<-Artile_Category_Sum[order(Artile_Category_Sum$sum_Artile, decreasing=T), "Category"][c(1, 3:11), 1]
top_10<-top_10$Category
target<-c("ENVIRONMENTAL SCIENCES", "BIOLOGY", "PLANT SCIENCES", "MICROBIOLOGY", "ZOOLOGY", "ECOLOGY", "EVOLUTIONARY BIOLOGY", "MATHEMATICAL & COMPUTATIONAL BIOLOGY")

Artile_Category_target<-Artile_Category%>% filter(Artile_Category$Category %in% c(top_10, target))
ggplot(Artile_Category_target, aes(x=year, y=n, color=factor(Category)))+geom_point()+geom_line()

gender_sum_Category<-unique_Gender_With_IF_Category%>% group_by(year, Category)%>% summarise(male=sum(proportion_male, na.rm = T),
                                                                 female=sum(proportion_female, na.rm = T))

gender_sum_Category$female_ratio<-gender_sum_Category$female/(gender_sum_Category$male+gender_sum_Category$female)
gender_sum_Category<-gender_sum_Category%>%filter(year>2000)
cate<-unique(gender_sum_Category$Category)[1]
for (cate in unique(gender_sum_Category$Category)){
  print(cate)
  item<-gender_sum_Category %>% filter(Category==cate)
  p<-ggplot(item, aes(x=year, y=female_ratio))+geom_point()
  ggsave(p, file=sprintf("../Figures/Female_Ratio_Category/%s.png", cate))
}
gender_sum_Category_target<-gender_sum_Category %>% filter(Category %in% c(top_10, target))

ggplot(gender_sum_Category_target, aes(x=year, y=female_ratio, color=factor(Category)))+geom_point()+geom_line()



library("Hmisc")
cuts<-seq(from=0, to=10, by=2.5)
Article_Author_Full_With_IF_2004_2018$IF_CUT<-cut2(Article_Author_Full_With_IF_2004_2018$IF, cuts=cuts)

test<-Article_Author_Full_With_IF_2004_2018 %>% filter(year==2016)
test<-test %>% filter(between(IF, 5, 7.5))

test2<-unique_Gender_With_IF_Cuts %>% filter(year==2016)
test2<-test2 %>% filter(between(IF, 5, 7.5))
test2 %>% filter(PMID==29875998)
gender_sum_Cuts<-Article_Author_Full_With_IF_2004_2018%>% group_by(year, IF_CUT)%>% 
  summarise(male=sum(proportion_male, na.rm = T),
            female=sum(proportion_female, na.rm = T))

gender_sum_Cuts$female_ratio<-gender_sum_Cuts$female/(gender_sum_Cuts$male+gender_sum_Cuts$female)
ggplot(gender_sum_Cuts, aes(x=year, y=female_ratio, color=IF_CUT))+geom_point()+geom_line()


gender_sum_Cuts_first<-Article_Author_Full_With_IF_First%>% group_by(year, IF_CUT)%>% 
  summarise(male=sum(proportion_male, na.rm = T),
            female=sum(proportion_female, na.rm = T))

gender_sum_Cuts_first$female_ratio<-gender_sum_Cuts_first$female/(gender_sum_Cuts_first$male+gender_sum_Cuts_first$female)
ggplot(gender_sum_Cuts_first, aes(x=year, y=female_ratio, color=IF_CUT))+geom_point()+geom_line()
gender_sum_Cuts_first<-Article_Author_Full_With_IF_First%>% group_by(year, IF_CUT)%>% 
  summarise(male=sum(proportion_male, na.rm = T),
            female=sum(proportion_female, na.rm = T))

gender_sum_Cuts_first$female_ratio<-gender_sum_Cuts_first$female/(gender_sum_Cuts_first$male+gender_sum_Cuts_first$female)
ggplot(gender_sum_Cuts_first, aes(x=year, y=female_ratio, color=IF_CUT))+geom_point()+geom_line()





gender_sum_Cuts_Last<-Article_Author_Full_With_IF_Last%>% group_by(year, IF_CUT)%>% 
  summarise(male=sum(proportion_male, na.rm = T),
            female=sum(proportion_female, na.rm = T))

gender_sum_Cuts_Last$female_ratio<-gender_sum_Cuts_Last$female/(gender_sum_Cuts_Last$male+gender_sum_Cuts_Last$female)
ggplot(gender_sum_Cuts_Last, aes(x=year, y=female_ratio, color=IF_CUT))+geom_point()+geom_line()
gender_sum_Cuts_Last<-Article_Author_Full_With_IF_Last%>% group_by(year, IF_CUT)%>% 
  summarise(male=sum(proportion_male, na.rm = T),
            female=sum(proportion_female, na.rm = T))

gender_sum_Cuts_Last$female_ratio<-gender_sum_Cuts_Last$female/(gender_sum_Cuts_Last$male+gender_sum_Cuts_Last$female)
ggplot(gender_sum_Cuts_Last, aes(x=year, y=female_ratio, color=IF_CUT))+geom_point()+geom_line()


gender_sum_Cuts<-Article_Author_Full_With_IF_2004_2018 %>% filter(Category %in% c(top_10, target)) %>% group_by(year, IF_CUT)%>% 
  summarise(male=sum(proportion_male, na.rm = T),
            female=sum(proportion_female, na.rm = T))

gender_sum_Cuts$female_ratio<-gender_sum_Cuts$female/(gender_sum_Cuts$male+gender_sum_Cuts$female)
ggplot(gender_sum_Cuts, aes(x=year, y=female_ratio, color=IF_CUT))+geom_point()+geom_line()

