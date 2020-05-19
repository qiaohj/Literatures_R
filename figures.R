library(dplyr)
library(ggplot2)
setwd("/Volumes/Disk2/Experiments/PubMed/PubMed_R")
Article_Author_Full_With_IF<-readRDS("../Objects/Article_Author_Full_With_IF.rda")

Article_Author_Full_With_IF_Last <- Article_Author_Full_With_IF %>% 
  group_by(PMID) %>%
  filter(Sort == max(Sort))

Article_Author_Full_With_IF_First <- Article_Author_Full_With_IF %>% 
  filter(Sort == 1)

head(Article_Author_Full_With_IF_Last)
dim(Article_Author_Full_With_IF_First)

Article_Author_Full_With_IF_Last<- Article_Author_Full_With_IF_Last%>% filter(!is.na(proportion_male))
Article_Author_Full_With_IF_First<- Article_Author_Full_With_IF_First%>% filter(!is.na(proportion_male))

head(Article_Author_Full_With_IF_First$IF)
Article_Author_Full_With_IF_First[which(is.na(Article_Author_Full_With_IF_First$IF)),]

gender_ratio_first<-Article_Author_Full_With_IF_First %>% group_by(year)%>% summarise(male=sum(proportion_male),
                                                                                    female=sum(proportion_female))

df_c<-data.frame(table(Article_Author_Full_With_IF$year))
ggplot(df_c)+geom_line(aes(x=as.numeric(Var1), y=Freq))
