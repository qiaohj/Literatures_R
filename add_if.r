library(dplyr)
library(ggplot2)

if (F){
  #check category
  Category_all<-read.csv("C:/Users/Huijie Qiao/Dropbox/Others/SCI-IF/Category.csv", head=T, sep=",", stringsAsFactors = F)
  a<-NULL
  for (cs in Category_all$Category){
    f<-sprintf("C:/Users/Huijie Qiao/Dropbox/Others/SCI-IF/Category/%s.csv", cs)
    if (!file.exists(f)){
      print(cs)
    }
    con <- file(f,"r")
    first_line <- readLines(con,n=1)
    close(con)
    first_line<-trimws(first_line)
    sub<-gsub("Journal Data Filtered By:  Selected JCR Year: 2018 Selected Editions: SCIE Selected Categories: ", 
              "", first_line)
    sub<-gsub("Journal Data Filtered By:  Selected JCR Year: 2018 Selected Editions: SSCI Selected Categories: ", 
              "", sub)
    sub<-gsub(" Selected Category Scheme: WoS,,,,", "", sub)
    sub<-gsub(" Selected Category Scheme: WoS\",,,,", "", sub)
    sub<-gsub("\'", "", sub)
    sub<-gsub("\"", "", sub)
    
    item<-data.frame(cs=cs, sub=sub, stringsAsFactors = F)
    if (is.null(a)){
      a<-item
    }else{
      a<-bind_rows(a, item)
    }
    
  }
  a[which(a$cs!=a$sub),]
  
  Category<-NULL
  for (cs in Category_all$Category){
    f<-sprintf("C:/Users/Huijie Qiao/Dropbox/Others/SCI-IF/Category/%s.csv", cs)
    item<-read.csv(f, head=T, sep=",", skip=1, stringsAsFactors = F)
    item$Category<-cs
    item<-item[, c("Full.Journal.Title", "Category")]
    
    if (is.null(Category)){
      Category<-item
    }else{
      Category<-bind_rows(Category, item)
    }
  }
  f<-sprintf("C:/Users/Huijie Qiao/Dropbox/Others/SCI-IF/JCR1997-2018/JournalHomeGrid %d.csv", 2018)
  if (!file.exists(f)){
    next()
  }
  if_df<-read.csv(f, head=T, sep=",", stringsAsFactors =F, skip=1)
  Category_Full<-left_join(if_df[, c("ISSN", "Full.Journal.Title")], Category, by="Full.Journal.Title")
  write.table(Category_Full, "C:/Users/Huijie Qiao/Dropbox/Others/SCI-IF/Category_Full_2018.csv", row.names = F, sep=",")
}

Category<-read.csv("C:/Users/Huijie Qiao/Dropbox/Others/SCI-IF/Category_Full_2018.csv", head=T, sep=",", stringsAsFactors = F)
t<-data.frame(table(Category$Category))
t[order(t$Freq),]
dim(Category)
unique(Category$Category)



all_IF<-NULL
for (i in c(1997:2018)){
  f<-sprintf("C:/Users/Huijie Qiao/Dropbox/Others/SCI-IF/JCR1997-2018/JournalHomeGrid %d.csv", i)
  if (!file.exists(f)){
    next()
  }
  if_df<-read.csv(f, head=T, sep=",", stringsAsFactors =F, skip=1)
  if_df<-unique(if_df)
  if_df$year<-i
  if_df$Average.Journal.Impact.Factor.Percentile<-as.numeric(if_df$Average.Journal.Impact.Factor.Percentile)
  if_df$Journal.Impact.Factor<-as.numeric(if_df$Journal.Impact.Factor)
  if_df$Eigenfactor.Score<-as.numeric(if_df$Eigenfactor.Score)
  if_df$Normalized.Eigenfactor<-as.numeric(if_df$Normalized.Eigenfactor)
  
  
  if_df<-if_df%>%filter(ISSN!="****-****")
  if_df<-left_join(if_df, Category[, c("ISSN", "Category")], by="ISSN")
  
  titles<-data.frame(table(if_df$JCR.Abbreviated.Title))
  titles[which(titles$Freq>1),]
  if_df[which(if_df$JCR.Abbreviated.Title=="FOOD SCI TECH-BRAZIL"),]
  Category[which(Category$ISSN=="0101-2061"),]
  
  
  if_df[which(if_df$JCR.Abbreviated.Title=="INT J HEALTH POLICY"),]
  print(paste(i, nrow(if_df)))
  if (is.null(all_IF)){
    all_IF<-if_df
  }else{
    all_IF<-bind_rows(all_IF, if_df)
  }
  
}
titles<-unique(all_IF[which(is.na(all_IF$Category)), c("ISSN")])

all_IF[which(is.na(all_IF$Category)), "Category"]<-"UNKNOWN"

unique(all_IF$Category)

#all_IF[which(is.na(all_IF$Journal.Impact.Factor)),]$Journal.Impact.Factor<-0

dim(all_IF)
colnames(all_IF)
#all_IF<-all_IF[complete.cases(all_IF),]

mean_IF<-all_IF %>% group_by(Category)%>% 
  summarise(mean_IF = mean(Journal.Impact.Factor))

tbl_df(mean_IF) %>%
  top_n(10)

mean_IF<-all_IF %>% group_by(Category, year)%>% 
  summarise(mean_IF = mean(Journal.Impact.Factor))
mean_IF<-mean_IF[which(!is.na(mean_IF$mean_IF)),]
ggplot(mean_IF, aes(x=year, y=mean_IF, color=Category))+geom_point()+geom_line()


top_journals<-mean_IF %>% 
  arrange(desc(mean_IF)) %>% 
  group_by(Category) %>% slice(1:2)

ggplot(all_IF %>% filter(JCR.Abbreviated.Title %in% top_journals$JCR.Abbreviated.Title), 
       aes(x=year, y=Journal.Impact.Factor, color=JCR.Abbreviated.Title, group=""))+geom_line()+geom_point()

all_IF[which(all_IF$ISSN=="0028-0836"),]
write.table(all_IF, "C:/Users/Huijie Qiao/Dropbox/Others/SCI-IF/IF_1997_2018.csv", row.names = F, sep=",")
