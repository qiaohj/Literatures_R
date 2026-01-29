library(stringi)
library(data.table)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
all.md<-list.files("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/Word/", pattern="\\.md")
l<-list()
for (i in c(1:length(all.md))){
  f<-all.md[i]
  lines <- readLines(sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/Word/%s", f), encoding = "UTF-8")
  
  full_text <- paste(lines, collapse = "\n")
  
  char_count <- nchar(full_text)
  item<-data.table(name=f, nchar=char_count)
  l[[i]]<-item
}

df<-rbindlist(l)
fwrite(df, "../Data/Vegetation/Word.count.csv")
sum(df$nchar)
View(df)
