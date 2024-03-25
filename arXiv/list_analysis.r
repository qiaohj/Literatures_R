library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")
arXiv_list<-fread("../Data/arXiv/arxiv-dataset_list-of-files.txt", header=F)
colnames(arXiv_list)<-"URL"
arXiv_list[, c("v1", "v2", "v3", "v4", "category", "type", "yymm", "v8", "v9") := tstrsplit(URL, "/", fixed=TRUE)]
arXiv_list<-arXiv_list[!is.na(v8)]


arXiv_list[!is.na(v9)]
arXiv_list[v8=="9810001v1"]
colnames()