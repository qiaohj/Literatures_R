library(R.utils)
library(rjson)
library(data.table)
library(stringi)


setwd("/media/huijieqiao/WD22T_11/literatures/Script")


lines <- readLines("../Data/JSTOR/jstor_metadata_2025-07-02.jsonl")
results <- lapply(lines, fromJSON)
for (j in c(1:length(results))){
  if (j %% 1000 ==0){
    print(paste(i, length(zips), f, j, length(results)))
  }
  item<-results[[j]]
}