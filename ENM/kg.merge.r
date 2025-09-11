library(data.table)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

target.journals<-readRDS("../Data/ENM/clean.journal.rda")
entities<-list()
relations<-list()
for (i in c(1:nrow(target.journals))){
  
  item<-target.journals[i]
  folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/ENM/LLM.Parse/%s", item$Title)
  print(paste(i, item$Title))
  kg.json<-list.files(folder, pattern="\\.kg.json.rda", full.name=T)
  if (length(kg.json)==0){
    next()
  }
  j=1
  for (j in c(1:length(kg.json))){
    json<-readRDS(kg.json[j])
    entity<-json$entities
    if (class(entity)=="list"){
      entity<-rbindlist(entity)
      entity<-unique(entity)
    }
    if (nrow(entity)==0){
      next()
    }
    entity$doi<-unique(json$doi)
    relation<-json$relations
    if (class(relation)=="list"){
      relation<-rbindlist(relation)
      relation<-unique(relation)
      
    }
    if (is.null(relation)){
      next()
    }
    if (nrow(relation)==0){
      next()
    }
    relation$doi<-unique(json$doi)
    entities[[length(entities)+1]]<-entity
    relations[[length(relations)+1]]<-relation
  }
}
entities.df<-rbindlist(entities, fill=T)
entities.df<-entities.df[!is.na(label)]
entities.df[, label.length:=lengths(label)]
entities.df<-entities.df[label.length==1]
entities.df$label<-as.character(entities.df$label)

entities.type.N<-entities.df[,.(N=.N), by=list(type)]
entities.df<-entities.df[type %in% entities.type.N[N>5000]$type]
entities.df<-entities.df[,c("id", "type", "label", "context", "doi")]
entities.df$label<-tolower(entities.df$label)

entities.N<-entities.df[, .(N=.N), by=c("label", "type")]
setorderv(entities.N, c("type", "N"), c(1, -1))
entities.N[, `:=`(cum_by_type = cumsum(N),
           cum_perc_by_type = cumsum(N) / sum(N)), by = type]
main.entities<-entities.N[N>=10]
str_c(unique(main.entities$type), collapse = ",")

fwrite(main.entities, "../Data/ENM/entities.main.csv")


fwrite(entities.N[, c("label")], "../Data/ENM/entities.csv")
quantile(entities.N$N, seq(0, 1, 0.01))
setorderv(entities.N, "N", -1)

relations.df<-rbindlist(relations, fill=T)
relations.df<-relations.df[!is.na(to_id) & !is.na(from_id)]
relations.df<-relations.df[, c("from_id", "to_id", "type", "context", "doi")]
relations.N<-relations.df[, .(N=.N), by=type]
saveRDS(entities.df, "../Data/ENM/entities.rda")
saveRDS(relations.df, "../Data/ENM/relations.rda")

entities.N<-entities.df[,.(N=.N), by=c("label")]
