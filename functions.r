insertKeyword<-function(node, type){
  attrs<-xmlAttrs(node)
  text<-gsub("'", "’", xmlValue(node))
  if ("UI" %in% names(attrs)){
    UI<-attrs[which(names(attrs)=="UI")]
  }else{
    UI<-""
  }
  if ("MajorTopicYN" %in% names(attrs)){
    MajorTopicYN<-attrs[which(names(attrs)=="MajorTopicYN")]
  }else{
    MajorTopicYN<-""
  }
  if (MajorTopicYN=="Y"){
    MajorTopicYN<-1
  }else{
    MajorTopicYN<-0
  }
  sql<-sprintf("SELECT * FROM Keyword WHERE UI='%s'", UI)
  if (!is_exist_db(sql)){
    sql<-sprintf("INSERT INTO Keyword (UI, Name, Type) VALUES ('%s', '%s', '%s')", UI, text, type)
    dbExecute(con, sql)
  }
  return(data.frame(UI=UI, MajorTopicYN=MajorTopicYN))
}


getXmlValue<-function(xml, path, type){
  v<-xpathApply(xml, path, xmlValue)
  if (type!="list"){
    if (length(v)==0){
      v<-NA
    }else{
      v<-v[[1]]
    }
    if (type=="character"){
      if (is.na(v)){
        v<-""
      }
    }
    if (type=="numeric"){
      if (is.na(v)){
        v<- -9999
      }else{
        if (v==""){
          v<- -9999
        }
        v<-as.numeric(v)
      }
    }
    if (type=="boolean"){
      if (is.na(v)){
        v<-0
      }else{
        if (v=="Y"){
          v<-1
        }else{
          v<-0
        }
      }
    }
  }
  if (type=="character"){
    v<-fix_sql(v)
  }
  return (v)
}
is_exist_db<-function(sql){
  rs<-dbSendQuery(con, sql)
  df_ui<-fetch(rs, n=-1)
  dbClearResult(rs)
  if (nrow(df_ui)==0){
    return(F)
  }else{
    return(T)
  }
}
fix_sql<-function(str){
  str<-gsub("'", "’", str)
  str<-gsub("\\\\", "", str)
  return (str)
}
killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm),
                     median = median (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  #datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
