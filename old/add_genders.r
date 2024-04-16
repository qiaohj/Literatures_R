library("genderizeR")
x = c("Jongkar Grinang",
      "Nazalan Najimudin",
      "Suriani Mohamad")

x = rep(c("Jongkar",
      "Nazalan",
      "Suriani"), 1000)

givenNames = findGivenNames(x, progress = FALSE)

genderize(x, genderDB = givenNames, progress = FALSE)


library("gender")
library("genderdata")

gender(x)
gender(names, years = c(1932, 2012), 
       method = c("ssa", "ipums", "napp", "kantrowitz", "genderize", "demo"), 
       countries = c("United States", "Canada", "United Kingdom", "Denmark", "Iceland", "Norway", "Sweden"))

gender(x, method = "ssa", year=c(1932, 2012))
gender(x, method = "genderize")

gender(x, method = "napp", countries = c("Sweden", "Denmark"))
data(package = "genderdata")


library("RMySQL")
library("purrr")
con<-dbConnect(MySQL(), user="", password="", 
               dbname="PubMed", host="172.16.120.50")
sql<-"SELECT * FROM Author"
rs<-dbSendQuery(con, sql)
df<-fetch(rs, n=-1)
dbClearResult(rs)

dim(df)

head(df)
tail(df)
nrow(df)

x<-df[, "ForeName"]
name<-"N"
nameFun(name)
nameFun<-function(name){
   items<-unlist(strsplit(trimws(name), " "))
   items<-items[nchar(items)==max(nchar(items))]
   return(items[1])
}
for (name in x){
   print(paste(name, map_chr(name, nameFun), sep=":"))
   my.age <- readline(prompt="Enter age: ")
   if (my.age=="X"){
      break()
   }
}

names<-map_chr(x, nameFun)

genders<-gender(unique(names), method = "ssa")
saveRDS(genders, "../Objects/genders.rda")
nrow(genders)
