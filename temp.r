library(data.table)
dd<-fread("~/Downloads/nacti_metadata.csv")
head(dd)

N<-dd[, .(N=.N), by=list(name, common_name, class)]
N


birds<-dd[class=="aves"]

fwrite(birds, "~/Downloads/bird.nacti.csv")

dd
dd$image<-sprintf("~/Downloads/%s", dd$filename)
dd$image.exists<-file.exists(dd$image)
table(dd$image.exists)

species<-dd[, .(N=.N), by=c("name", "genus" , "family", "order", "class")]
species<-species[!is.na(class)]

for (i in c(1:nrow(species))){
  item<-species[i]
  
  print(sprintf("%d/%d: %s %s %d", i, nrow(species), item$class, item$name, item$N))
  folder<-sprintf("~/Downloads/images/%s/%s/%s/%s/%s",
                  item$class, item$order, item$family, item$genus, item$name)
  if (!dir.exists(folder)){
    dir.create(folder, recursive = T)
  }else{
    next()
  }
  images<-dd[class==item$class & order==item$order & family==item$family & genus==item$genus & name==item$name]
  file.copy(images$image, folder)
}
