setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
all.txt<-list.files("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/Splitted.TXT/", full.names = T, recursive = T, pattern="\\.txt")
txt<-"/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/Splitted.TXT/内蒙古森林/内蒙古森林_part_0015.txt"
for (txt in all.txt){
  size<-file.size(txt)
  if (size<=100){
    print(sprintf("remove %s", txt))
    #file.remove(txt)
    next()
  }
  xx<-readLines(txt)
  lines<-nchar(xx)
  if (length(lines[lines>1e4])){
    target<-gsub("Splitted.TXT", "Error.TXT", txt)
    dir<-dirname(target)
    if (!dir.exists(dir)){
      dir.create(dir)
    }
    print(txt)
    #file.rename(txt, target)
  }
}

