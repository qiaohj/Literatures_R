folder<-"/media/huijieqiao/WD22T_11/literatures/Data/PDF/PEERJ"
files<-list.files(folder, pattern="\\.PDF", full.names = T)
f<-files[1]
for (f in files){
  pages<-pdf_info(f)$pages
  if (pages==0){
    print(f)
    file.remove(f)
  }
}

