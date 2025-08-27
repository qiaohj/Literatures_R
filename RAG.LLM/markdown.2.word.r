library(rmarkdown)
library(readr)
library(stringr)
setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")

books<-list.files("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/RAW.BOOK", pattern="\\.pdf")
books<-gsub("\\.pdf", "", books)
book<-"云南植被"
for (book in books){
  
  head<-sprintf('---\ntitle: "%s"\nauthor: "植被结构功能与建造全国重点实验室开放课题"\noutput: %s\n---',
                book, "word_document")
  book.folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/Splitted.TXT/%s", book)
  input_md_file<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/Word/%s.md", book)
  output_docx_file<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/Word/%s.docx", book)
  if (file.exists(output_docx_file)){
    next()
  }
  print(book)
  book.items<-list.files(book.folder,
                         pattern="\\.txt")
  #check that if all the text have been ocred
  
  is.ocred<-T
  item<-book.items[1]
  for (item in book.items){
    item.f<-sprintf("%s/%s", book.folder, item)
    if (file.size(item.f)<=100){
      print(item.f)
      file.remove(item.f)
      is.ocred<-F  
    }
  }
  if (is.ocred==F){
    next()
  }
  text.c<-c()
  for (item in book.items){
    item.f<-sprintf("%s/%s", book.folder, item)
    text<-read_lines(item.f)
    text.c<-c(text.c, text)
  }
  #xx<-nchar(text.c)
  #write_lines(c(head, text.c), input_md_file)
  write_lines(text.c, input_md_file)
  if (book %in% c("中国植物志全版", "云南植物志 1977-2006版 1-21卷合订（带书签可搜索）")){
    next()  
  }

  
  tryCatch({
    rmarkdown::render(
      input = input_md_file,
      output_format = "word_document",
      output_file = output_docx_file,
      quiet = FALSE
    )
    
    cat("--------------------------------------------------\n")
    cat("Convert successfully\n")
    cat(paste("Saved Word to:", normalizePath(output_docx_file), "\n"))
    cat("--------------------------------------------------\n")
    
  }, error = function(e) {
    cat("--------------------------------------------------\n")
    cat("Fail to convert the following file", output_docx_file, "\n")
    cat("Error message:", e$message, "\n")
    cat("--------------------------------------------------\n")
  })
}
