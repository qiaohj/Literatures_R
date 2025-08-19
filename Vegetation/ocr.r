library(pdftools)
library(tesseract)
library(stringr)

setwd("/media/huijieqiao/WD22T_11/literatures/Literatures_R")
raw.folder<-"/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/RAW.BOOK"
pdfs<-list.files(raw.folder, pattern="\\.pdf", ignore.case=T, recursive=T, full.names =T)
ocr_folder<-"/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/OCR.BOOK"
temp.folder<-"/media/huijieqiao/WD22T_11/literatures/Data/Vegetation/TEMP"
output_file_template <- file.path(temp.folder, "%s.page%04d.png")
pdf<-pdfs[1]
tra_journals<-c("台湾自然史（2）台湾植被志（第二卷）高山植被带与高山植物（上）",
                "台湾自然史（3）台湾植被志（第三卷）亚高山冷杉林带与高地草原（下）",
                "台湾自然史（3）台湾植被志（第二卷）高山植被带与高山植物（下）",
                "四川省武隆县火炉区植被调查报告",
                "广西临桂雁山附近的植物群落",
                "广西龙津西南部及其邻近地区的植物群落",
                "柴达木盆地植被与土壤调查报告",
                "澳门植被志（第一卷）",
                "甘肃疏勒河中下游的植被概况",
                "雷州半岛的植被")
if (F){
  for (f in tra_journals){
    orc<-sprintf("%s/%s.pdf", ocr_folder, f)
    file.remove(orc)
  }
}
for (i in c(1:length(pdfs))){
  pdf<-pdfs[i]
  pdf.path<-gsub(raw.folder, ocr_folder, pdf)
  file.name<-basename(pdf)
  file.name.x<-gsub("\\.pdf", "", file.name)
  if (file.exists(pdf.path)){
    next(pdf.path)
  }
  print(pdf.path)
  saveRDS(NULL, pdf.path)
  pages <- pdftools::pdf_info(pdf)$pages
  output_filenames <- sprintf(output_file_template, file.name, 1:pages)
  
  message("Step 1/3: Converting PDF to images...")
  images <- pdftools::pdf_convert(pdf, dpi = 300, filenames=output_filenames)
  message(paste("Successfully converted", length(images), "pages."))
  
  message("Step 2/3: Performing OCR via command line...")
  single_page_pdfs <- c()
  single_page_txts <- c()
  temp_dir <- tempdir()
  
  tesseract_cmd <- "/usr/bin"
  
  
  for (i in seq_along(images)) {
    image_file <- images[i]
    output_base <- file.path(temp_dir, sprintf("page_%04d", i))
    output_pdf_page <- paste0(output_base, ".pdf")
    output_pdf_page.txt <- paste0(output_base, ".txt")
    if (file.name.x %in% tra_journals){
      langs<-"eng+chi_tra"
    }else{
      langs<-"eng+chi_sim"
    }
    
    command <- sprintf('"%s/tesseract" %s %s -l %s pdf txt',
                       tesseract_cmd,
                       shQuote(image_file),
                       shQuote(output_base),
                       langs)
    
    cat(sprintf("Processing page %d/%d: %s\n", i, length(images), command))
    system(command)
    
    if (file.exists(output_pdf_page)) {
      single_page_pdfs <- c(single_page_pdfs, output_pdf_page)
      single_page_txts <- c(single_page_txts, output_pdf_page.txt)
    } else {
      warning(sprintf("Failed to create PDF for page %d.", i))
    }
  }
  
 
  if (length(single_page_pdfs) > 0) {
    message("Step 3/3: Combining pages into a final PDF...")
    pdftools::pdf_combine(input = single_page_pdfs, output = pdf.path)
    message(paste("Success! Searchable PDF saved to:", pdf.path))
    
    all_content <- character(0)
    for (file_path in single_page_txts) {
      # 使用 readLines() 读取文件内容
      content <- readLines(file_path, warn = FALSE)
      # 将当前文件的内容添加到总向量中
      all_content <- c(all_content, content)
    }
    
    all_content<-str_c(all_content, collapse = "\n")
    output_file <- gsub("\\.pdf", "\\.txt", pdf.path)
    writeLines(all_content, output_file)
  } else {
    message("No pages were successfully processed. Final PDF not created.")
  }
  
  unlink(images)
  unlink(single_page_pdfs)
  message("Temporary files cleaned up.")
}
