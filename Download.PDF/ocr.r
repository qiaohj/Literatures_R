library(pdftools)
library(tesseract)
setwd("/media/huijieqiao/WD22T_11/literatures/Script")

journal.name<-"NEW PHYTOLOGIST"

ocr_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/OCR.PDF/%s", journal.name)

if (!dir.exists(ocr_folder)){
  dir.create(ocr_folder)
}

pdf_folder<-sprintf("/media/huijieqiao/WD22T_11/literatures/Data/PDF/%s", journal.name)

pdfs<-list.files(pdf_folder, pattern = "\\.PDF")
pdf<-pdfs[1]
for (pdf in pdfs){
  pdf.path<-sprintf("%s/%s", pdf_folder, pdf)
  ocr.path<-sprintf("%s/%s", ocr_folder, pdf)
  print(pdf.path)
  file.size<-file.size(pdf.path)
  if (file.size<1024){
    next()
  }
  text<-pdf_text(pdf.path)
  if (sum(nchar(text))>1000){
    next()
  }
  file.rename(pdf.path, ocr.path)
  
  message("Step 1/3: Converting PDF to images...")
  images <- pdftools::pdf_convert(ocr.path, dpi = 300)
  message(paste("Successfully converted", length(images), "pages."))
  
  # 4. 循环调用命令行工具生成单页可搜索PDF
  message("Step 2/3: Performing OCR via command line...")
  single_page_pdfs <- c()
  temp_dir <- tempdir() # 使用临时目录
  
  tesseract_cmd <- "/usr/bin"
  
  
  for (i in seq_along(images)) {
    image_file <- images[i]
    
    # Tesseract命令行会自己添加.pdf后缀，所以我们提供不带扩展名的输出路径
    output_base <- file.path(temp_dir, sprintf("page_%03d", i))
    output_pdf_page <- paste0(output_base, ".pdf")
    
    # 构建命令行指令： tesseract input.png output_base -l lang pdf
    # 我们需要用 shQuote() 来处理可能包含空格的路径
    command <- sprintf('"%s/tesseract" %s %s -l %s pdf',
                       tesseract_cmd,
                       shQuote(image_file),
                       shQuote(output_base),
                       "eng")
    
    # 执行命令
    cat(sprintf("Processing page %d/%d: %s\n", i, length(images), command))
    system(command) # system()会执行这条指令
    
    # 检查文件是否生成
    if (file.exists(output_pdf_page)) {
      single_page_pdfs <- c(single_page_pdfs, output_pdf_page)
    } else {
      warning(sprintf("Failed to create PDF for page %d.", i))
    }
  }
  
  # 5. 合并所有单页PDF
  if (length(single_page_pdfs) > 0) {
    message("Step 3/3: Combining pages into a final PDF...")
    pdftools::pdf_combine(input = single_page_pdfs, output = pdf.path)
    message(paste("Success! Searchable PDF saved to:", pdf.path))
  } else {
    message("No pages were successfully processed. Final PDF not created.")
  }
  
  
  # 6. 清理临时文件
  unlink(images)
  unlink(single_page_pdfs)
  message("Temporary files cleaned up.")
  
}
