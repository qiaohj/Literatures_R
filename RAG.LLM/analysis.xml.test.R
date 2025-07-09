library(xml2)
library(stringr)
library(pdftools)
library(data.table)
library(ggplot2)
library(scales)
library(tidytext)
library(stringi)

setwd("~/GIT/Literatures_R")
pdfs<-list.files("/Users/huijieqiao/PDF/TEST", pattern="\\.pdf")
file.name<-"1591-New Phytologist"
keywords_file <- "RAG.LLM/section_keywords.csv"
keywords_dt <- fread(keywords_file)
keywords_dt[, lower_alias := tolower(alias)]

lengths_list<-list()
for (file.name in pdfs){
  file.name<-gsub("\\.pdf", "", file.name)
  print(file.name)
  pdf_file_path <- sprintf("/Users/huijieqiao/PDF/TEST/%s.pdf", file.name)
  full.text<-pdf_text(pdf_file_path)
  nchar.full.text<-sum(nchar(full.text))
  
  
  xml_file_path <- sprintf("/Users/huijieqiao/PDF/TEST/%s.xml", file.name)
  doc <- read_xml(xml_file_path)
  
  ns <- xml_ns(doc)
  footer_nodes <- xml_find_all(doc, ".//d1:note[@place='foot']", ns)
  if (length(footer_nodes) > 0) {
    xml_remove(footer_nodes)
  }
  #ns <- c(d = "http://www.tei-c.org/ns/1.0")
  
  clean_text_from_node <- function(node) {
    if (is.null(node) || length(node) == 0) return("")
    
    # 创建一个可修改的副本
    node_clone <- xml2::xml_new_root(node)
    
    # *** 核心修正：移除直接的子div，防止内容被重复计算 ***
    #child_divs_to_remove <- xml_find_all(node_clone, "./d1:div", ns)
    #if (length(child_divs_to_remove) > 0) {
    #  xml_remove(child_divs_to_remove)
    #}
    
    # 移除文献引用
    refs_to_remove <- xml_find_all(node_clone, ".//d1:ref", ns)
    if (length(refs_to_remove) > 0) xml_remove(refs_to_remove)
    
    # 提取文本
    text <- xml_text(node_clone, trim = TRUE)
    
    
    # 最终清理
    text <- str_replace_all(text, "\\s+", " ")
    return(trimws(text))
  }
  
  # 1. 在<text>标签下查找所有<div>，这是最可靠的选择器
  content_divs <- xml_find_all(doc, ".//d1:text//d1:div", ns)
  # 2. 提取所有内容div的基础信息
  div_info <- data.table(
    div_index = 1:length(content_divs),
    heading = sapply(content_divs, function(div) {
      head_node <- xml_find_first(div, "./d1:head")
      if (length(head_node) > 0) xml_text(head_node, trim = TRUE) else ""
    }),
    # 使用我们修正后的函数提取内容
    content = sapply(content_divs, clean_text_from_node)
  )
  div_info[, lower_heading := tolower(heading)]
  
  # 3. 匹配关键词
  div_info[keywords_dt, on = .(lower_heading = lower_alias), canonical_name := i.canonical_name]
  
  # 4. 识别需要审查的子标题
  unmatched_headings_report <- div_info[heading != "" & is.na(canonical_name), .(div_index, heading)]
  
  # 5. 向前填充
  div_info[, active_section := na.locf(canonical_name, na.rm = FALSE)]
  
  # 6. 处理前导内容 (Preamble)
  div_info[is.na(active_section), active_section := "Preamble"]
  
  # 7. 聚合内容
  major_sections_dt <- div_info[
    # 过滤掉内容为空的行，这些通常是已被处理的容器div
    nchar(content) > 0, 
    .(full_text = paste(content, collapse = "\n\n")), 
    by = .(section_name = active_section)
  ]
  
  # 8. 转换为命名的列表
  major_sections <- setNames(as.list(major_sections_dt$full_text), major_sections_dt$section_name)
  
  
  
  # --- 整合并输出最终结果 ---
  # ... (这部分与之前的代码相同，此处为简洁省略，请参考上一个回答)
  extracted_data <- list()
  extracted_data$Title <- xml_text(xml_find_first(doc, ".//d1:titleStmt/d1:title[@level='a' and @type='main']", ns), trim = TRUE)
  authors_nodes <- xml_find_all(doc, ".//d1:teiHeader//d1:author/d1:persName", ns)
  extracted_data$Authors <- sapply(authors_nodes, function(p) {
    forename <- paste(xml_text(xml_find_all(p, "./d1:forename")), collapse = " ")
    surname <- xml_text(xml_find_first(p, "./d1:surname"))
    trimws(paste(forename, surname))
  })
  extracted_data$Abstract <- clean_text_from_node(xml_find_first(doc, ".//d1:abstract", ns))
  extracted_data <- c(extracted_data, major_sections)
  
  
  ref_nodes <- xml_find_all(doc, ".//d1:listBibl//d1:note[@type='raw_reference']", ns)
  extracted_data$References <- paste(sapply(ref_nodes, xml_text), collapse = "\n")
  
  figure_nodes <- xml_find_all(doc, ".//d1:figure[not(@type='table')]", ns)
  figures_list <- sapply(figure_nodes, function(fig) {
    head <- xml_text(xml_find_first(fig, ".//d1:head", ns), trim = TRUE)
    desc_node <- xml_find_first(fig, ".//d1:figDesc", ns)
    desc <- clean_text_from_node(desc_node)
    paste0(head, ": ", desc)
  })
  extracted_data$Figures <- paste(figures_list, collapse = "\n\n")
  
  table_nodes <- xml_find_all(doc, ".//d1:figure[@type='table']", ns)
  tables_list <- sapply(table_nodes, function(tbl_fig) {
    head <- xml_text(xml_find_first(tbl_fig, ".//d1:head", ns), trim = TRUE)
    desc_node <- xml_find_first(tbl_fig, ".//d1:figDesc", ns)
    desc <- clean_text_from_node(desc_node)
    
    caption <- paste0(head, " (Caption): ", desc)
    table_content_node <- xml_find_first(tbl_fig, ".//d1:table", ns)
    content_str <- ""
    if (length(table_content_node) > 0) {
      rows <- xml_find_all(table_content_node, ".//d1:row", ns)
      table_text <- sapply(rows, function(row) {
        cells <- xml_text(xml_find_all(row, ".//d1:cell", ns), trim = TRUE)
        paste(cells, collapse = "\t|\t")
      })
      content_str <- paste(table_text, collapse = "\n")
    }
    note_node <- xml_find_first(tbl_fig, ".//d1:note", ns)
    note_text <- if(length(note_node) > 0) clean_text_from_node(note_node) else ""
    paste(caption, "--- Table Content ---\n", content_str, "\n--- Table Note ---\n", note_text, sep = "")
  })
  extracted_data$Table <- paste(tables_list, collapse = "\n\n---\n\n")
  
  if (F){
    View(extracted_data)
  }
  
  char_lengths <- sapply(extracted_data, function(item) {
    # 1. 首先处理 NULL 的情况
    if (is.null(item)) {
      return(0)
    }
    
    # 2. 使用 sum(nchar()) 来统一处理单一字符串和字符串向量
    # nchar() 是向量化的，它会为向量中的每个元素计算长度
    # sum() 将所有长度加起来得到总和
    return(sum(nchar(item)))
  })
  
  lengths_df <- data.frame(
    Section = names(char_lengths),
    Character_Count = char_lengths,
    row.names = NULL
  )
  
  total.xml.length <- sum(lengths_df$Character_Count)
  
  lengths_df<-rbindlist(list(lengths_df,
                             data.frame(Section=c("full.xml", "full.pdf"),
                                        Character_Count=c(total.xml.length,
                                                          nchar.full.text))))
  lengths_df$file.name<-file.name
  lengths_list[[length(lengths_list)+1]]<-lengths_df
  if (F){
    print(lengths_df)
    cat("--- EXTRACTION COMPLETE ---\n\n")
    cat(extracted_data$title, "\n\n")
    
    cat(extracted_data$authors, "\n\n")
    
    cat(extracted_data$abstract, "\n\n")
    
    cat(str_wrap(extracted_data$introduction), "\n\n")
    
    cat(str_wrap(extracted_data$methods), "\n\n")
    
    cat(str_wrap(extracted_data$results), "\n\n")
    
    cat(str_wrap(extracted_data$discussion), "\n\n")
    
    if (!is.null(extracted_data$conclusion)) {
      cat(str_wrap(extracted_data$conclusion), "\n\n")
    }
    
    cat(extracted_data$figures, "\n\n")
    
    cat(extracted_data$tables, "\n\n")
    
    cat(extracted_data$references, "\n\n")
    
    # View(extracted_data)
  }
}
lengths_all<-rbindlist(lengths_list)

View(lengths_all)

plot_data <- lengths_all[Section != "full.xml"][
  , Bar_Group := fcase(
    Section == "full.pdf", "Total PDF Content",
    default = "Extracted Sections"
  )
]
summary_labels <- plot_data[, .(Total_Count = sum(Character_Count)), by = .(file.name, Bar_Group)]

ggplot(plot_data, aes(x = Bar_Group, y = Character_Count, fill = Section)) +
  geom_col(position = "stack") +
  facet_wrap(~ file.name, scales = "free_y") +
  geom_text(
    data = summary_labels,
    aes(x = Bar_Group, y = Total_Count, label = comma(Total_Count)),
    vjust = -0.5,
    size = 3.5,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Comparison of PDF vs. Extracted Section Character Counts",
    subtitle = "Grouped by File Name (using data.table)",
    x = "Content Type",
    y = "Total Character Count",
    fill = "Data Section"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 11)
  )



clean_for_comparison <- function(text) {
  text <- stri_trans_general(text, "Latin-ASCII")
  
  text |>
    str_to_lower() |>
    str_replace_all("-\\s*\\r?\\n", "") |>
    str_replace_all("\\r?\\n", " ") |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish()
}

extracted_data$Discussion

# --- 步骤 3: 清理并准备文本源 (角色反转) ---

# 3.1. [HAYSTACK] 将所有XML提取的文本部分合并成一个巨大的、清理过的字符串
# 我们排除非文本或结构化的部分，如参考文献，因为它们可能不在PDF正文中
xml_text_parts <- extracted_data[!names(extracted_data) %in% c("References")]
clean_xml_blob <- paste(unlist(xml_text_parts), collapse = " ") %>%
  clean_for_comparison()

# 3.2. [NEEDLES] 清理PDF文本，并准备用于回溯的原始PDF文本
clean_pdf_blob <- paste(full.text, collapse = " ") %>%
  clean_for_comparison()
original_pdf_text <- paste(raw_pdf_pages, collapse = " ")


# --- 步骤 4: 执行反向比较 ---

# 4.1. 将清理后的PDF文本分解为句子
pdf_sentences_dt <- data.table(text = clean_pdf_blob)[
  , .(sentence = unlist(unnest_tokens(data.table(text = text), "sentence", "text", token = "sentences")$sentence))
]
# 过滤掉太短的、无意义的“句子”
pdf_sentences_dt <- pdf_sentences_dt[nchar(trimws(sentence)) > 10]

# 4.2. 检查PDF的每个句子是否存在于合并后的XML文本中
pdf_sentences_dt[, is_found_in_xml := str_detect(clean_xml_blob, fixed(sentence))]

# 4.3. 计算总体XML提取完整度分数
total_pdf_sentences <- nrow(pdf_sentences_dt)
found_in_xml <- sum(pdf_sentences_dt$is_found_in_xml)
completeness_score <- if (total_pdf_sentences > 0) (found_in_xml / total_pdf_sentences) * 100 else 100

# 4.4. 识别所有在XML中找不到的PDF句子
missing_pdf_sentences_clean <- pdf_sentences_dt[is_found_in_xml == FALSE, sentence]


# --- 步骤 5: 准备并打印清晰的报告 ---

cat("--- XML EXTRACTION COMPLETENESS REPORT ---\n\n")
cat(sprintf("Overall Completeness Score: %.1f%%\n", completeness_score))
cat("(This score represents the percentage of sentences from the PDF that were found in the extracted XML text.)\n\n")

if (length(missing_pdf_sentences_clean) > 0) {
  cat("--- DETAILS OF CONTENT FOUND IN PDF BUT MISSING FROM XML ---\n\n")
  
  # 为了更好的可读性，我们从原始PDF文本中找到这些缺失句子的未清理版本
  original_pdf_sentences_dt <- data.table(text = full.text)[
    , .(original_sentence = unlist(unnest_tokens(data.table(text = text), "sentence", "text", token = "sentences")$sentence))
  ]
  original_pdf_sentences_dt[, cleaned_sentence := clean_for_comparison(original_sentence)]
  
  # 找到原始句子
  missing_content_original <- original_pdf_sentences_dt[
    cleaned_sentence %in% missing_pdf_sentences_clean, 
    original_sentence
  ]
  
  # 为了使输出更有意义，我们将连续的缺失句子组合成块
  original_pdf_sentences_dt[, is_missing := cleaned_sentence %in% missing_pdf_sentences_clean]
  original_pdf_sentences_dt[, block_id := rleid(is_missing)] # rleid 来自 data.table
  
  missing_blocks <- original_pdf_sentences_dt[is_missing == TRUE, 
                                              .(Missing_Chunk = paste(str_squish(original_sentence), collapse = " ")), 
                                              by = block_id
  ]
  
  cat("The following chunks of text were identified in the PDF but not in your XML extraction:\n\n")
  for (i in 1:nrow(missing_blocks)) {
    cat(paste0("[MISSING CHUNK ", i, "]\n"))
    cat(str_wrap(missing_blocks$Missing_Chunk[i], width = 100)) # 使用str_wrap格式化长文本
    cat("\n-----------------------------------------------------\n\n")
  }
} else {
  cat("--- NO SIGNIFICANT MISSING CONTENT DETECTED ---\n")
  cat("The extracted XML appears to be a good representation of the PDF text content.\n")
}

cat("NOTE: Discrepancies can be caused by OCR errors in the PDF, complex layouts (e.g., multi-column text), or text within images/tables that `pdf_text` might handle differently than the XML parser.\n")
