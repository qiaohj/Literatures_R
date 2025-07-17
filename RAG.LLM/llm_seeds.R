library(httr)
library(data.table)
library(pdftools)
library(xml2)
library(stringr)
library(zoo)
library(ggplot2)
library(scales)

setwd("~/GIT/Literatures_R")

if (F){
  pdfs<-list.files("/Users/huijieqiao/PDF/Training.SEEDS", pattern="\\.pdf")
  lengths_list<-list()
  attnames<-c()
  for (file.name in pdfs){
    file.name<-gsub("\\.pdf", "", file.name)
    print(file.name)
    
    xml_file_path <- sprintf("/Users/huijieqiao/PDF/Training.SEEDS/%s.xml", file.name)
    if (!file.exists(xml_file_path)){
      next()
    }
    doc <- read_xml(xml_file_path)
    
    all_nodes <- xml_find_all(doc, "//*")
    all_attributes_list <- xml_attrs(all_nodes)
    all_attribute_names_list <- lapply(all_attributes_list, names)
    all_attribute_names_vector <- unlist(all_attribute_names_list)
    unique_attribute_names <- unique(all_attribute_names_vector)
    attnames<-c(attnames, unique_attribute_names)
  }
  attnames<-unique(attnames)
  
  attr.names<-c("level", "type", "status", "when", "unit", "from", "to")
}

grobid_url <- "http://localhost:8070/api/processFulltextDocument"

pdfs<-list.files("/Users/huijieqiao/PDF/Training.SEEDS", pattern="\\.pdf", full.names = T)
pdf_path<-pdfs[1]

for (pdf_path in pdfs){
  print(pdf_path)
  res <- POST(
    grobid_url,
    body = list(
      input = upload_file(pdf_path),
      segmentSentences=1,
      consolidateHeader=1,
      includeRawAffiliations=1,
      consolidatFunders=1,
      includeRawCitations=1)
  )
  target<-gsub("\\.pdf", "\\.xml", pdf_path)
  
  if (status_code(res) == 200) {
    xml_content <- content(res, as = "text", encoding = "UTF-8")
    writeLines(xml_content, target)
  } else {
    cat("Error:", status_code(res), "\n")
  }
}

xmls<-list.files("/Users/huijieqiao/PDF/Training.SEEDS", pattern="\\.xml", full.names = T)



clean_text_from_node <- function(node, clean.head=F, type="normal",
                                 with.text=1, remove.ref=0) {
  if (is.null(node) || length(node) == 0) return("")
  clone_node <- xml_new_root(node)
  if (clean.head){
    child_divs_to_remove <- xml_find_all(clone_node, "./d1:head", ns)
    if (length(child_divs_to_remove) > 0) {
      xml_remove(child_divs_to_remove)
    }
  }
  if (remove.ref==1){
    refs_to_remove <- xml_find_all(clone_node, ".//d1:ref", ns)
    if (length(refs_to_remove) > 0) xml_remove(refs_to_remove)
  }
  
  if (type=="normal"){
    named_attrs <- sapply(attr.names, function(attr) {
      xml_attr(clone_node, attr)
    }, USE.NAMES = TRUE)
    
    if (with.text==1){
      text <- xml_text(clone_node, trim = TRUE)
      text <- str_replace_all(text, "\\s+", " ")
      text <- trimws(text)
    }else{
      text<-""
    }
    
    item <- as.data.table(as.list(named_attrs))
    item[, text := text] 
    return(item)
  }
  if (type=="author"){
    forename <- clean_text_from_nodes(xml_find_all(node, "./d1:persName/d1:forename"),
                                      label="forname")
    surname <- clean_text_from_nodes(xml_find_all(node, "./d1:persName/d1:surname"),
                                     label="surname")
    idno <- clean_text_from_nodes(xml_find_all(node, "./d1:idno"),
                                  label="idno")
    return(rbindlist(list(forename, surname, idno), use.names = TRUE, fill = TRUE))
  }
  
  if (type=="body"){
    head_node <- clean_text_from_nodes(xml_find_all(node, "./d1:head"))
    head_to_remove <- xml_find_all(clone_node, "./d1:head")
    if (length(head_to_remove) > 0) xml_remove(head_to_remove)
    
    text <- clean_text_from_node(clone_node)
    return(data.table(text=text$text, head=head_node$text))
  }
}
clean_text_from_nodes <- function(nodes, clean.head=F, label="", 
                                  type="normal",
                                  set.attr=1,
                                  with.text=1,
                                  remove.ref=0) {
  list_of_dts <- lapply(nodes, function(single_node) {
    clean_text_from_node(single_node, clean.head = clean.head, type=type,
                         with.text=with.text, remove.ref=remove.ref)
  })
  
  dt<-rbindlist(list_of_dts, use.names = TRUE, fill = TRUE)
  if (set.attr==1){
    dt$label<-label
  }
  return(dt)
}

keywords_file <- "RAG.LLM/section_keywords.csv"
keywords_dt <- fread(keywords_file)
keywords_dt[, lower_alias := tolower(alias)]

lengths_list<-list()
extracted_list<-list()
pdfs<-list.files("/Users/huijieqiao/PDF/Training.SEEDS", pattern="\\.pdf", full.names = F)
file.name<-pdfs[1]
attr.names<-c("level", "type", "status", "when", "unit", "from", "to")
xpaths<-fread("RAG.LLM/xpath.csv")

for (file.name in pdfs){
  file.name<-gsub("\\.pdf", "", file.name)
  print(file.name)
  if (file.exists( sprintf("/Users/huijieqiao/PDF/Training.SEEDS/%s.rda", file.name))){
    #next()
  }
  pdf_file_path <- sprintf("/Users/huijieqiao/PDF/Training.SEEDS/%s.pdf", file.name)
  full.text<-pdf_text(pdf_file_path)
  nchar.full.text<-sum(nchar(full.text))
  
  xml_file_path <- sprintf("/Users/huijieqiao/PDF/Training.SEEDS/%s.xml", file.name)
  
  doc <- read_xml(xml_file_path)
  ns <- xml_ns(doc)
  
  
  {
    
    i=1
    extracted_data <- list()
    for (i in c(1:nrow(xpaths))){
      xpath<-xpaths[i]
      extracted_data[[xpath$label]]<-clean_text_from_nodes(
        xml_find_all(doc, xpath$xpath), label=xpath$label, type=xpath$type, 
        set.attr=xpath$set.attr,
        with.text=xpath$with.text,
        remove.ref=xpath$remove.ref)
    }
    extracted_data<-rbindlist(extracted_data, fill=T)
    extracted_data$word.length<-nchar(extracted_data$text)
    #sum(extracted_data$word.length)
    if (!("head" %in% colnames(extracted_data))){
      extracted_data$head<-""
      extracted_data[label=="body", head:="Body"]
      extracted_data[label=="back", head:="Back"]
    }
    extracted_data[, lower_heading := tolower(head)]
    
    extracted_data[keywords_dt, on = .(lower_heading = lower_alias), canonical_name := i.canonical_name]
    extracted_data[, active_section := na.locf(canonical_name, na.rm = FALSE)]
    
    extracted_data[is.na(active_section), active_section := "Preamble"]
    
    extracted_data[!label %in% c("body", "back"), active_section:=NA]
    #View(extracted_data)
  }
  
  extracted_data$file.name<-file.name
  extracted_data[label=="title", active_section:="Title"]
  extracted_data[label=="abstract", active_section:="Abstract"]
  saveRDS(extracted_data, sprintf("/Users/huijieqiao/PDF/Training.SEEDS/%s.rda", file.name))
  extracted_list[[length(extracted_list)+1]]<-extracted_data
  
  text_A<-paste(full.text, collapse =" ")
  text_A<-str_replace_all(text_A, "\\s+", " ") %>% str_trim()
  text_A<-str_replace_all(text_A, " ", "") %>% str_trim()
  text_B<-paste(extracted_data$text, collapse =" ")
  text_B<-str_replace_all(text_B, "\\s+", " ") %>% str_trim()
  text_B<-str_replace_all(text_B, " ", "") %>% str_trim()
  
  total.xml.length <- nchar(text_B)
  nchar.full.text<-nchar(text_A)
  
  lengths_df<-extracted_data[!is.na(active_section), c("label", "active_section", "word.length")]
  lengths_df<-lengths_df[,.(Character_Count=sum(word.length)), by=list(active_section)]
  lengths_df<-rbindlist(list(lengths_df,
                             data.frame(active_section=c("full.xml", "full.pdf"),
                                        Character_Count=c(total.xml.length,
                                                          nchar.full.text))))
  lengths_df$file.name<-file.name
  lengths_list[[length(lengths_list)+1]]<-lengths_df
  
}


lengths_all<-rbindlist(lengths_list)
extracted_all<-rbindlist(extracted_list, use.names=T)
#saveRDS(extracted_all, "../Temp/extracted_all.rda")
#View(lengths_all)

plot_data <- lengths_all[
  , Bar_Group := fcase(
    active_section == "full.pdf", "Total PDF Content",
    default = "Extracted Sections"
  )
]
plot_data[active_section=="full.xml", Bar_Group:="Total XML Content"]
summary_labels <- plot_data[, .(Total_Count = sum(Character_Count)), by = .(file.name, Bar_Group)]

ggplot(plot_data, aes(x = Bar_Group, y = Character_Count, fill = active_section)) +
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
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 11)
  )


