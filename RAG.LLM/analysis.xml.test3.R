library(xml2)
library(data.table)
library(stringr)
library(zoo)



clean_text_from_node <- function(node, clean.head=F) {
  if (is.null(node) || length(node) == 0) return("")
  clone_node <- xml_new_root(node)
  if (clean.head){
    child_divs_to_remove <- xml_find_all(clone_node, "./d1:head", ns)
    if (length(child_divs_to_remove) > 0) {
      xml_remove(child_divs_to_remove)
    }
  }
  refs_to_remove <- xml_find_all(clone_node, ".//d1:ref", ns)
  if (length(refs_to_remove) > 0) xml_remove(refs_to_remove)
  text <- xml_text(clone_node, trim = TRUE)
  text <- str_replace_all(text, "\\s+", " ")
  return(trimws(text))
}


keywords_file <- "RAG.LLM/section_keywords.csv"
keywords_dt <- fread(keywords_file)
keywords_dt[, lower_alias := tolower(alias)]

file.name<-"1591-New Phytologist"
pdf_file_path <- sprintf("/Users/huijieqiao/PDF/TEST/%s.pdf", file.name)
full.text<-pdf_text(pdf_file_path)
nchar.full.text<-sum(nchar(full.text))

xml_file_path <- sprintf("/Users/huijieqiao/PDF/TEST/%s.xml", file.name)

doc <- read_xml(xml_file_path)
ns <- xml_ns(doc)

extracted_data <- list()

extracted_data$title <- xml_text(xml_find_first(doc, ".//d1:titleStmt/d1:title[@level='a' and @type='main']", ns), trim = TRUE)

authors_nodes <- xml_find_all(doc, ".//d1:teiHeader//d1:author/d1:persName", ns)
extracted_data$authors <- sapply(authors_nodes, function(p) {
  forename <- paste(xml_text(xml_find_all(p, "./d1:forename")), collapse = " ")
  surname <- xml_text(xml_find_first(p, "./d1:surname"))
  trimws(paste(forename, surname))
})
abstract_node <- xml_find_first(doc, ".//d1:abstract", ns)
if (length(abstract_node) > 0) {
  extracted_data$abstract <- clean_text_from_node(abstract_node)
}

keyword_nodes <- xml_find_all(doc, ".//d1:profileDesc/d1:textClass/d1:keywords/d1:term", ns)
if (length(keyword_nodes) > 0) {
  extracted_data$keywords <- sapply(keyword_nodes, xml_text, trim = TRUE)
}

body_divs <- xml_find_all(doc, "./d1:text/d1:body/d1:div", ns)
if (length(body_divs) > 0) {
  div_info_body <- data.table(
    heading = sapply(body_divs, function(div) {
      head_node <- xml_find_first(div, "./d1:head")
      if (length(head_node) > 0) xml_text(head_node, trim = TRUE) else ""
    }),
    div_node = body_divs
  )
  div_info_body[, lower_heading := tolower(heading)]
  div_info_body[keywords_dt, on = .(lower_heading = lower_alias), canonical_name := i.canonical_name]
  
  div_info_body[, active_section := na.locf(canonical_name, na.rm = FALSE)]
  div_info_body[is.na(active_section), active_section := "Preamble"]
  
  body_sections_dt <- div_info_body[, .(Nodes = list(div_node)), by = .(section_name = active_section)]
  
  major_sections <- list()
  for(i in 1:nrow(body_sections_dt)) {
    section_name <- body_sections_dt$section_name[i]
    nodes_list <- body_sections_dt$Nodes[[i]]
    
    full_text <- sapply(nodes_list, function(single_node) {
      return(clean_text_from_node(single_node, clean.head = T))
    })
    
    major_sections[[section_name]] <- paste(full_text, collapse = "\n\n")
  }
  
  extracted_data <- c(extracted_data, major_sections)
  
}

back_divs <- xml_find_all(doc, "./d1:text/d1:back/d1:div[not(@type='references')]", ns)
if (length(back_divs) > 0) {
  div_info_body <- data.table(
    heading = sapply(back_divs, function(div) {
      head_node <- xml_find_first(div, "./d1:head")
      if (length(head_node) > 0) xml_text(head_node, trim = TRUE) else ""
    }),
    div_node = back_divs
  )
  
  div_info_body[, lower_heading := tolower(heading)]
  
  div_info_body[keywords_dt, on = .(lower_heading = lower_alias), canonical_name := i.canonical_name]
  
  div_info_body[, active_section := na.locf(canonical_name, na.rm = FALSE)]
  div_info_body[is.na(active_section), active_section := "Preamble"]
  
  body_sections_dt <- div_info_body[, .(Nodes = list(div_node)), by = .(section_name = active_section)]
  
  
  major_sections <- list()
  for(i in 1:nrow(body_sections_dt)) {
    section_name <- body_sections_dt$section_name[i]
    nodes_list <- body_sections_dt$Nodes[[i]]
    
    full_text <- sapply(nodes_list, function(single_node) {
      return(clean_text_from_node(single_node, clean.head = T))
    })
    
    major_sections[[section_name]] <- paste(full_text, collapse = "\n\n")
  }
  
  extracted_data <- c(extracted_data, major_sections)
}


ref_note_nodes <- xml_find_all(doc, ".//d1:listBibl//d1:biblStruct/d1:note[@type='raw_reference']", ns)
if (length(ref_note_nodes) > 0) {
  extracted_data$References <- paste(sapply(ref_note_nodes, xml_text, trim = TRUE), collapse = "\n")
}
figure_nodes <- xml_find_all(doc, ".//d1:figure[not(@type='table')]", ns)
if(length(figure_nodes) > 0) {
  extracted_data$Figures <- sapply(figure_nodes, function(fig) {
    head <- xml_text(xml_find_first(fig, ".//d1:head", ns), trim = TRUE)
    desc <- clean_text_from_node(xml_find_first(fig, ".//d1:figDesc", ns))
    paste0(head, ": ", desc)
  })
}
table_nodes <- xml_find_all(doc, ".//d1:figure[@type='table']", ns)
if(length(table_nodes) > 0) {
  extracted_data$Tables <- sapply(table_nodes, function(tbl) {
    head <- xml_text(xml_find_first(tbl, ".//d1:head", ns), trim = TRUE)
    desc <- clean_text_from_node(xml_find_first(tbl, ".//d1:figDesc", ns))
    paste0(head, ": ", desc)
  })
}


char_lengths <- sapply(extracted_data, function(item) {
  if (is.null(item)) {
    return(0)
  }
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
