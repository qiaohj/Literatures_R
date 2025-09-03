keywords_file <- "RAG.LLM/section_keywords.csv"
keywords_dt <- fread(keywords_file)
keywords_dt[, lower_alias := tolower(alias)]
keywords_dt$lower_alias<-trimws(gsub(" ", "", keywords_dt$lower_alias))

attr.names<-c("level", "type", "status", "when", "unit", "from", "to")
xpaths<-fread("RAG.LLM/xpath.csv")
xpaths.elsivier<-fread("RAG.LLM/xpath.elsevier.csv")

clean_text_from_node <- function(node, clean.head=F, type="normal",
                                 with.text=1, remove.ref=0, ns) {
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
    #refs_to_remove <- xml_find_all(clone_node, "./ce:cross-ref", ns)
    
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
                                      label="forname", ns=ns)
    surname <- clean_text_from_nodes(xml_find_all(node, "./d1:persName/d1:surname"),
                                     label="surname", ns=ns)
    idno <- clean_text_from_nodes(xml_find_all(node, "./d1:idno"),
                                  label="idno", ns=ns)
    return(rbindlist(list(forename, surname, idno), use.names = TRUE, fill = TRUE))
  }
  
  if (type=="body"){
    head_node <- clean_text_from_nodes(xml_find_all(node, "./d1:head"), ns=ns)
    head_to_remove <- xml_find_all(clone_node, "./d1:head")
    if (length(head_to_remove) > 0) xml_remove(head_to_remove)
    
    text <- clean_text_from_node(clone_node, ns=ns)
    return(data.table(text=text$text, head=head_node$text))
  }
  
  if (type=="section"){
    head_node <- clean_text_from_nodes(xml_find_all(node, "./ce:section-title"), ns=ns)
    head_to_remove <- xml_find_all(clone_node, "./ce:section-title")
    if (length(head_to_remove) > 0) xml_remove(head_to_remove)
    head_to_remove <- xml_find_all(clone_node, "./ce:label")
    if (length(head_to_remove) > 0) xml_remove(head_to_remove)
    
    
    text <- clean_text_from_node(clone_node, ns=ns)
    return(data.table(text=text$text, head=head_node$text))
  }
  
}
clean_text_from_nodes <- function(nodes, clean.head=F, label="", 
                                  type="normal",
                                  set.attr=1,
                                  with.text=1,
                                  remove.ref=0, ns) {
  list_of_dts <- lapply(nodes, function(single_node) {
    clean_text_from_node(single_node, clean.head = clean.head, type=type,
                         with.text=with.text, remove.ref=remove.ref, ns=ns)
  })
  
  dt<-rbindlist(list_of_dts, use.names = TRUE, fill = TRUE)
  if (set.attr==1){
    dt$label<-label
  }
  return(dt)
}


xml2csv<-function(xml_file_path){
  if (!file.exists(xml_file_path)){
    return(NULL)
  }
  doc <- tryCatch({read_xml(xml_file_path)},
                  error = function(e) {
                    message("Error: ", e$message)
                    return(NULL)
                  },
                  warning = function(w) {
                    message("Warning: ", w$message)
                    return(NULL)
                  },
                  finally = {
                    
                  }
  )
  if (is.na(doc)){
    return(NULL)
  }
  ns <- xml_ns(doc)
  
  i=1
  extracted_data <- list()
  for (i in c(1:nrow(xpaths))){
    xpath<-xpaths[i]
    extracted_data[[xpath$label]]<-clean_text_from_nodes(
      xml_find_all(doc, xpath$xpath), label=xpath$label, type=xpath$type, 
      set.attr=xpath$set.attr,
      with.text=xpath$with.text,
      remove.ref=xpath$remove.ref, ns=ns)
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
  extracted_data$lower_heading<-trimws(gsub("\\|", " ", extracted_data$lower_heading))
  extracted_data$lower_heading<-trimws(gsub(" ", "", extracted_data$lower_heading))
  
  extracted_data[keywords_dt, on = .(lower_heading = lower_alias), canonical_name := i.canonical_name]
  extracted_data[, active_section := na.locf(canonical_name, na.rm = FALSE)]
  
  extracted_data[is.na(active_section), active_section := "Unknown"]
  
  extracted_data[!label %in% c("body", "back"), active_section:=NA]
  extracted_data$file.name<-gsub("\\.XML", "", basename(xml_file_path))
  extracted_data[label=="title", active_section:="Title"]
  extracted_data[label=="abstract", active_section:="Abstract"]
  extracted_data
}




elsevier.xml2csv<-function(xml_file_path){
  if (!file.exists(xml_file_path)){
    return(NULL)
  }
  doc <- tryCatch({read_xml(xml_file_path)},
                  error = function(e) {
                    message("Error: ", e$message)
                    return(NULL)
                  },
                  warning = function(w) {
                    message("Warning: ", w$message)
                    return(NULL)
                  },
                  finally = {
                    
                  }
  )
  if (is.na(doc)){
    return(NULL)
  }
  ns <- xml_ns(doc)
  i=1
  extracted_data <- list()
  for (i in c(1:nrow(xpaths.elsivier))){
    xpath.elsivier<-xpaths.elsivier[i]
    extracted_data[[xpath.elsivier$label]]<-clean_text_from_nodes(
      xml_find_all(doc, xpath.elsivier$xpath), 
      label=xpath.elsivier$label, type=xpath.elsivier$type, 
      set.attr=xpath.elsivier$set.attr,
      with.text=xpath.elsivier$with.text,
      remove.ref=xpath.elsivier$remove.ref, ns=ns)
  }
  extracted_data<-rbindlist(extracted_data, fill=T)
  extracted_data$word.length<-nchar(extracted_data$text)
  if (!("head" %in% colnames(extracted_data))){
    extracted_data$head<-""
    extracted_data[label=="body_para", head:="Body"]
    extracted_data[label=="description", head:="Body"]
    extracted_data[label=="simple_para", head:="Body"]
  }
  extracted_data[, lower_heading := tolower(head)]
  extracted_data$lower_heading<-trimws(gsub("\\|", " ", extracted_data$lower_heading))
  extracted_data$lower_heading<-trimws(gsub(" ", "", extracted_data$lower_heading))
  
  extracted_data[keywords_dt, on = .(lower_heading = lower_alias), canonical_name := i.canonical_name]
  extracted_data[, active_section := na.locf(canonical_name, na.rm = FALSE)]
  
  extracted_data[is.na(active_section), active_section := "Unknown"]
  
  extracted_data$file.name<-gsub("\\.XML", "", basename(xml_file_path))
  extracted_data[label=="title", active_section:="Title"]
  extracted_data[label=="abstract", active_section:="Abstract"]
  extracted_data
}



