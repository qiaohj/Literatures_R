library(xml2)

xml_file <- "/media/huijieqiao/WD22T_11/literatures/Data/XML/SCIENTIA HORTICULTURAE/J.SCIENTA.2023.112495.PDF"
doc <- read_xml(xml_file)

truncate_text <- function(node) {
  children <- xml_children(node)
  if (length(children) == 0) {
    txt <- xml_text(node)
    if (nchar(txt) > 10) {
      xml_text(node) <- substr(txt, 1, 1000)
    }
  } else {
    lapply(children, truncate_text)
  }
}

truncate_text(xml_root(doc))


write_xml(doc, "~/Downloads/truncated.xml")
