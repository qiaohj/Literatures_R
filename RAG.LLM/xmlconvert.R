library("xmlconvert")
file.name<-"1591-New Phytologist"
xml_file_path <- sprintf("/Users/huijieqiao/PDF/TEST/%s.xml", file.name)


# 3. 定义核心的递归转换函数
# 这个函数接受一个xml_node对象，并返回一个代表该节点的R列表
xml_to_list <- function(node) {
  
  # 基本情况: 如果节点是文本节点，返回其文本内容
  # 我们只关心非空的文本节点
  if (xml_type(node) == "text") {
    text_content <- xml_text(node, trim = TRUE)
    if (nzchar(text_content)) { # nzchar() 检查字符串是否非空
      return(text_content)
    }
    return(NULL) # 忽略只包含空白的文本节点
  }
  
  # 递归情况: 如果节点是元素节点
  if (xml_type(node) == "element") {
    # 创建一个列表来存储此节点的信息
    result <- list()
    
    # a) 存储节点名称 (tag name)
    result$name <- xml_name(node)
    
    # b) 存储所有属性
    attrs <- xml_attrs(node)
    if (length(attrs) > 0) {
      result$attributes <- as.list(attrs)
    }
    
    # c) 递归处理所有子节点 (包括元素和文本)
    children_nodes <- xml_contents(node)
    if (length(children_nodes) > 0) {
      # 对每个子节点调用此函数
      children_list <- lapply(children_nodes, xml_to_list)
      # 移除由空文本节点等产生的NULL值
      children_list <- children_list[!sapply(children_list, is.null)]
      if (length(children_list) > 0) {
        result$children <- children_list
      }
    }
    
    return(result)
  }
  
  # 忽略其他类型的节点，如注释、处理指令等
  return(NULL)
}

# 4. 解析XML字符串
# read_xml() 将字符串转换为一个xml_document对象
doc <- read_xml(xml_file_path)

# 5. 调用函数进行转换
# 我们从XML文档的根节点开始
r_object <- xml_to_list(doc)


# 6. 显示结果
cat("XML已成功转换为一个名为 'r_object' 的R列表对象。\n")
cat("----------------------------------------------------\n\n")

# 由于对象非常大，直接打印会刷屏。
# 我们使用 str() 来展示对象的结构，限制显示深度以便查看。
cat("使用 str() 查看对象结构 (前4层):\n")
str(r_object, max.level = 4)

cat("\n----------------------------------------------------\n\n")

# 7. 访问示例：如何从对象中提取信息
cat("访问示例: 提取文章主标题\n")
# 根据XML结构，路径是 TEI -> teiHeader -> fileDesc -> titleStmt -> title
# 在我们的列表里，这对应于:
main_title_node <- r_object$children[[1]]$children[[1]]$children[[1]]$children[[1]]

# 打印标题节点的完整信息
print(main_title_node)

# 仅提取标题文本
# 标题文本是标题节点的第一个子节点
main_title_text <- main_title_node$children[[1]]
cat("\n提取到的标题文本是: '", main_title_text, "'\n", sep="")