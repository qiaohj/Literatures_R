library(jsonlite)

# 定义文件路径
file_path <- "/media/huijieqiao/WD22T_11/literatures/Data/json_crossref/2025/0.jsonl.gz.json"

# 使用 file() 创建一个文件连接
# 这是推荐的做法，可以更好地控制文件的打开和关闭
con <- file(file_path, "r")

# 使用 stream_in() 读取数据
# 它会逐行读取和解析，然后将结果合并成一个数据框 (data.frame)
my_data <- stream_in(con)

# 完成后关闭文件连接
close(con)

# 查看数据
head(my_data)
# str(my_data)