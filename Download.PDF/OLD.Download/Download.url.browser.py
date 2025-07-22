import csv
import os
import time
import requests
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, WebDriverException

CSV_FILE = '/home/huijieqiao/Downloads/ECOLOGY.csv'       # 你的CSV文件名
OUTPUT_DIR = '/media/huijieqiao/WD22T_11/literatures/Temp'         # PDF文件保存的文件夹
WAIT_TIMEOUT = 10           # 等待页面元素加载的超时时间（秒）

def download_pdfs_from_csv():
    """
    从CSV文件中读取URL和文件名，使用Selenium解析PDF链接，并下载PDF。
    """
    # 1. 设置Selenium WebDriver
    # 使用ChromeOptions可以进行更多自定义设置，例如无头模式
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless")  # 如果不想看到浏览器窗口，取消此行注释
    options.add_argument("--disable-gpu")
    options.add_argument("--no-sandbox")
    options.add_argument("user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")


    # 推荐让 Selenium 自动管理 driver
    try:
        driver = webdriver.Chrome(options=options)
    except WebDriverException as e:
        print(f"无法启动WebDriver，请确保Chrome浏览器已安装，并且网络通畅。错误: {e}")
        return

    # 2. 创建输出文件夹
    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)
        print(f"创建文件夹: {OUTPUT_DIR}")

    # 3. 读取CSV文件并处理
    try:
        with open(CSV_FILE, mode='r', encoding='utf-8') as file:
            reader = csv.reader(file)
            # next(reader) # 如果你的CSV文件有表头，取消此行注释以跳过第一行

            for i, row in enumerate(reader):
                if len(row) < 2:
                    print(f"第 {i+1} 行数据不完整，跳过。")
                    continue

                page_url = row[0].strip()
                output_filename = row[1].strip()
                output_filepath = os.path.join(OUTPUT_DIR, output_filename)

                print(f"\n--- 处理第 {i+1} 行 ---")
                print(f"页面URL: {page_url}")

                # 4. 使用Selenium访问页面并提取PDF链接
                try:
                    driver.get(page_url)

                    # 等待 meta 标签出现
                    # 我们使用CSS选择器来定位这个特定的meta标签
                    wait = WebDriverWait(driver, WAIT_TIMEOUT)
                    meta_tag = wait.until(
                        EC.presence_of_element_located((By.CSS_SELECTOR, "meta[name='citation_pdf_url']"))
                    )

                    pdf_url = meta_tag.get_attribute('content')
                    if not pdf_url:
                        print("错误：找到了meta标签，但其'content'属性为空。")
                        continue
                    
                    print(f"成功解析到PDF链接: {pdf_url}")

                    # 5. 使用Requests下载PDF文件
                    try:
                        print(f"开始下载PDF并保存为: {output_filepath}")
                        response = requests.get(pdf_url, stream=True, timeout=30)
                        response.raise_for_status()  # 如果请求失败 (如 404), 会抛出异常

                        with open(output_filepath, 'wb') as f:
                            for chunk in response.iter_content(chunk_size=8192):
                                f.write(chunk)
                        
                        print(f"成功下载: {output_filename}")

                    except requests.exceptions.RequestException as e:
                        print(f"下载PDF时出错: {e}")

                except TimeoutException:
                    print(f"错误：在 {WAIT_TIMEOUT} 秒内未能找到 'citation_pdf_url' meta标签。可能页面结构不同或加载失败。")
                except Exception as e:
                    print(f"访问页面或解析时发生未知错误: {e}")
                
                # 添加一个小的延迟，避免对服务器造成过大压力
                time.sleep(2)

    except FileNotFoundError:
        print(f"错误：找不到CSV文件 '{CSV_FILE}'。请确保它在正确的路径下。")
    finally:
        # 6. 关闭浏览器
        print("\n--- 所有任务完成，关闭浏览器 ---")
        driver.quit()
    time.sleep(10)

if __name__ == '__main__':
    download_pdfs_from_csv()
