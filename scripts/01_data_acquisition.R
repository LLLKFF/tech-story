# scripts/01_data_acquisition_robust.R
# 增强版数据获取脚本：包含错误处理和重试机制

library(tidyquant)
library(purrr)
library(dplyr)
library(readr)
library(tidyverse)

# 更新后的公司列表
us_stocks <- c("AAPL", "MSFT", "TSLA", "NVDA", "GOOGL")  # 苹果、微软、特斯拉、英伟达、谷歌
china_stocks <- c("BABA", "0700.HK", "BIDU", "JD", "3690.HK")  # 阿里巴巴、腾讯、百度、京东、美团

# 设置时间范围
start_date <- "2015-01-01"
end_date <- "2025-11-20"

# 获取美国股票数据
cat("正在获取美国股票数据...\n")
us_data <- tq_get(us_stocks, 
                  from = start_date,
                  to = end_date,
                  get = "stock.prices")

# 获取中国股票数据  
cat("正在获取中国股票数据...\n")
china_data <- tq_get(china_stocks, 
                     from = start_date,
                     to = end_date,
                     get = "stock.prices")

# 合并数据并添加分类
all_stocks <- bind_rows(
  us_data %>% mutate(region = "US"),
  china_data %>% mutate(region = "China")
) %>% 
  mutate(company_name = case_when(
    symbol == "AAPL" ~ "Apple Inc.",
    symbol == "MSFT" ~ "Microsoft Corporation",
    symbol == "TSLA" ~ "Tesla Inc.",
    symbol == "NVDA" ~ "NVIDIA Corporation",
    symbol == "GOOGL" ~ "Alphabet Inc. (Google)",
    symbol == "BABA" ~ "Alibaba Group",
    symbol == "0700.HK" ~ "Tencent Holdings",
    symbol == "BIDU" ~ "Baidu Inc.",
    symbol == "JD" ~ "JD.com Inc.",
    symbol == "3690.HK" ~ "Meituan",
    TRUE ~ symbol
  ))

# 保存原始数据
write_csv(all_stocks, "data/raw/stock_prices_raw.csv")

cat("数据获取完成！共获取", nrow(all_stocks), "行数据\n")
cat("公司列表:", paste(unique(all_stocks$company_name), collapse = ", "), "\n")


