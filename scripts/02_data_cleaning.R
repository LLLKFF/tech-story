# scripts/02_data_cleaning_enhanced.R
library(tidyverse)
library(lubridate)
library(scales)

# 读取原始数据
cat("正在读取原始数据...\n")
raw_data <- read_csv("data/raw/stock_prices_raw.csv")

library(tidyverse)
library(lubridate)
library(scales)

# 读取原始数据
cat("正在读取原始数据...\n")
raw_data <- read_csv("data/raw/stock_prices_raw.csv")

# 数据清洗和增强
cleaned_data <- raw_data %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(symbol, date) %>% 
  # 首先替换股票代码为可读的公司名称
  mutate(
    symbol = case_when(
      symbol == "0700.HK" ~ "Tencent",
      symbol == "3690.HK" ~ "Meituan",
      symbol == "AAPL" ~ "Apple",
      symbol == "MSFT" ~ "Microsoft",
      symbol == "TSLA" ~ "Tesla", 
      symbol == "NVDA" ~ "NVIDIA",
      symbol == "GOOGL" ~ "Google",
      symbol == "BABA" ~ "Alibaba",
      symbol == "BIDU" ~ "Baidu",
      symbol == "JD" ~ "JD.com",
      TRUE ~ symbol  # 保持其他代码不变
    )
  ) %>%
  # 同时更新公司名称，确保一致性
  mutate(
    company_name = case_when(
      symbol == "Tencent" ~ "Tencent Holdings",
      symbol == "Meituan" ~ "Meituan",
      symbol == "Apple" ~ "Apple Inc.",
      symbol == "Microsoft" ~ "Microsoft Corporation",
      symbol == "Tesla" ~ "Tesla Inc.",
      symbol == "NVIDIA" ~ "NVIDIA Corporation",
      symbol == "Google" ~ "Alphabet Inc. (Google)",
      symbol == "Alibaba" ~ "Alibaba Group",
      symbol == "Baidu" ~ "Baidu Inc.",
      symbol == "JD.com" ~ "JD.com Inc.",
      TRUE ~ company_name
    )
  ) %>%
  group_by(symbol) %>% 
  mutate(
    # 计算日收益率
    daily_return = (adjusted - lag(adjusted)) / lag(adjusted),
    # 计算累积收益(以2015年1月为基准100)
    base_price = first(adjusted),
    cumulative_return = (adjusted / base_price) * 100,
    # 更新公司分类（使用新的符号名称）
    company_category = case_when(
      symbol %in% c("Apple", "Microsoft", "NVIDIA") ~ "Hardware/semiconductors",
      symbol %in% c("Google", "Baidu") ~ "Search/Advertising",
      symbol == "Tesla" ~ "Automotive/Energy",
      symbol %in% c("Alibaba", "JD.com") ~ "E-commerce",
      symbol == "Tencent" ~ "Social Media/Gaming",
      symbol == "Meituan" ~ "Local Services",
      TRUE ~ "other"
    )
  ) %>% 
  ungroup() %>% 
  filter(!is.na(cumulative_return))

# 保存清洗后的数据
write_csv(cleaned_data, "data/processed/stock_prices_cleaned.csv")

# 生成基本统计信息
stats_summary <- cleaned_data %>% 
  group_by(symbol, company_name, region, company_category) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    start_price = first(adjusted),
    end_price = last(adjusted),
    total_return = (end_price - start_price) / start_price,
    .groups = "drop"
  )

cat("\n=== 新的公司组合统计分析 ===\n")
print(stats_summary)

# 验证替换结果
cat("\n=== 符号替换验证 ===\n")
unique_symbols <- unique(cleaned_data$symbol)
cat("当前使用的公司符号:", paste(unique_symbols, collapse = ", "), "\n")