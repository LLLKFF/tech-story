# scripts/02_data_cleaning_enhanced.R
library(tidyverse)
library(lubridate)
library(scales)

# 读取原始数据
cat("正在读取原始数据...\n")
raw_data <- read_csv("data/raw/stock_prices_raw.csv")

# 显示数据概览
cat("数据概览:\n")
cat("时间范围:", as.character(min(raw_data$date)), "至", as.character(max(raw_data$date)), "\n")
cat("包含公司:", paste(unique(raw_data$symbol), collapse = ", "), "\n")
cat("总数据行数:", nrow(raw_data), "\n")

# 数据清洗和增强
cleaned_data <- raw_data %>%
  mutate(date = as.Date(date)) %>%
  arrange(symbol, date) %>%
  group_by(symbol) %>%
  mutate(
    # 计算日收益率
    daily_return = (adjusted - lag(adjusted)) / lag(adjusted),
    # 计算累积收益（以2015年1月为基准100）
    base_price = first(adjusted),
    cumulative_return = (adjusted / base_price) * 100,
    # 添加公司全名和地区
    company_name = case_when(
      symbol == "AAPL" ~ "Apple Inc.",
      symbol == "MSFT" ~ "Microsoft Corporation",
      symbol == "GOOGL" ~ "Alphabet Inc. (Google)",
      symbol == "AMZN" ~ "Amazon.com Inc.",
      symbol == "META" ~ "Meta Platforms Inc.",
      symbol == "BABA" ~ "Alibaba Group",
      symbol == "PDD" ~ "Pinduoduo Inc.",
      symbol == "NTES" ~ "NetEase Inc.",
      TRUE ~ symbol
    ),
    region = ifelse(symbol %in% c("BABA", "PDD", "NTES"), "China", "US")
  ) %>%
  ungroup() %>%
  filter(!is.na(cumulative_return))  # 移除NA值

# 保存清洗后的数据
write_csv(cleaned_data, "data/processed/stock_prices_cleaned.csv")

# 生成基本统计信息
stats_summary <- cleaned_data %>%
  group_by(symbol, company_name, region) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    start_price = first(adjusted),
    end_price = last(adjusted),
    total_return = (end_price - start_price) / start_price,
    .groups = "drop"
  )

cat("\n=== 基本统计分析 ===\n")
print(stats_summary)

# 保存统计摘要
write_csv(stats_summary, "data/processed/stock_stats_summary.csv")

cat("\n数据清洗完成！清洗后数据行数:", nrow(cleaned_data), "\n")