# scripts/01_data_acquisition_robust.R
# 增强版数据获取脚本：包含错误处理和重试机制

library(tidyquant)
library(purrr)
library(dplyr)

# 1. 定义我们要获取的股票列表（美股代码通常更稳定）
symbols_to_fetch <- c(
  "AAPL",  # Apple
  "MSFT",  # Microsoft
  "GOOGL", # Google (Alphabet)
  "AMZN",  # Amazon
  "META",  # Meta (Facebook)
  "BABA",   # Alibaba (港股，但在Yahoo上交易)
  "PDD",    # Pinduoduo
  "NTES"    # NetEase
)

# 2. 创建一个安全的数据获取函数，包含重试逻辑
safe_tq_get <- function(symbol, max_retries = 3, delay = 2) {
  
  cat("正在尝试获取 [", symbol, "] 的数据...\n")
  
  for (attempt in 1:max_retries) {
    tryCatch({
      # 尝试获取数据
      df <- tq_get(symbol,
                   get = "stock.prices",
                   from = "2015-01-01",
                   to = Sys.Date(), # 获取到最新日期
                   complete_cases = TRUE)
      
      # 检查是否成功获取到数据
      if (!is.null(df) && nrow(df) > 0) {
        cat("✓ 成功获取 [", symbol, "] 的数据 (第", attempt, "次尝试)\n")
        return(df)
      }
    }, error = function(e) {
      cat("✗ 第", attempt, "次尝试获取 [", symbol, "] 失败: ", e$message, "\n")
      if (attempt < max_retries) {
        cat("等待", delay, "秒后重试...\n")
        Sys.sleep(delay) # 等待一段时间后重试
      }
    })
  }
  
  cat("⚠️  警告: 无法获取 [", symbol, "] 的数据，已跳过。\n")
  return(NULL) # 所有尝试都失败后返回NULL
}

# 3. 逐个获取股票数据（避免批量请求导致的限制）
cat("开始从 Yahoo Finance 获取数据...\n")
all_data_list <- map(symbols_to_fetch, safe_tq_get)

# 4. 清理列表，移去失败的元素（NULL值），并合并为一个数据框
successful_data <- all_data_list %>% 
  compact() %>% # 移除NULL元素
  bind_rows() %>% # 合并所有成功的数据框
  mutate(
    region = ifelse(symbol %in% c("BABA", "PDD", "NTES"), "China", "US")
  )

# 5. 保存成功获取的数据
if (nrow(successful_data) > 0) {
  write_csv(successful_data, "data/raw/stock_prices_raw.csv")
  cat("✓ 数据获取完成！成功获取", length(unique(successful_data$symbol)), "支股票的数据\n")
  cat("已保存至: data/raw/stock_prices_raw.csv\n")
  
  # 显示成功获取的股票列表
  cat("成功获取的股票:", paste(unique(successful_data$symbol), collapse = ", "), "\n")
} else {
  cat("✗ 未能获取任何数据，请检查网络连接后重试。\n")
}