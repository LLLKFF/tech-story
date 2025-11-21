# 完整增强版可视化函数库
library(tidyverse)
library(plotly)
library(lubridate)
library(scales)
library(viridis)
library(reshape2)  # 用于相关性分析

# 设置主题
tech_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 1. 累积收益对比图（赛车图）
create_race_chart <- function(data) {
  company_colors <- get_company_colors()
  current_companies <- unique(data$symbol)
  available_colors <- company_colors[names(company_colors) %in% current_companies]
  
  p <- ggplot(data, aes(x = date, y = cumulative_return, 
                        color = symbol, group = symbol,
                        text = paste("Company:", symbol,
                                     "<br>Date:", date,
                                     "<br>Cumulative Return:", round(cumulative_return, 1), "%"))) +
    geom_line(size = 1.2, alpha = 0.8) +
    labs(
      title = "US-China Tech Giants Performance Comparison",
      subtitle = "Cumulative Returns (Base: Jan 2015 = 100)",
      x = "Date",
      y = "Cumulative Return (%)",
      color = "Company"
    ) +
    scale_color_manual(values = available_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    tech_theme
  
  ggplotly(p, tooltip = "text") %>%
    layout(legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center"))
}

# 2. 地区对比图
create_region_comparison <- function(data) {
  region_data <- data %>%
    group_by(region, date) %>%
    summarise(
      avg_cumulative_return = mean(cumulative_return, na.rm = TRUE),
      .groups = 'drop'
    )
  
  p <- ggplot(region_data, aes(x = date, y = avg_cumulative_return, 
                               color = region, fill = region)) +
    geom_line(size = 1.5) +
    geom_area(alpha = 0.3) +
    labs(
      title = "Regional Performance Comparison",
      subtitle = "Average cumulative returns by region",
      x = "Date",
      y = "Average Cumulative Return (%)",
      color = "Region",
      fill = "Region"
    ) +
    scale_color_manual(values = c("US" = "#3498db", "China" = "#e74c3c")) +
    scale_fill_manual(values = c("US" = "#3498db", "China" = "#e74c3c")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    tech_theme
  
  ggplotly(p)
}

# 3. 收益率热力图
create_returns_heatmap <- function(data) {
  yearly_returns <- data %>%
    mutate(year = year(date)) %>%
    group_by(symbol, year) %>%
    summarise(
      yearly_return = (last(adjusted) - first(adjusted)) / first(adjusted),
      .groups = 'drop'
    )
  
  p <- ggplot(yearly_returns, aes(x = year, y = symbol, fill = yearly_return,
                                  text = paste("Company:", symbol,
                                               "<br>Year:", year,
                                               "<br>Return:", percent(yearly_return)))) +
    geom_tile() +
    labs(
      title = "Annual Returns Heatmap",
      x = "Year",
      y = "Company",
      fill = "Annual Return"
    ) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green",
                         midpoint = 0, labels = percent) +
    scale_x_continuous(breaks = seq(2015, 2024, 1)) +
    tech_theme
  
  ggplotly(p, tooltip = "text")
}

# 4. 风险-收益分析
create_risk_return_analysis <- function(data) {
  # 计算风险收益指标
  performance_metrics <- data %>%
    group_by(symbol, region) %>%
    summarise(
      total_return = (last(adjusted) - first(adjusted)) / first(adjusted),
      annualized_return = (1 + total_return)^(1/10) - 1,  # 10年数据
      volatility = sd(daily_return, na.rm = TRUE) * sqrt(252),
      sharpe_ratio = annualized_return / volatility,
      max_drawdown = min((adjusted - cummax(adjusted)) / cummax(adjusted), na.rm = TRUE),
      .groups = 'drop'
    )
  
  p <- ggplot(performance_metrics, aes(x = volatility, y = annualized_return, 
                                       color = region, size = total_return,
                                       text = paste("Company:", symbol,
                                                    "<br>Annual Return:", percent(annualized_return),
                                                    "<br>Volatility:", percent(volatility),
                                                    "<br>Sharpe Ratio:", round(sharpe_ratio, 2),
                                                    "<br>Max Drawdown:", percent(max_drawdown)))) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = symbol), vjust = -0.8, size = 3, check_overlap = TRUE) +
    labs(
      title = "Risk-Return Profile Analysis",
      subtitle = "Annualized Return vs. Volatility (2015-2024)",
      x = "Annual Volatility", 
      y = "Annualized Return",
      size = "Total Return",
      color = "Region"
    ) +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(labels = percent) +
    scale_color_manual(values = c("US" = "#3498db", "China" = "#e74c3c")) +
    tech_theme
  
  ggplotly(p, tooltip = "text")
}

# 5. 相关性分析
create_correlation_analysis <- function(data) {
  # 创建收益率矩阵
  returns_matrix <- data %>%
    select(symbol, date, daily_return) %>%
    pivot_wider(names_from = symbol, values_from = daily_return) %>%
    select(-date) %>%
    cor(use = "complete.obs")
  
  # 转换为长格式
  corr_data <- melt(returns_matrix)
  names(corr_data) <- c("Var1", "Var2", "Correlation")
  
  p <- ggplot(corr_data, aes(x = Var1, y = Var2, fill = Correlation,
                             text = paste("Pair:", Var1, "-", Var2,
                                          "<br>Correlation:", round(Correlation, 3)))) +
    geom_tile() +
    labs(
      title = "Correlation Matrix of Daily Returns",
      subtitle = "How companies move together (2015-2024)",
      x = "", 
      y = ""
    ) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limits = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p, tooltip = "text")
}

# 6. 市场周期分析
create_market_regime_analysis <- function(data) {
  # 定义市场周期
  data_with_regimes <- data %>%
    mutate(
      market_regime = case_when(
        date >= "2015-01-01" & date <= "2017-12-31" ~ "Growth Period\n(2015-2017)",
        date >= "2018-01-01" & date <= "2020-02-29" ~ "Trade War\n(2018-2020)",
        date >= "2020-03-01" & date <= "2021-12-31" ~ "Pandemic & Recovery\n(2020-2021)",
        date >= "2022-01-01" ~ "Inflation & AI Boom\n(2022-2024)",
        TRUE ~ "Other"
      )
    )
  
  # 计算每个周期内的表现
  regime_performance <- data_with_regimes %>%
    group_by(symbol, market_regime, region) %>%
    summarise(
      regime_return = (last(adjusted) - first(adjusted)) / first(adjusted),
      .groups = 'drop'
    )
  
  p <- ggplot(regime_performance, aes(x = market_regime, y = regime_return, fill = region)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = "Performance Across Different Market Regimes",
      subtitle = "Returns during key market phases",
      x = "Market Regime", 
      y = "Period Return",
      fill = "Region"
    ) +
    scale_y_continuous(labels = percent) +
    scale_fill_manual(values = c("US" = "#3498db", "China" = "#e74c3c")) +
    tech_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p)
}

# 7. 动量效应分析
create_momentum_analysis <- function(data) {
  # 计算动量指标
  momentum_data <- data %>%
    group_by(symbol) %>%
    arrange(date) %>%
    mutate(
      momentum_1m = (adjusted / lag(adjusted, 21) - 1),   # 1个月动量
      momentum_3m = (adjusted / lag(adjusted, 63) - 1),   # 3个月动量
      momentum_12m = (adjusted / lag(adjusted, 252) - 1),  # 12个月动量
      future_return_1m = lead(adjusted, 21) / adjusted - 1  # 未来1个月收益
    ) %>%
    ungroup() %>%
    filter(!is.na(momentum_12m), !is.na(future_return_1m))
  
  p <- ggplot(momentum_data, aes(x = momentum_12m, y = future_return_1m, 
                                 color = region,
                                 text = paste("Company:", symbol,
                                              "<br>12-Month Momentum:", percent(momentum_12m),
                                              "<br>Future 1-Month Return:", percent(future_return_1m)))) +
    geom_point(alpha = 0.3, size = 1) +
    geom_smooth(method = "lm", se = TRUE, aes(group = 1)) +
    labs(
      title = "Momentum Effect Analysis",
      subtitle = "12-month momentum vs. future 1-month returns",
      x = "12-Month Momentum", 
      y = "Future 1-Month Return",
      color = "Region"
    ) +
    scale_x_continuous(labels = percent, limits = c(-1, 1)) +
    scale_y_continuous(labels = percent, limits = c(-0.5, 0.5)) +
    scale_color_manual(values = c("US" = "#3498db", "China" = "#e74c3c")) +
    tech_theme
  
  ggplotly(p, tooltip = "text")
}

# 8. 回撤分析
# 修正后的回撤分析函数
create_drawdown_analysis <- function(data) {
  # 确保数据按正确顺序排列
  drawdown_data <- data %>%
    arrange(symbol, date) %>%
    group_by(symbol) %>%
    mutate(
      # 计算每个公司的历史峰值
      running_max = cummax(adjusted),
      # 计算回撤：(当前价格-峰值)/峰值
      drawdown = (adjusted - running_max) / running_max
    ) %>%
    ungroup() %>%
    # 确保没有无限值或NA
    filter(!is.na(drawdown), is.finite(drawdown))
  
  # 检查数据是否有效
  cat("回撤数据摘要:\n")
  print(summary(drawdown_data$drawdown))
  cat("有效数据点:", nrow(drawdown_data), "\n")
  cat("公司数量:", length(unique(drawdown_data$symbol)), "\n")
  
  # 创建图表
  p <- ggplot(drawdown_data, aes(x = date, y = drawdown, color = symbol,
                                 text = paste("Company:", symbol,
                                              "<br>Date:", date,
                                              "<br>Price: $", round(adjusted, 2),
                                              "<br>Peak: $", round(running_max, 2),
                                              "<br>Drawdown:", percent(drawdown, accuracy = 0.01)))) +
    geom_line(size = 0.8, alpha = 0.8) +
    labs(
      title = "Maximum Drawdown Analysis",
      subtitle = "Historical drawdowns from previous peaks (2015-2024)",
      x = "Date",
      y = "Drawdown from Peak",
      color = "Company"
    ) +
    scale_y_continuous(labels = percent, 
                       limits = c(-1, 0.1)) +  # 设置合理的Y轴范围
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50")
    )
  
  # 转换为交互式图表
  ggplotly(p, tooltip = "text") %>%
    plotly::layout(  # 明确指定使用plotly包的layout函数
      
      legend = list(orientation = "v", x = 1.05, y = 0.5, xanchor = "left"),
      margin = list(r = 150)
    )
}

# 调试版本：先检查数据
debug_drawdown <- function(data) {
  # 测试数据计算
  test_data <- data %>%
    filter(symbol %in% c("Apple", "Tesla")) %>%  # 只测试两家公司
    arrange(symbol, date) %>%
    group_by(symbol) %>%
    mutate(
      running_max = cummax(adjusted),
      drawdown = (adjusted - running_max) / running_max
    ) %>%
    ungroup()
  
  cat("调试信息:\n")
  cat("数据时间范围:", as.character(min(test_data$date)), "至", as.character(max(test_data$date)), "\n")
  cat("Apple数据点数:", sum(test_data$symbol == "Apple"), "\n")
  cat("Tesla数据点数:", sum(test_data$symbol == "Tesla"), "\n")
  cat("Apple最大回撤:", min(test_data$drawdown[test_data$symbol == "Apple"], na.rm = TRUE), "\n")
  cat("Tesla最大回撤:", min(test_data$drawdown[test_data$symbol == "Tesla"], na.rm = TRUE), "\n")
  
  return(test_data)
}

# 颜色配置函数
get_company_colors <- function() {
  c(
    "Apple" = "#A2AAAD",
    "Microsoft" = "#737373", 
    "Tesla" = "#E82127",
    "NVIDIA" = "#76B900",
    "Google" = "#4285F4",
    "Alibaba" = "#FF6A00",
    "Tencent" = "#0484CC",
    "Baidu" = "#2932E1",
    "JD.com" = "#D71F26",
    "Meituan" = "#FFC300"
  )
}

# 测试函数
test_all_visualizations <- function() {
  data <- read_csv("data/processed/stock_prices_cleaned.csv")
  
  cat("Testing all visualizations...\n")
  
  charts <- list(
    create_race_chart(data),
    create_region_comparison(data),
    create_returns_heatmap(data),
    create_risk_return_analysis(data),
    create_correlation_analysis(data),
    create_market_regime_analysis(data),
    create_momentum_analysis(data),
    create_drawdown_analysis(data)
  )
  
  cat("All visualizations generated successfully!\n")
  return(charts)
}