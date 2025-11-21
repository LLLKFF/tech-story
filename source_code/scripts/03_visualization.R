# scripts/03_visualization_enhanced.R
library(ggplot2)
library(plotly)
library(viridis)

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

# 1. 主要赛车图（累积收益对比）
create_race_chart <- function(data) {
  p <- ggplot(data, aes(x = date, y = cumulative_return, 
                        color = symbol, group = symbol,
                        text = paste("公司:", company_name,
                                     "<br>日期:", date,
                                     "<br>累积收益:", round(cumulative_return, 1), "%"))) +
    geom_line(size = 1.2, alpha = 0.8) +
    labs(
      title = "中美科技巨头十年表现对比",
      subtitle = "累积收益率 (2015年1月 = 100)",
      x = "日期",
      y = "累积收益率 (%)",
      color = "公司"
    ) +
    scale_color_viridis(discrete = TRUE) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    tech_theme
  
  ggplotly(p, tooltip = "text") %>%
    layout(legend = list(orientation = "h", x = 0.5, y = -0.1, xanchor = "center"))
}

# 2. 地区对比图（中美市场平均表现）
create_region_comparison <- function(data) {
  region_data <- data %>%
    group_by(region, date) %>%
    summarise(
      avg_cumulative_return = mean(cumulative_return, na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- ggplot(region_data, aes(x = date, y = avg_cumulative_return, 
                               color = region, fill = region)) +
    geom_line(size = 1.5) +
    geom_area(alpha = 0.3) +
    labs(
      title = "中美科技市场整体表现对比",
      subtitle = "地区平均累积收益率",
      x = "日期",
      y = "平均累积收益率 (%)",
      color = "地区",
      fill = "地区"
    ) +
    scale_color_manual(values = c("US" = "#1f77b4", "China" = "#ff7f0e")) +
    scale_fill_manual(values = c("US" = "#1f77b4", "China" = "#ff7f0e")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    tech_theme
  
  ggplotly(p)
}

# 3. 收益率分布图
create_returns_heatmap <- function(data) {
  yearly_returns <- data %>%
    mutate(year = year(date)) %>%
    group_by(symbol, company_name, year) %>%
    summarise(
      yearly_return = (last(adjusted) - first(adjusted)) / first(adjusted),
      .groups = "drop"
    )
  
  p <- ggplot(yearly_returns, aes(x = year, y = company_name, 
                                  fill = yearly_return,
                                  text = paste("公司:", company_name,
                                               "<br>年份:", year,
                                               "<br>年收益率:", percent(yearly_return)))) +
    geom_tile() +
    labs(
      title = "年度收益率热力图",
      x = "年份",
      y = "公司",
      fill = "年收益率"
    ) +
    scale_fill_gradient2(low = "red", mid = "white", high = "green",
                         midpoint = 0, labels = percent) +
    scale_x_continuous(breaks = seq(2015, 2024, 1)) +
    tech_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p, tooltip = "text")
}

# 测试函数
test_visualizations <- function() {
  data <- read_csv("data/processed/stock_prices_cleaned.csv")
  
  cat("生成累积收益对比图...\n")
  p1 <- create_race_chart(data)
  print(p1)
  
  cat("生成地区对比图...\n") 
  p2 <- create_region_comparison(data)
  print(p2)
  
  cat("生成收益率热力图...\n")
  p3 <- create_returns_heatmap(data)
  print(p3)
  
  return(TRUE)
}