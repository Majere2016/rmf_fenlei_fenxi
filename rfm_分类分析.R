library(dplyr)

options(scipen = 999)
library(sqldf)

# 假设 df_bravo_order_info 是订单信息表，包含 dt、register_time 和 status 列


df_bravo_payment <- sqldf("SELECT * 
          FROM bravo_order_info
          WHERE status IN ('订单完成') 
          AND julianday(ts) - julianday(dt) <= 30")
# 计算 recency (最小付费时间差)
recency <- df_bravo_payment %>%
  group_by(account_id) %>%
  summarize(recency = as.numeric(difftime(min(first_pay_time), min(register_time), units = "days")))

# 计算 frequency (付费频次)
frequency <- df_bravo_payment %>%
  group_by(account_id) %>%
  summarize(frequency = n_distinct(dt))

# 计算 monetary (总付费金额、最大付费金额和平均付费金额)
monetary <- df_bravo_payment %>%
  group_by(account_id) %>%
  summarize(
    total_amount = sum(pay_amount),
    max_amount = max(pay_amount),
    avg_amount = mean(pay_amount)
  )

# 合并 RFM 指标
df_rfm <- recency %>%
  left_join(frequency, by = "account_id") %>%
  left_join(monetary, by = "account_id")

# 设置 RFM 分位数和标签
r_levels <- c("小R", "中R", "大R", "超R")
quantile_recency <- quantile(df_rfm$recency, probs = c(0.25, 0.5, 0.75, 1))
quantile_frequency <- quantile(df_rfm$frequency, probs = c(0.25, 0.5, 0.75, 1))
quantile_total_amount <- quantile(df_rfm$total_amount, probs = c(0.25, 0.5, 0.75, 1))
quantile_max_amount <- quantile(df_rfm$max_amount, probs = c(0.25, 0.5, 0.75, 1))
quantile_avg_amount <- quantile(df_rfm$avg_amount, probs = c(0.25, 0.5, 0.75, 1))

# 根据 RFM 模型进行分类
df_rfm <- df_rfm %>%
  mutate(
    r_segment = findInterval(recency, quantile_recency, all.inside = TRUE) + 1,
    f_segment = findInterval(frequency, quantile_frequency, all.inside = TRUE) + 1,
    m_segment = findInterval(max_amount, quantile_max_amount, all.inside = TRUE) + 1,
    avg_segment = findInterval(avg_amount, quantile_avg_amount, all.inside = TRUE) + 1,
    sum_amount = findInterval(total_amount,quantile_total_amount,all.inside = TRUE) + 1,
    rfm_segment = paste(r_levels[r_segment], r_levels[f_segment], r_levels[m_segment], r_levels[avg_segment], r_levels[sum_amount],sep = "_")
  )




library(stringr)

library(dplyr)

# 假设 df_rfm 是您计算出的包含 rfm_segment 的数据框

# 计算每个用户的分类关键字频次
df_rfm <- df_rfm %>%
  mutate(
    user_category = str_extract_all(rfm_segment, "(大|小|中|超)") %>%
      sapply(function(x) ifelse(length(x) >= 2, paste(unique(x), collapse = ""), ""))
  )



library(ggplot2)

# 假设 df_rfm 是您计算出的包含 user_category 的数据框

# 统计每个分类的数量
category_counts <- table(df_rfm$user_category)

# 转换为数据框
category_counts_df <- data.frame(user_category = names(category_counts),
                                 count = as.numeric(category_counts))

# 绘制柱形图
ggplot(category_counts_df, aes(x = user_category, y = count, fill = user_category)) +
  geom_bar(stat = "identity") +
  labs(title = "用户分类分布情况", x = "用户分类", y = "数量") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(openxlsx)



# 将DataFrame保存为Excel文件
write.xlsx(df_rfm, "/Users/mazhecheng/Desktop/ifungames/20230811/df_rfm.xlsx", row.names = FALSE)

cat("DataFrame saved as","/Users/mazhecheng/Desktop/ifungames/20230811/df_rfm.xlsx, "\n")


