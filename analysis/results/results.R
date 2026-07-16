# ==========================================
# 自动化实证数据分析：MNAR 盲区 (动态计算版)
# ==========================================

# 1. 加载必要的包
# install.packages(c("ggplot2", "dplyr", "readxl", "scales"))
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)

# 2. 动态读取并清洗数据
# 请确保文件名与你电脑中的完全一致 (注意文件名中的空格)
file_name <- "results_coded_full .xlsx"
df <- read_excel(file_name, sheet = 1)

# 筛选中立条件 (Neutral)，并统计各个机制的准确率
stats_df <- df %>%
  filter(grepl("Neutral", condition, ignore.case = TRUE)) %>%
  group_by(true_mech) %>%
  summarise(
    Total = n(),
    Correct = sum(mech_correct == 1, na.rm = TRUE),
    Accuracy = Correct / Total,
    .groups = 'drop'
  ) %>%
  # 确保顺序按照 MCAR, MAR, MNAR 排列
  mutate(true_mech = factor(true_mech, levels = c("MCAR", "MAR", "MNAR")))

# 3. 动态计算 95% 精确置信区间 (Clopper-Pearson)
# 利用 rowwise 针对每一行运行 binom.test
stats_df <- stats_df %>%
  rowwise() %>%
  mutate(
    CI_Lower = binom.test(Correct, Total)$conf.int[1],
    CI_Upper = binom.test(Correct, Total)$conf.int[2]
  ) %>%
  ungroup()

print("--- 准确率与 95% 置信区间统计 ---")
print(stats_df)

# 4. 动态计算 Fisher 精确检验
# 将 MCAR 和 MAR 视为“可观测机制 (Observable)”，MNAR 视为不可观测
mnar_correct <- stats_df$Correct[stats_df$true_mech == "MNAR"]
mnar_wrong <- stats_df$Total[stats_df$true_mech == "MNAR"] - mnar_correct

obs_correct <- sum(stats_df$Correct[stats_df$true_mech %in% c("MCAR", "MAR")])
obs_wrong <- sum(stats_df$Total[stats_df$true_mech %in% c("MCAR", "MAR")]) - obs_correct

# 构建 2x2 列联表
fisher_matrix <- matrix(c(mnar_correct, mnar_wrong, obs_correct, obs_wrong), 
                        nrow = 2, byrow = TRUE)
fisher_res <- fisher.test(fisher_matrix)
p_value <- fisher_res$p.value

# 格式化 p 值用于显示
p_text <- ifelse(p_value < 0.0001, "p < 0.0001 ***", sprintf("p = %.4f", p_value))
print(paste("Fisher Test Result:", p_text))

# 5. 生成专业级带误差棒的可视化图表
p <- ggplot(stats_df, aes(x = true_mech, y = Accuracy, fill = true_mech)) +
  geom_col(color = "black", width = 0.6, size = 0.8) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.15, size = 1) +
  geom_text(aes(label = sprintf("%.0f%%\n(%d/%d)", Accuracy * 100, Correct, Total), 
                y = Accuracy + 0.08), size = 4.5, fontface = "bold", lineheight = 0.9) +
  scale_fill_manual(values = c("MCAR" = "#4C72B0", "MAR" = "#55A868", "MNAR" = "#C44E52")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.3)) +
  theme_classic(base_size = 14) +
  labs(
    title = "Figure 1: The MNAR Blindspot in LLM Data Agents",
    subtitle = "Accuracy rates with 95% Exact CIs and Fisher's Exact Test",
    x = "True Missing Mechanism",
    y = "Identification Accuracy"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30", margin = margin(b = 15)),
    axis.text.x = element_text(face = "bold", size = 12)
  )

# 6. 添加 Fisher 检验的动态显著性支架
p <- p +
  annotate("segment", x = 1.5, xend = 3, y = 1.20, yend = 1.20, size = 0.8) + 
  annotate("segment", x = 1.5, xend = 1.5, y = 1.17, yend = 1.20, size = 0.8) + 
  annotate("segment", x = 3, xend = 3, y = max(stats_df$CI_Upper[stats_df$true_mech == "MNAR"]) + 0.08, yend = 1.20, size = 0.8) + 
  annotate("text", x = 2.25, y = 1.24, label = paste("Fisher's Exact Test:", p_text), 
           size = 4.5, fontface = "italic")

# 如果 MNAR 准确率依然很低，添加结构性失效文本提示
if(mnar_correct == 0){
  p <- p + annotate("text", x = 3, y = max(stats_df$CI_Upper[stats_df$true_mech == "MNAR"]) + 0.03, 
                    label = "Structural\nFailure", color = "#C44E52", fontface = "bold", size = 4)
}

# 显示图表
print(p)

# ggsave("Figure1_MNAR_Dynamic.pdf", plot = p, width = 8, height = 6, device = "pdf")




# 2   论点二 (抗压韧性与信号强度)    筛选出误导性条件 (Misleading)
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(broom) 


df_misleading <- df %>%
  filter(grepl("Misleading", condition, ignore.case = TRUE)) %>%
  mutate(
    # 将 pushback 转化为二分类变量 (1 = Disagree/反驳, 0 = Agree/顺从)
    is_pushback = ifelse(pushback == 1, 1, 0),
    # 格式化缺失率为因子，方便分组可视化
    missing_rate = factor(missing_rate, levels = c(10, 30), labels = c("10% Missing", "30% Missing")),
    # 固定机制的排序
    true_mech = factor(true_mech, levels = c("MCAR", "MAR", "MNAR"))
  )

# ==========================================
# [统计检验] 逻辑回归分析 (Logistic Regression)
# ==========================================
cat("=== 逻辑回归模型结果 (因变量: 是否反驳) ===\n")
# 运行 GLM，检验机制和缺失率对反驳率的影响
model_pushback <- glm(is_pushback ~ true_mech + missing_rate, 
                      data = df_misleading, 
                      family = binomial(link = "logit"))

# 打印规范的回归系数和 p 值
print(tidy(model_pushback))
cat("-------------------------------------------\n")

# ==========================================
# [图表绘制] 数据聚合与可视化
# ==========================================
# 计算各组的反驳率及标准误 (Standard Error)
plot_data <- df_misleading %>%
  group_by(true_mech, missing_rate) %>%
  summarise(
    Total = n(),
    Pushback_Count = sum(is_pushback, na.rm = TRUE),
    Pushback_Rate = Pushback_Count / Total,
    # 计算标准误用于绘制误差棒
    SE = sqrt((Pushback_Rate * (1 - Pushback_Rate)) / Total),
    .groups = 'drop'
  ) %>%
  mutate(
    CI_Lower = pmax(0, Pushback_Rate - 1.96 * SE),
    CI_Upper = pmin(1, Pushback_Rate + 1.96 * SE)
  )

# 生成图表
p2 <- ggplot(plot_data, aes(x = true_mech, y = Pushback_Rate, fill = missing_rate)) +
  # 分组并排柱状图
  geom_col(position = position_dodge(width = 0.8), color = "black", width = 0.7, size = 0.8) +
  # 添加误差棒
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.8), width = 0.15, size = 0.8) +
  # 添加数据标签
  geom_text(aes(label = sprintf("%.0f%%", Pushback_Rate * 100)), 
            position = position_dodge(width = 0.8), vjust = -1.5, size = 4.5, fontface = "bold") +
  # 使用具有警示意味的颜色区分 10% 和 30% 缺失率
  scale_fill_manual(values = c("10% Missing" = "#F28E2B", "30% Missing" = "#E15759")) +
  # Y 轴设置为百分比
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.25)) +
  theme_classic(base_size = 14) +
  labs(
    title = "Figure 2: Robustness Over Sycophancy",
    subtitle = "Pushback Rate Driven by True Mechanism and Signal Strength",
    x = "True Missing Mechanism",
    y = "Pushback Rate (Resistance to Misleading Prompt)",
    fill = "Signal Strength"
  ) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30", margin = margin(b = 15)),
    axis.text.x = element_text(face = "bold", size = 12)
  )

# 显示图表
print(p2)

# ggsave("Figure2_Pushback_Logistic.pdf", plot = p2, width = 8, height = 6, device = "pdf")


# ==========================================
# 自动化实证数据分析：论点三 (认知过自信陷阱)
# ==========================================
library(ggplot2)
library(dplyr)
library(ggstatsplot) # 用于生成带有统计检验结果的图表

# 1. 读取数据
file_name <- "results_coded_full .xlsx"
df <- read_excel(file_name, sheet = 1)

# 2. 数据清洗与标注
# 确保 Certainty 被转化为数值，mech_correct 转化为二分类标签
df_certainty <- df %>%
  filter(!is.na(certainty)) %>%
  mutate(
    Correctness = ifelse(mech_correct == 1, "Correct ", "Incorrect "),
    # 确信度评分已在 excel 中量化（如 1-10）
  )

# 3. 运行 t 检验
t_test_result <- t.test(certainty ~ Correctness, data = df_certainty)
print(t_test_result)

# 4. 可视化：小提琴图 + 箱线图
p3 <- ggplot(df_certainty, aes(x = Correctness, y = certainty, fill = Correctness)) +
  # 小提琴图展示密度分布
  geom_violin(alpha = 0.3, trim = FALSE) +
  # 箱线图展示中位数与四分位距
  geom_boxplot(width = 0.1, color = "black", alpha = 0.8) +
  # 添加抖动点，查看样本分布
  geom_jitter(width = 0.1, alpha = 0.2) +
  scale_fill_manual(values = c("Correct" = "#0072B2", 
                               "Incorrect" = "#E69F00")) +
  theme_classic(base_size = 14) +
  labs(
    title = "Figure 3: The Overconfidence Trap",
    subtitle = sprintf("Comparison of Certainty Scores (t-test p = %.4f)", t_test_result$p.value),
    x = "Prediction Performance",
    y = "Agent Certainty Score "
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30")
  )

# 显示图表
print(p3)

# ggsave("Figure3_Certainty_Trap.pdf", plot = p3, width = 8, height = 6)


  
  
  
  
  
  # ==========================================
  # 最终版 Figure 3：带 T 检验标注的可视化
  # ==========================================
  library(ggplot2)
  library(dplyr)
  library(readxl)
  
  # 1. 读取与清洗数据
  file_name <- "results_coded_full .xlsx"
  df <- read_excel(file_name, sheet = 1)
  
  df_certainty <- df %>%
    filter(!is.na(certainty)) %>%
    mutate(
      Correctness = ifelse(mech_correct == 1, "Correct", "Incorrect")
    )
  
  # 2. 生成学术级分面图
  p3_final <- ggplot(df_certainty, aes(x = Correctness, y = certainty, fill = Correctness)) +
    # 分面：按机制拆分
    facet_wrap(~true_mech) +
    # 使用小提琴图 + 箱线图组合
    geom_violin(alpha = 0.2, trim = FALSE, adjust = 1) +
    geom_boxplot(width = 0.15, color = "black", alpha = 0.7, outlier.shape = NA) +
    scale_fill_manual(values = c("Correct" = "#0072B2", "Incorrect" = "#E69F00")) +
    theme_classic(base_size = 14) +
    labs(
      title = "Figure 3: The Overconfidence Trap",
      x = "Prediction Performance",
      y = "Agent Certainty Score"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "black", face = "italic"), # 强调 T 检验结果
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(face = "bold", size = 12)
    )
  
  # 显示图表
  print(p3_final)
  
  # ggsave("Figure3_Final.pdf", plot = p3_final, width = 9, height = 6)
