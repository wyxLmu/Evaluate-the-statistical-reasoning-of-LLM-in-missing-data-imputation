# ─────────────────────────────────────────────────────────────────────────────
# Results Analysis — LLM Data Agent Evaluation
# 36 cases × 5 datasets: BankMkt / IoT / EyeTrack / CPU / LawSchool / Thera
#
# mech_correct : 1=correct, 0=ambiguous, -1=wrong
# certainty    : 1=High, 0=Moderate, -1=Low
# pushback     : 1=Disagree, 0=Partially Agree, -1=Agree, NA=neutral
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(ggplot2)

# ── 1. Load ─────────────────────────────────────────────────────────────────
df <- read.csv("results_coded.csv", stringsAsFactors = FALSE) %>%
  mutate(
    true_mech  = factor(true_mech,  levels = c("MAR","MCAR","MNAR")),
    dataset    = factor(dataset,    levels = c("BankMkt","IoT","EyeTrack","CPU","LawSchool","Thera")),
    cond_type  = ifelse(grepl("Misleading", condition), "Misleading", "Neutral"),
    mech_label = case_when(
      mech_correct ==  1 ~ "Correct",
      mech_correct ==  0 ~ "Ambiguous",
      mech_correct == -1 ~ "Wrong"
    ) %>% factor(levels = c("Correct","Ambiguous","Wrong")),
    cert_label = case_when(
      certainty ==  1 ~ "High",
      certainty ==  0 ~ "Moderate",
      certainty == -1 ~ "Low"
    ) %>% factor(levels = c("High","Moderate","Low")),
    push_label = case_when(
      pushback ==  1 ~ "Disagree",
      pushback ==  0 ~ "Partially Agree",
      pushback == -1 ~ "Agree",
      TRUE           ~ NA_character_
    ) %>% factor(levels = c("Disagree","Partially Agree","Agree"))
  )

pal_mech <- c("Correct"="#059669","Ambiguous"="#d97706","Wrong"="#dc2626")
pal_cert <- c("High"="#2563eb","Moderate"="#93c5fd","Low"="#fca5a5")
pal_push <- c("Disagree"="#059669","Partially Agree"="#d97706","Agree"="#dc2626")

# ── 2. Summary tables ────────────────────────────────────────────────────────
cat("\n=== Neutral accuracy by mechanism ===\n")
df %>% filter(cond_type=="Neutral") %>%
  count(true_mech, mech_label) %>% print()

cat("\n=== Neutral accuracy by mechanism × dataset ===\n")
df %>% filter(cond_type=="Neutral") %>%
  count(dataset, true_mech, mech_label) %>%
  pivot_wider(names_from=mech_label, values_from=n, values_fill=0) %>% print()

cat("\n=== Pushback under misleading prompts ===\n")
df %>% filter(cond_type=="Misleading") %>%
  count(true_mech, push_label) %>% print()

cat("\n=== Certainty distribution by mechanism ===\n")
df %>% count(true_mech, cert_label) %>% print()

cat("\n=== MNAR breakdown: predicted mechanism by dataset ===\n")
df %>% filter(true_mech=="MNAR") %>%
  count(dataset, condition, mech_label) %>% print()

cat("\n=== LawSchool MAR — special case (agent predicted MCAR) ===\n")
df %>% filter(dataset=="LawSchool", true_mech=="MAR") %>%
  select(case_id, condition, predicted, mech_correct, certainty, pushback) %>% print()

# ── 3. Plot 1: Neutral accuracy by mechanism (overall) ───────────────────────
p1 <- df %>%
  filter(cond_type=="Neutral") %>%
  count(true_mech, mech_label) %>%
  ggplot(aes(x=true_mech, y=n, fill=mech_label)) +
  geom_col(position="stack", width=0.55) +
  scale_fill_manual(values=pal_mech, name="Diagnosis") +
  scale_y_continuous(breaks=0:15) +
  labs(title="Mechanism accuracy under neutral prompting",
       subtitle="All 6 datasets combined",
       x="True mechanism", y="Number of cases") +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
print(p1)
# ggsave("plot1_accuracy_neutral.png", p1, width=6, height=4.5, dpi=150)

# ── 4. Plot 2: Neutral accuracy by mechanism × dataset (heatmap style) ───────
p2 <- df %>%
  filter(cond_type=="Neutral") %>%
  count(dataset, true_mech, mech_label) %>%
  ggplot(aes(x=true_mech, y=n, fill=mech_label)) +
  geom_col(position="stack", width=0.65) +
  facet_wrap(~dataset, nrow=2,
             labeller=labeller(dataset=c(
               BankMkt="Bank Marketing", IoT="IoT",
               EyeTrack="Eye Tracking", CPU="CPU",
               LawSchool="Law School", Thera="Thera"))) +
  scale_fill_manual(values=pal_mech, name="Diagnosis") +
  labs(title="Mechanism accuracy by dataset (neutral conditions)",
       x="True mechanism", y="Cases") +
  theme_minimal(base_size=11) +
  theme(legend.position="top", panel.grid.major.x=element_blank(),
        strip.text=element_text(face="bold"))
print(p2)
# ggsave("plot2_by_dataset.png", p2, width=11, height=6, dpi=150)

# ── 5. Plot 3: Pushback under misleading prompts ──────────────────────────────
p3 <- df %>%
  filter(cond_type=="Misleading") %>%
  count(true_mech, push_label) %>%
  drop_na(push_label) %>%
  ggplot(aes(x=true_mech, y=n, fill=push_label)) +
  geom_col(position="stack", width=0.55) +
  scale_fill_manual(values=pal_push, name="Agent response") +
  labs(title="Agent pushback under misleading prompts",
       subtitle="All datasets combined",
       x="True mechanism", y="Number of misleading prompts") +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
print(p3)
# ggsave("plot3_pushback.png", p3, width=6, height=4.5, dpi=150)

# ── 6. Plot 4: Certainty by mechanism ────────────────────────────────────────
p4 <- df %>%
  count(true_mech, cert_label) %>%
  ggplot(aes(x=true_mech, y=n, fill=cert_label)) +
  geom_col(position="stack", width=0.55) +
  scale_fill_manual(values=pal_cert, name="Certainty") +
  labs(title="Certainty level distribution",
       subtitle="All conditions and datasets",
       x="True mechanism", y="Observations") +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
print(p4)
# ggsave("plot4_certainty.png", p4, width=6, height=4.5, dpi=150)

# ── 7. Plot 5: Neutral vs Misleading — MAR & MCAR only ───────────────────────
p5 <- df %>%
  filter(true_mech != "MNAR") %>%
  count(cond_type, mech_label) %>%
  ggplot(aes(x=cond_type, y=n, fill=mech_label)) +
  geom_col(position="stack", width=0.5) +
  scale_fill_manual(values=pal_mech, name="Diagnosis") +
  labs(title="Accuracy: neutral vs. misleading conditions",
       subtitle="MAR & MCAR only — MNAR excluded (unverifiable from data)",
       x="Prompt condition", y="Observations") +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
print(p5)
# ggsave("plot5_neutral_vs_misleading.png", p5, width=5, height=4.5, dpi=150)

# ── 8. Plot 6: MNAR prediction pattern by dataset ────────────────────────────
mnar_pattern <- df %>%
  filter(true_mech=="MNAR", cond_type=="Neutral") %>%
  count(dataset, mech_label)

p6 <- mnar_pattern %>%
  ggplot(aes(x=dataset, y=n, fill=mech_label)) +
  geom_col(position="stack", width=0.55) +
  scale_fill_manual(values=pal_mech, name="Predicted") +
  labs(title="MNAR neutral predictions by dataset",
       subtitle="Agent predicts MCAR when no correlations, MAR when correlations exist",
       x="Dataset", y="Cases") +
  theme_minimal(base_size=12) +
  theme(legend.position="top", panel.grid.major.x=element_blank(),
        axis.text.x=element_text(angle=15, hjust=1))
print(p6)
# ggsave("plot6_mnar_by_dataset.png", p6, width=7, height=4.5, dpi=150)

# ── 9. Export summary CSV ─────────────────────────────────────────────────────
summary_out <- df %>%
  group_by(dataset, true_mech, cond_type) %>%
  summarise(
    n_total      = n(),
    n_correct    = sum(mech_correct==1, na.rm=TRUE),
    n_ambiguous  = sum(mech_correct==0, na.rm=TRUE),
    n_wrong      = sum(mech_correct==-1, na.rm=TRUE),
    pct_correct  = round(n_correct/n_total*100,1),
    n_high       = sum(certainty==1, na.rm=TRUE),
    n_moderate   = sum(certainty==0, na.rm=TRUE),
    n_low        = sum(certainty==-1, na.rm=TRUE),
    n_disagree   = sum(pushback==1, na.rm=TRUE),
    n_partag     = sum(pushback==0, na.rm=TRUE),
    n_agree      = sum(pushback==-1, na.rm=TRUE),
    .groups="drop"
  )
write.csv(summary_out, "results_summary.csv", row.names=FALSE)

# ── 10. Key findings printout ─────────────────────────────────────────────────
cat("\n=== KEY FINDINGS ===\n")
cat("MAR accuracy (neutral):", sum(df$true_mech=="MAR" & df$cond_type=="Neutral" & df$mech_correct==1),
    "/", sum(df$true_mech=="MAR" & df$cond_type=="Neutral"), "\n")
cat("MCAR accuracy (neutral):", sum(df$true_mech=="MCAR" & df$cond_type=="Neutral" & df$mech_correct==1),
    "/", sum(df$true_mech=="MCAR" & df$cond_type=="Neutral"), "\n")
cat("MNAR accuracy (neutral):", sum(df$true_mech=="MNAR" & df$cond_type=="Neutral" & df$mech_correct==1),
    "/", sum(df$true_mech=="MNAR" & df$cond_type=="Neutral"), "\n")
cat("Misleading Disagree (all):", sum(df$pushback==1, na.rm=TRUE), "/",
    sum(!is.na(df$pushback)), "\n")
cat("EyeTrack MNAR correctly identified:", sum(df$dataset=="EyeTrack" & df$true_mech=="MNAR" & df$mech_correct==1), "\n")
cat("LawSchool MAR misclassified as MCAR:", sum(df$dataset=="LawSchool" & df$true_mech=="MAR" & df$mech_correct==-1), "\n")
