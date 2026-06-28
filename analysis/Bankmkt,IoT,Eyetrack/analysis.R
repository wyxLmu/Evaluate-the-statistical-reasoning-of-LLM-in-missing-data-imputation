# ─────────────────────────────────────────────────────────────────
# Results Analysis — LLM Data Agent Evaluation
# Datasets: Bank Marketing / IoT / EyeTracking  (cases 001–018)
#
# mech_correct : 1 = correct, 0 = ambiguous, -1 = wrong
# certainty    : 1 = High,    0 = Moderate,  -1 = Low
# pushback     : 1 = Disagree, 0 = Partially Agree, -1 = Agree
#                NA = neutral condition (not applicable)
# ─────────────────────────────────────────────────────────────────

library(tidyverse)
library(ggplot2)

# ── 1. Load ────────────────────────────────────────────────────
df <- read.csv("results_coded.csv", stringsAsFactors = FALSE) %>%
  mutate(
    true_mech  = factor(true_mech,  levels = c("MAR","MCAR","MNAR")),
    dataset    = factor(dataset,    levels = c("BankMkt","IoT","EyeTrack")),
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

# ── 2. Summary tables ──────────────────────────────────────────
cat("\n=== Mechanism accuracy — Neutral conditions ===\n")
df %>% filter(cond_type=="Neutral") %>%
  count(true_mech, mech_label) %>% print()

cat("\n=== Pushback under Misleading prompts ===\n")
df %>% filter(cond_type=="Misleading") %>%
  count(true_mech, push_label) %>% print()

cat("\n=== Certainty by true mechanism ===\n")
df %>% count(true_mech, cert_label) %>% print()

cat("\n=== MNAR cases in detail ===\n")
df %>% filter(true_mech=="MNAR") %>%
  select(case_id, dataset, condition, predicted, mech_correct, certainty, pushback) %>%
  print()

# ── 3. Plot 1: Accuracy by mechanism — Neutral ─────────────────
p1 <- df %>%
  filter(cond_type=="Neutral") %>%
  count(true_mech, mech_label) %>%
  ggplot(aes(x=true_mech, y=n, fill=mech_label)) +
  geom_col(position="stack", width=0.55) +
  scale_fill_manual(values=pal_mech, name="Diagnosis") +
  scale_y_continuous(breaks=0:8) +
  labs(title="Mechanism accuracy under neutral prompting",
       subtitle="All three datasets combined · neutral conditions only",
       x="True mechanism", y="Number of cases") +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
print(p1)
# ggsave("plot1_accuracy_neutral.png", p1, width=6, height=4, dpi=150)

# ── 4. Plot 2: Accuracy by mechanism + dataset ─────────────────
p2 <- df %>%
  count(dataset, true_mech, mech_label) %>%
  ggplot(aes(x=true_mech, y=n, fill=mech_label)) +
  geom_col(position="stack", width=0.6) +
  facet_wrap(~dataset,
             labeller=labeller(dataset=c(BankMkt="Bank Marketing",
                                          IoT="IoT",
                                          EyeTrack="Eye Tracking"))) +
  scale_fill_manual(values=pal_mech, name="Diagnosis") +
  labs(title="Mechanism accuracy by dataset",
       subtitle="All conditions combined",
       x="True mechanism", y="Number of observations") +
  theme_minimal(base_size=12) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
print(p2)
# ggsave("plot2_by_dataset.png", p2, width=9, height=4, dpi=150)

# ── 5. Plot 3: Pushback rate ───────────────────────────────────
p3 <- df %>%
  filter(cond_type=="Misleading") %>%
  count(true_mech, push_label) %>%
  drop_na(push_label) %>%
  ggplot(aes(x=true_mech, y=n, fill=push_label)) +
  geom_col(position="stack", width=0.55) +
  scale_fill_manual(values=pal_push, name="Agent response") +
  scale_y_continuous(breaks=0:12) +
  labs(title="Agent pushback under misleading prompts",
       x="True mechanism", y="Number of misleading prompts") +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
print(p3)
# ggsave("plot3_pushback.png", p3, width=6, height=4, dpi=150)

# ── 6. Plot 4: Certainty distribution ─────────────────────────
p4 <- df %>%
  count(true_mech, cert_label) %>%
  ggplot(aes(x=true_mech, y=n, fill=cert_label)) +
  geom_col(position="stack", width=0.55) +
  scale_fill_manual(values=pal_cert, name="Certainty") +
  labs(title="Certainty level distribution",
       subtitle="All conditions",
       x="True mechanism", y="Number of observations") +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
print(p4)
# ggsave("plot4_certainty.png", p4, width=6, height=4, dpi=150)

# ── 7. Plot 5: Neutral vs Misleading (MAR+MCAR only) ──────────
p5 <- df %>%
  filter(true_mech!="MNAR") %>%
  count(cond_type, mech_label) %>%
  ggplot(aes(x=cond_type, y=n, fill=mech_label)) +
  geom_col(position="stack", width=0.5) +
  scale_fill_manual(values=pal_mech, name="Diagnosis") +
  labs(title="Accuracy: neutral vs. misleading conditions",
       subtitle="MAR and MCAR only — MNAR excluded (unverifiable from data alone)",
       x="Prompt condition", y="Number of observations") +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
print(p5)
# ggsave("plot5_neutral_vs_misleading.png", p5, width=5, height=4, dpi=150)

# ── 8. Plot 6: MNAR deep-dive by dataset ──────────────────────
# ★ Key finding: EyeTrack Misleading1 correctly identified MNAR
p6 <- df %>%
  filter(true_mech=="MNAR") %>%
  mutate(cond_short = case_when(
    condition=="Neutral"         ~ "Neutral",
    grepl("Misleading1", condition) ~ "Misleading1\n(analyst: MAR)",
    grepl("Misleading2", condition) ~ "Misleading2\n(analyst: MCAR)"
  )) %>%
  count(dataset, cond_short, mech_label) %>%
  ggplot(aes(x=cond_short, y=n, fill=mech_label)) +
  geom_col(position="stack", width=0.6) +
  facet_wrap(~dataset,
             labeller=labeller(dataset=c(BankMkt="Bank Marketing",
                                          IoT="IoT",
                                          EyeTrack="Eye Tracking ★"))) +
  scale_fill_manual(values=pal_mech, name="Diagnosis") +
  labs(title="MNAR cases: diagnosis by dataset and condition",
       subtitle="★ EyeTrack Misleading1: agent correctly identified MNAR using domain reasoning",
       x="Condition", y="Number of cases") +
  theme_minimal(base_size=11) +
  theme(legend.position="top", panel.grid.major.x=element_blank(),
        strip.text=element_text(face="bold"))
print(p6)
# ggsave("plot6_mnar_detail.png", p6, width=9, height=4, dpi=150)

# ── 9. Export summary CSV ──────────────────────────────────────
summary_out <- df %>%
  group_by(dataset, true_mech, cond_type) %>%
  summarise(
    n_total      = n(),
    n_correct    = sum(mech_correct==1, na.rm=TRUE),
    n_ambiguous  = sum(mech_correct==0, na.rm=TRUE),
    n_wrong      = sum(mech_correct==-1, na.rm=TRUE),
    pct_correct  = round(n_correct/n_total*100,1),
    n_high_cert  = sum(certainty==1, na.rm=TRUE),
    n_mod_cert   = sum(certainty==0, na.rm=TRUE),
    n_low_cert   = sum(certainty==-1, na.rm=TRUE),
    n_disagree   = sum(pushback==1, na.rm=TRUE),
    n_part_agree = sum(pushback==0, na.rm=TRUE),
    n_agree      = sum(pushback==-1, na.rm=TRUE),
    .groups="drop"
  )

write.csv(summary_out, "results_summary.csv", row.names=FALSE)
cat("\nSummary saved to results_summary.csv\n")
cat("\n=== Key finding: MNAR correct identifications ===\n")
df %>% filter(true_mech=="MNAR", mech_correct==1) %>%
  select(case_id, dataset, condition, predicted, certainty, pushback) %>%
  print()
