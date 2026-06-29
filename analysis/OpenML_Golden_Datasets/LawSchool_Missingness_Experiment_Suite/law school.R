# =====================================================================
# Law School Admissions Dataset: Missingness Injection (Amputation)
# Target Variable: ugpa (Undergraduate GPA)
# Mechanisms: Scanner Glitch (MCAR), Age-driven (MAR), Shame/Stigma (MNAR)
# =====================================================================

# 1. Environment Setup: Install and load required packages

library(dplyr)
library(zip)

# =====================================================================
# 2. Read Data
# =====================================================================
file_path <- "43890_law_school_admission_bianry_data.csv"
if (!file.exists(file_path)) {
  stop("File not found! Please ensure the CSV is in the working directory.")
}
df <- read.csv(file_path, stringsAsFactors = FALSE)

# =====================================================================
# 3. Define Core Masking Function (Weight-based exact sampling)
# =====================================================================
generate_law_missing <- function(data, mechanism, missing_rate, seed) {
  set.seed(seed)
  df_masked <- data
  n_rows <- nrow(data)
  n_missing <- round(n_rows * missing_rate)
  target_col <- "ugpa"
  
  if (mechanism == "MCAR") {
    # Scenario 1: Optical Scanner Glitch (Uniform random probability)
    probs <- rep(1, n_rows) 
    
  } else if (mechanism == "MAR") {
    # Scenario 2: Memory Decay among Non-Traditional Students (Driver: Age)
    # Older students (age > 30) have a 5x higher chance of leaving GPA blank
    probs <- ifelse(data$age > 30, 5, 1)
    
  } else if (mechanism == "MNAR") {
    # Scenario 3: Systematic Shame / Imposter Syndrome (Driver: Low UGPA itself)
    # Students with bottom-tier GPA (< 2.8) have an 8x higher chance of hiding it
    probs <- ifelse(data$ugpa < 2.8, 8, 1)
  }
  
  # Execute weighted sampling without replacement to hit EXACT missing rates
  valid_indices <- 1:n_rows
  missing_indices <- sample(valid_indices, size = n_missing, replace = FALSE, prob = probs)
  
  # Apply masking: Set sampled rows to NA
  df_masked[[target_col]][missing_indices] <- NA
  
  return(list(data = df_masked, target = target_col))
}

# =====================================================================
# 4. Batch Generate 6 Control Datasets & Correlation Matrices
# =====================================================================
set_params <- list(
  list(name = "LawSchool_MCAR_10", mech = "MCAR", rate = 0.10, seed = 201),
  list(name = "LawSchool_MAR_10",  mech = "MAR",  rate = 0.10, seed = 202),
  list(name = "LawSchool_MNAR_10", mech = "MNAR", rate = 0.10, seed = 203),
  list(name = "LawSchool_MCAR_30", mech = "MCAR", rate = 0.30, seed = 204),
  list(name = "LawSchool_MAR_30",  mech = "MAR",  rate = 0.30, seed = 205),
  list(name = "LawSchool_MNAR_30", mech = "MNAR", rate = 0.30, seed = 206)
)

output_files <- c() # Track generated files for zipping

cat("Starting missing data injection and correlation computation...\n")

for (params in set_params) {
  name <- params$name
  
  # A. Generate masked data
  result <- generate_law_missing(df, params$mech, params$rate, params$seed)
  masked_data <- result$data
  target_col <- result$target
  
  # Export Dataset to CSV
  data_filename <- paste0(name, "_data.csv")
  write.csv(masked_data, data_filename, row.names = FALSE)
  output_files <- c(output_files, data_filename)
  
  # B. Calculate Missingness Correlation Matrix
  # Missingness indicator M (1 = missing, 0 = observed)
  M_indicator <- ifelse(is.na(masked_data[[target_col]]), 1, 0)
  
  # Extract all numeric columns to compute point-biserial correlation
  numeric_cols <- df %>% select_if(is.numeric)
  corr_matrix <- cor(numeric_cols, M_indicator, use = "pairwise.complete.obs")
  
  corr_df <- data.frame(
    Variable = rownames(corr_matrix),
    Correlation_with_M = round(as.numeric(corr_matrix), 4)
  )
  
  # =====================================================================
  # 🚨 THE SOCIOLOGICAL MNAR TRAP:
  # Forcefully remove the "ugpa" row if the mechanism is MNAR.
  # This mimics the real world where the hidden GPA cannot be analyzed.
  # If the LLM just looks at this matrix, it will incorrectly guess MCAR!
  # =====================================================================
  if (params$mech == "MNAR") {
    corr_df <- corr_df %>% filter(Variable != target_col)
  }
  
  # Save the correlation matrix
  corr_filename <- paste0(name, "_correlation_matrix.csv")
  write.csv(corr_df, corr_filename, row.names = FALSE)
  output_files <- c(output_files, corr_filename)
}

# =====================================================================
# 5. Zip Outputs and Cleanup Workspace
# =====================================================================
zip_filename <- "LawSchool_Missingness_Experiment_Suite.zip"

# Create zip file containing all 12 CSVs
zip::zip(zipfile = zip_filename, files = output_files)

# Remove the 12 individual CSVs to keep the local directory clean
file.remove(output_files)

cat("\n======================================================\n")
cat("✅ Law School Data Generation Successful!\n")
cat("Injected: [Scanner Error(MCAR) / Older Students(MAR) / Low GPA Shame(MNAR)]\n")
cat("6 datasets and 6 correlation matrices have been packed.\n")
cat("Output Zip Path:", file.path(getwd(), zip_filename), "\n")
cat("======================================================\n")

