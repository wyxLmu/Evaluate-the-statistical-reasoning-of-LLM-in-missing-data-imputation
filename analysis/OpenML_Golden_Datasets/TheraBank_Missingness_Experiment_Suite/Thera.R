# =====================================================================
# Thera Bank Dataset: Missingness Injection & Correlation Matrices
# Target Dataset: 43826_Personal_Loan_Modeling_data.csv
# Outputs: 6 Datasets, 6 Correlation Matrices, 1 Zip Archive
# =====================================================================

# 1. Environment Setup: Load required packages

library(dplyr)
library(zip)

# =====================================================================
# 2. Read and Preprocess Data
# =====================================================================
file_path <- "43826_Personal_Loan_Modeling_data.csv"
if (!file.exists(file_path)) {
  stop("File not found! Please ensure '43826_Personal_Loan_Modeling_data.csv' is in the working directory.")
}
df <- read.csv(file_path, stringsAsFactors = FALSE)

# =====================================================================
# 3. Define Core Masking Function (Weight-based exact sampling)
# =====================================================================
generate_thera_missing <- function(data, mechanism, missing_rate, seed) {
  set.seed(seed)
  df_masked <- data
  n_rows <- nrow(data)
  n_missing <- round(n_rows * missing_rate)
  
  if (mechanism == "MCAR") {
    # Target: ZIP_Code (Uniform random probability)
    target_col <- "ZIP_Code"
    probs <- rep(1, n_rows) 
    
  } else if (mechanism == "MAR") {
    # Target: CCAvg (Driver: Online status)
    target_col <- "CCAvg"
    # Offline customers have a 6x higher chance of missing data
    probs <- ifelse(data$Online == 0, 6, 1)
    
  } else if (mechanism == "MNAR") {
    # Target: Income (Driver: Income itself - U shape)
    target_col <- "Income"
    # Both low (<30) and high (>120) income earners have 8x higher chance of missingness
    probs <- ifelse(data$Income < 30 | data$Income > 120, 8, 1)
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
  list(name = "Thera_MCAR_10", mech = "MCAR", rate = 0.10, seed = 101),
  list(name = "Thera_MAR_10",  mech = "MAR",  rate = 0.10, seed = 102),
  list(name = "Thera_MNAR_10", mech = "MNAR", rate = 0.10, seed = 103),
  list(name = "Thera_MCAR_30", mech = "MCAR", rate = 0.30, seed = 104),
  list(name = "Thera_MAR_30",  mech = "MAR",  rate = 0.30, seed = 105),
  list(name = "Thera_MNAR_30", mech = "MNAR", rate = 0.30, seed = 106)
)

output_files <- c() # Track generated files for zipping

cat("Starting missing data injection and correlation computation...\n")

for (params in set_params) {
  name <- params$name
  
  # A. Generate masked data and extract target variable name
  result <- generate_thera_missing(df, params$mech, params$rate, params$seed)
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
  # We use the original 'df' to compute true correlations, revealing underlying mechanisms
  numeric_cols <- df %>% select_if(is.numeric)
  corr_matrix <- cor(numeric_cols, M_indicator, use = "pairwise.complete.obs")
  
  corr_df <- data.frame(
    Variable = rownames(corr_matrix),
    Correlation_with_M = round(as.numeric(corr_matrix), 4)
  )
  
  # =====================================================================
  # 🚨 THE MNAR TRAP:
  # Forcefully remove the target variable row if the mechanism is MNAR.
  # This mimics real-world scenarios where the unobserved values cannot 
  # be correlated, depriving the analyst/LLM of the ultimate ground truth.
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
zip_filename <- "TheraBank_Missingness_Experiment_Suite.zip"

# Create zip file containing all 12 CSVs
zip::zip(zipfile = zip_filename, files = output_files)

# Remove the 12 individual CSVs to keep the local directory clean (Optional)
file.remove(output_files)

cat("\n======================================================\n")
cat("✅ Thera Bank Data Generation Successful!\n")
cat("Successfully injected [API Fault(MCAR) / Offline Branch(MAR) / Privacy Shield(MNAR)].\n")
cat("6 control datasets and 6 correlation matrices have been packed.\n")
cat("Output Zip Path:", file.path(getwd(), zip_filename), "\n")
cat("======================================================\n")
