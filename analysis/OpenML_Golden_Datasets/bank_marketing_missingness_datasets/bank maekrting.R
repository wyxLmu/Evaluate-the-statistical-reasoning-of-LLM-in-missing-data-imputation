# =====================================================================
# 1. Environment Setup: Install and load required R packages
# =====================================================================
if (!require("dplyr")) install.packages("dplyr")
if (!require("zip")) install.packages("zip") # Used for zipping files

library(dplyr)
library(zip)

# =====================================================================
# 2. Load Data and Restore Semantic Column Names
# =====================================================================
# Ensure this file is in your R working directory (check with getwd(), set with setwd())
df <- read.csv("44117_bank_marketing_data.csv", stringsAsFactors = FALSE)

# Restore anonymized columns (V1, V6, etc.) to meaningful business attributes
df <- df %>%
  rename(
    age = V1,
    balance = V6,
    day = V10,
    duration = V12,
    campaign = V13,
    pdays = V14,
    previous = V15,
    y = Class
  )

# =====================================================================
# 3. Define Strict Missingness Injection Function (Controlled Variables)
# =====================================================================
generate_missing <- function(data, target_col, mechanism, missing_rate, seed) {
  set.seed(seed)
  df_masked <- data
  
  # Get indices of rows with valid (non-NA) target values
  valid_indices <- which(!is.na(data[[target_col]]))
  n_missing <- round(length(valid_indices) * missing_rate)
  
  if (mechanism == "MCAR") {
    # Mechanism 1: MCAR (Equal probability for all valid rows)
    probs <- rep(1, length(valid_indices))
    
  } else if (mechanism == "MAR") {
    # Mechanism 2: MAR based on Age (Older = higher probability of missingness)
    # Scale age to [0, 1] and amplify the difference using an exponential function
    age_scaled <- (data$age[valid_indices] - min(data$age, na.rm=TRUE)) / 
      (max(data$age, na.rm=TRUE) - min(data$age, na.rm=TRUE))
    probs <- exp(age_scaled * 5)
    
  } else if (mechanism == "MNAR") {
    # Mechanism 3: MNAR based on Balance itself (Extreme values are more likely to be missing)
    # The further from the median (extreme rich or poor), the higher the weight (U-shaped distribution)
    median_val <- median(data[[target_col]], na.rm = TRUE)
    probs <- abs(data[[target_col]][valid_indices] - median_val)
    # Prevent zero-weight vector if all values are identical
    if (sum(probs) == 0) probs <- rep(1, length(valid_indices)) 
  }
  
  # Weighted sampling without replacement
  missing_indices <- sample(valid_indices, size = n_missing, replace = FALSE, prob = probs)
  
  # Apply mask (set to NA)
  df_masked[[target_col]][missing_indices] <- NA
  return(df_masked)
}

# =====================================================================
# 4. Batch Generate 6 Datasets and Correlation Matrices
# =====================================================================
# Define experimental group parameters
set_params <- list(
  list(name = "MCAR_10", mech = "MCAR", rate = 0.10, seed = 20261),
  list(name = "MAR_10",  mech = "MAR",  rate = 0.10, seed = 20262),
  list(name = "MNAR_10", mech = "MNAR", rate = 0.10, seed = 20263),
  list(name = "MCAR_30", mech = "MCAR", rate = 0.30, seed = 20264),
  list(name = "MAR_30",  mech = "MAR",  rate = 0.30, seed = 20265),
  list(name = "MNAR_30", mech = "MNAR", rate = 0.30, seed = 20266)
)

output_files <- c() # Record generated files for zipping

for (params in set_params) {
  name <- params$name
  
  # A. Generate masked data and save
  masked_data <- generate_missing(df, "balance", params$mech, params$rate, params$seed)
  data_filename <- paste0(name, "_data.csv")
  write.csv(masked_data, data_filename, row.names = FALSE)
  output_files <- c(output_files, data_filename)
  
  # B. Generate Correlation Matrix
  # Create Missingness Indicator M_balance (1 = missing, 0 = observed)
  M_balance <- ifelse(is.na(masked_data$balance), 1, 0)
  
  # Extract all numeric columns (Note: using original 'df' to keep the TRUE 'balance' values)
  numeric_cols <- df %>% select_if(is.numeric)
  
  # Calculate point-biserial correlation
  corr_matrix <- cor(numeric_cols, M_balance, use = "pairwise.complete.obs")
  
  # Format as standard Dataframe
  corr_df <- data.frame(
    Variable = rownames(corr_matrix),
    Correlation_with_M_balance = as.numeric(corr_matrix)
  )
  
  corr_filename <- paste0(name, "_correlation_matrix.csv")
  write.csv(corr_df, corr_filename, row.names = FALSE)
  output_files <- c(output_files, corr_filename)
}

# =====================================================================
# 5. Zip Outputs and Cleanup Intermediate Files
# =====================================================================
zip_filename <- "bank_marketing_missingness_datasets.zip"
zip::zip(zipfile = zip_filename, files = output_files)

# Optional: Remove the 12 individual CSVs, keeping only the ZIP file for a clean directory
file.remove(output_files)

cat("\n============================================\n")
cat("✅ Execution Successful!\n")
cat("6 datasets and 6 correlation matrices have been generated and zipped.\n")
cat("ZIP file location:", file.path(getwd(), zip_filename), "\n")
cat("============================================\n")

