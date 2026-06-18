# =====================================================================
# CPU Activity Dataset: Missingness Injection (R Script)
# Target Variable: usr (Portion of time CPUs run in user mode)
# =====================================================================

# 1. Environment Setup: Install and load required R packages
library(dplyr)
library(zip)

# =====================================================================
# 2. Load Original Data
# =====================================================================
# Ensure the dataset file is located in your R working directory
df <- read.csv("44132_cpu_act_data.csv", stringsAsFactors = FALSE)

# =====================================================================
# 3. Define Core Masking Function (Mapping to System Scenarios)
# =====================================================================
generate_cpu_missing <- function(data, target_col, mechanism, missing_rate, seed) {
  set.seed(2026)
  df_masked <- data
  
  # Get indices of valid (non-NA) rows for the target column
  valid_indices <- which(!is.na(data[[target_col]]))
  n_missing <- round(length(valid_indices) * missing_rate)
  
  if (mechanism == "MCAR") {
    # Scenario 1: MCAR (UDP network random packet loss)
    # All CPU records have an equal probability of being lost
    probs <- rep(1, length(valid_indices))
    
  } else if (mechanism == "MAR") {
    # Scenario 2: MAR (Buffer overflow based on system calls 'scall')
    # The higher the scall (frequent I/O or kernel interrupts), the higher the probability of dropping logs
    scall_scaled <- (data$scall[valid_indices] - min(data$scall, na.rm=TRUE)) / 
      (max(data$scall, na.rm=TRUE) - min(data$scall, na.rm=TRUE))
    probs <- exp(scall_scaled * 5) 
    
  } else if (mechanism == "MNAR") {
    # Scenario 3: MNAR (Monitor daemon starved/deadlocked due to extreme CPU usage)
    # The higher the true 'usr' (CPU load), the exponentially higher the chance of recording failure (Right-tail missingness)
    usr_scaled <- (data[[target_col]][valid_indices] - min(data[[target_col]], na.rm=TRUE)) / 
      (max(data[[target_col]], na.rm=TRUE) - min(data[[target_col]], na.rm=TRUE))
    probs <- exp(usr_scaled * 8) 
  }
  
  # Failsafe: Prevent zero-weight vector
  if (sum(probs) == 0) probs <- rep(1, length(valid_indices)) 
  
  # Execute weighted sampling without replacement
  missing_indices <- sample(valid_indices, size = n_missing, replace = FALSE, prob = probs)
  
  # Apply masking: Set sampled rows to NA
  df_masked[[target_col]][missing_indices] <- NA
  return(df_masked)
}

# =====================================================================
# 4. Batch Generate 6 Control Datasets & Correlation Matrices
# =====================================================================
# Define experimental group parameters (10% and 30% missing rates)
set_params <- list(
  list(name = "CPU_MCAR_10", mech = "MCAR", rate = 0.10, seed = 501),
  list(name = "CPU_MAR_10",  mech = "MAR",  rate = 0.10, seed = 502),
  list(name = "CPU_MNAR_10", mech = "MNAR", rate = 0.10, seed = 503),
  list(name = "CPU_MCAR_30", mech = "MCAR", rate = 0.30, seed = 504),
  list(name = "CPU_MAR_30",  mech = "MAR",  rate = 0.30, seed = 505),
  list(name = "CPU_MNAR_30", mech = "MNAR", rate = 0.30, seed = 506)
)

output_files <- c() # Record generated file names for zipping

for (params in set_params) {
  name <- params$name
  
  # A. Generate masked data and export to CSV
  masked_data <- generate_cpu_missing(df, "usr", params$mech, params$rate, params$seed)
  data_filename <- paste0(name, "_data.csv")
  write.csv(masked_data, data_filename, row.names = FALSE)
  output_files <- c(output_files, data_filename)
  
  # B. Calculate Correlation Matrix
  # Missingness indicator M_usr (1 = missing, 0 = observed)
  M_usr <- ifelse(is.na(masked_data$usr), 1, 0)
  
  # Extract all numeric columns from the ORIGINAL dataframe to compute point-biserial correlation
  numeric_cols <- df %>% select_if(is.numeric)
  corr_matrix <- cor(numeric_cols, M_usr, use = "pairwise.complete.obs")
  
  corr_df <- data.frame(
    Variable = rownames(corr_matrix),
    Correlation_with_M = as.numeric(corr_matrix)
  )
  
  # =====================================================================
  # 🚨 CORE MNAR TRAP FOR THE EXPERIMENT:
  # If the mechanism is MNAR, forcefully remove the "usr" row from the matrix!
  # This deprives the LLM and human analysts of the "ground truth" 
  # that missingness is highly correlated with CPU extreme values.
  # =====================================================================
  if (params$mech == "MNAR") {
    corr_df <- corr_df %>% filter(Variable != "usr")
  }
  
  # Save the correlation matrix
  corr_filename <- paste0(name, "_correlation_matrix.csv")
  write.csv(corr_df, corr_filename, row.names = FALSE)
  output_files <- c(output_files, corr_filename)
}

# =====================================================================
# 5. Zip Outputs and Cleanup Workspace
# =====================================================================
zip_filename <- "cpu_activity_missingness_datasets.zip"
zip::zip(zipfile = zip_filename, files = output_files)

# Remove the 12 individual CSVs to keep the local directory clean
file.remove(output_files)

cat("\n======================================================\n")
cat("✅ Data Generation Successful!\n")
cat("Successfully injected [Network Drop(MCAR) / Buffer Overflow(MAR) / Daemon Starvation(MNAR)] mechanisms.\n")
cat("6 control datasets and 6 correlation matrices have been packed.\n")
cat("Output Path:", file.path(getwd(), zip_filename), "\n")

