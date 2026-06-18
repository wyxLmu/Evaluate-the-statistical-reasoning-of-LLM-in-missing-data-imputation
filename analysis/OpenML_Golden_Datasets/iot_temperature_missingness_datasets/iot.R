# =====================================================================
# IoT Temperature Dataset: Missingness Injection (R Script)
# Target Variable: temp (Temperature readings)
# MAR Driver Variable: out.in (Location of device)
# =====================================================================

# 1. Environment Setup: Install and load required R packages


library(dplyr)
library(zip)

# =====================================================================
# 2. Read and Preprocess Data
# =====================================================================
# Ensure the dataset file is located in your R working directory
df <- read.csv("43351_Temperature_Readings__IOT_Devices_data.csv", stringsAsFactors = FALSE)

# [CRITICAL PREPROCESSING FOR CORRELATION MATRIX]
# Convert the categorical 'out.in' variable into a numeric dummy variable 'is_out'.
# 1 = "Out" (Outside), 0 = "In" (Inside). 
# This allows the correlation matrix to capture the relationship mathematically.
df$is_out <- ifelse(df$out.in == "Out", 1, 0)

# =====================================================================
# 3. Define Core Masking Function (Mapping to IoT Hardware Scenarios)
# =====================================================================
generate_iot_missing <- function(data, target_col, mechanism, missing_rate, seed) {
  set.seed(2026)
  df_masked <- data
  
  # Get indices of valid (non-NA) rows for the target column
  valid_indices <- which(!is.na(data[[target_col]]))
  n_missing <- round(length(valid_indices) * missing_rate)
  
  if (mechanism == "MCAR") {
    # Scenario 1: MCAR (Server random restart / Alpha firmware bug)
    # All temperature records have an equal probability of being lost
    probs <- rep(1, length(valid_indices))
    
  } else if (mechanism == "MAR") {
    # Scenario 2: MAR (Wi-Fi signal attenuation due to physical walls)
    # Sensors located outside ('is_out' == 1) have a much higher chance of dropping connection
    probs <- ifelse(data$is_out[valid_indices] == 1, 0.85, 0.15) 
    
  } else if (mechanism == "MNAR") {
    # Scenario 3: MNAR (Thermal Shutdown of cheap sensors)
    # The higher the extreme heat, the exponentially higher the chance of hardware failure
    temp_scaled <- (data[[target_col]][valid_indices] - min(data[[target_col]], na.rm=TRUE)) / 
      (max(data[[target_col]], na.rm=TRUE) - min(data[[target_col]], na.rm=TRUE))
    probs <- exp(temp_scaled * 8) # Strong exponential penalty for extreme temperatures
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
  list(name = "IoT_MCAR_10", mech = "MCAR", rate = 0.10, seed = 901),
  list(name = "IoT_MAR_10",  mech = "MAR",  rate = 0.10, seed = 902),
  list(name = "IoT_MNAR_10", mech = "MNAR", rate = 0.10, seed = 903),
  list(name = "IoT_MCAR_30", mech = "MCAR", rate = 0.30, seed = 904),
  list(name = "IoT_MAR_30",  mech = "MAR",  rate = 0.30, seed = 905),
  list(name = "IoT_MNAR_30", mech = "MNAR", rate = 0.30, seed = 906)
)

output_files <- c() # Record generated file names for zipping

for (params in set_params) {
  name <- params$name
  
  # A. Generate masked data and export to CSV
  masked_data <- generate_iot_missing(df, "temp", params$mech, params$rate, params$seed)
  data_filename <- paste0(name, "_data.csv")
  write.csv(masked_data, data_filename, row.names = FALSE)
  output_files <- c(output_files, data_filename)
  
  # B. Calculate Correlation Matrix
  # Missingness indicator M_temp (1 = missing, 0 = observed)
  M_temp <- ifelse(is.na(masked_data$temp), 1, 0)
  
  # Extract all numeric columns to compute point-biserial correlation
  # Thanks to our preprocessing, this now includes 'temp' and 'is_out'
  numeric_cols <- df %>% select_if(is.numeric)
  corr_matrix <- cor(numeric_cols, M_temp, use = "pairwise.complete.obs")
  
  corr_df <- data.frame(
    Variable = rownames(corr_matrix),
    Correlation_with_M = as.numeric(corr_matrix)
  )
  
  # =====================================================================
  # 🚨 CORE MNAR TRAP FOR THE EXPERIMENT:
  # If the mechanism is MNAR, forcefully remove the "temp" row!
  # This deprives the LLM and human analysts of the "ground truth" 
  # that missingness is highly correlated with extreme heat.
  # =====================================================================
  if (params$mech == "MNAR") {
    corr_df <- corr_df %>% filter(Variable != "temp")
  }
  
  # Save the correlation matrix
  corr_filename <- paste0(name, "_correlation_matrix.csv")
  write.csv(corr_df, corr_filename, row.names = FALSE)
  output_files <- c(output_files, corr_filename)
}

# =====================================================================
# 5. Zip Outputs and Cleanup Workspace
# =====================================================================
zip_filename <- "iot_temperature_missingness_datasets.zip"
zip::zip(zipfile = zip_filename, files = output_files)

# Remove the 12 individual CSVs to keep the local directory clean
file.remove(output_files)

cat("\n======================================================\n")
cat("✅ IoT Temperature Data Generation Successful!\n")
cat("Successfully injected [Server Bug(MCAR) / Wi-Fi Attenuation(MAR) / Thermal Shutdown(MNAR)].\n")
cat("6 control datasets and 6 correlation matrices have been packed.\n")
cat("Output Path:", file.path(getwd(), zip_filename), "\n")
cat("======================================================\n")
