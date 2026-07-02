# =====================================================================
# F-16 Delta Elevators Dataset
# Mechanisms: Telemetry Packet Loss (MCAR), High-G Throttling (MAR), Actuator Saturation (MNAR)
# Target Variable: Dynamically set to the LAST column of the dataset
# Driver Variable for MAR: Dynamically set to the FIRST column of the dataset
# =====================================================================

# 1. Environment Setup: Install and load required packages

library(dplyr)
library(zip)

# =====================================================================
# 2. Read Data
# =====================================================================
file_path <- "198_delta_elevators_data.csv"
if (!file.exists(file_path)) {
  stop("File not found! Please ensure the CSV file is in the working directory.")
}
df <- read.csv(file_path, stringsAsFactors = FALSE)

# =====================================================================
# 3. Define Core Masking Function (Weight-based exact sampling)
# =====================================================================
generate_elevator_missing <- function(data, mechanism, missing_rate, seed) {
  set.seed(seed)
  df_masked <- data
  n_rows <- nrow(data)
  n_missing <- round(n_rows * missing_rate)
  
  # Dynamic column selection
  # Assuming the target variable (variation) is the last column
  target_col <- names(data)[ncol(data)] 
  # Assuming the first column represents a key flight state (e.g., Pitch/Roll)
  driver_col <- names(data)[1]         
  
  if (mechanism == "MCAR") {
    # Scenario 1: Telemetry Packet Loss (Uniform random probability)
    probs <- rep(1, n_rows) 
    
  } else if (mechanism == "MAR") {
    # Scenario 2: High-G Maneuver Logging Throttling (Driver: Flight State)
    # If the flight state is extreme (e.g., top or bottom 15% of values), it has a 5x higher chance of missing
    lower_bound <- quantile(data[[driver_col]], 0.15, na.rm = TRUE)
    upper_bound <- quantile(data[[driver_col]], 0.85, na.rm = TRUE)
    probs <- ifelse(data[[driver_col]] < lower_bound | data[[driver_col]] > upper_bound, 5, 1)
    
  } else if (mechanism == "MNAR") {
    # Scenario 3: Actuator Saturation Trap (Driver: Extreme value of the Target itself)
    # If the required delta is extremely high/low (top/bottom 10%), it hits saturation limits and is 8x more likely to be dropped
    lower_limit <- quantile(data[[target_col]], 0.10, na.rm = TRUE)
    upper_limit <- quantile(data[[target_col]], 0.90, na.rm = TRUE)
    probs <- ifelse(data[[target_col]] < lower_limit | data[[target_col]] > upper_limit, 8, 1)
  }
  
  # Execute weighted sampling without replacement to achieve exact missing rate
  valid_indices <- 1:n_rows
  missing_indices <- sample(valid_indices, size = n_missing, replace = FALSE, prob = probs)
  
  # Apply mask: Set sampled row values to NA
  df_masked[[target_col]][missing_indices] <- NA
  
  return(list(data = df_masked, target = target_col))
}

# =====================================================================
# 4. Batch Generate 6 Control Datasets and Correlation Matrices
# =====================================================================
set_params <- list(
  list(name = "Elevators_MCAR_10", mech = "MCAR", rate = 0.10, seed = 601),
  list(name = "Elevators_MAR_10",  mech = "MAR",  rate = 0.10, seed = 602),
  list(name = "Elevators_MNAR_10", mech = "MNAR", rate = 0.10, seed = 603),
  list(name = "Elevators_MCAR_30", mech = "MCAR", rate = 0.30, seed = 604),
  list(name = "Elevators_MAR_30",  mech = "MAR",  rate = 0.30, seed = 605),
  list(name = "Elevators_MNAR_30", mech = "MNAR", rate = 0.30, seed = 606)
)

output_files <- c() # Track generated files for zipping

cat("Starting aerospace missing data injection and correlation computation...\n")

for (params in set_params) {
  name <- params$name
  
  # A. Generate masked data
  result <- generate_elevator_missing(df, params$mech, params$rate, params$seed)
  masked_data <- result$data
  target_col <- result$target
  
  # Export dataset to CSV
  data_filename <- paste0(name, "_data.csv")
  write.csv(masked_data, data_filename, row.names = FALSE)
  output_files <- c(output_files, data_filename)
  
  # B. Calculate missingness correlation matrix
  # Missingness indicator M (1 = missing, 0 = observed)
  M_indicator <- ifelse(is.na(masked_data[[target_col]]), 1, 0)
  
  # Extract all numerical columns to calculate point-biserial correlation
  numeric_cols <- df %>% select_if(is.numeric)
  corr_matrix <- cor(numeric_cols, M_indicator, use = "pairwise.complete.obs")
  
  corr_df <- data.frame(
    Variable = rownames(corr_matrix),
    Correlation_with_M = round(as.numeric(corr_matrix), 4)
  )
  
  # =====================================================================
  #  The Actuator Saturation Trap (MNAR):
  # If the mechanism is MNAR, forcefully remove the target variable row.
  # This simulates reality where the saturated/failed commands leave no trace.
  # =====================================================================
  if (params$mech == "MNAR") {
    corr_df <- corr_df %>% filter(Variable != target_col)
  }
  
  # Save correlation matrix
  corr_filename <- paste0(name, "_correlation_matrix.csv")
  write.csv(corr_df, corr_filename, row.names = FALSE)
  output_files <- c(output_files, corr_filename)
}

# =====================================================================
# 5. Package Outputs and Clean Workspace
# =====================================================================
zip_filename <- "Elevators_Missingness_Experiment_Suite.zip"

# Create a ZIP archive containing all 12 CSV files
zip::zip(zipfile = zip_filename, files = output_files)

# Remove the 12 individual CSV files to keep the local directory clean
file.remove(output_files)

cat("\n======================================================\n")
cat(" F-16 Elevators dataset generation successful!\n")
cat("Injected mechanisms: [Telemetry Loss (MCAR) / High-G Throttling (MAR) / Actuator Saturation (MNAR)]\n")
cat("6 datasets and 6 correlation matrices have been successfully zipped.\n")
cat("ZIP Output Path: ", file.path(getwd(), zip_filename), "\n")
cat("======================================================\n")
