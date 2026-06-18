# =====================================================================
# Eye-Tracking Dataset: Missingness Injection & Correlation Matrix Generator
# Target Variable: pupilDiamMax (Maximum pupil diameter)
# =====================================================================

# 1. Environment Setup: Install and load required R packages

library(dplyr)
library(zip)

# =====================================================================
# 2. Load the Dataset
# =====================================================================
# Ensure '44093_eye_movements.csv' is in your R working directory
# If you haven't renamed it, adjust the filename accordingly.
df <- read.csv("44093_eye_movements_data.csv", stringsAsFactors = FALSE)

# =====================================================================
# 3. Define Missingness Injection Function based on Scenarios
# =====================================================================
generate_eye_missing <- function(data, target_col, mechanism, missing_rate, seed) {
  set.seed(2027)
  df_masked <- data
  
  # Get indices of rows with valid (non-NA) target values
  valid_indices <- which(!is.na(data[[target_col]]))
  n_missing <- round(length(valid_indices) * missing_rate)
  
  if (mechanism == "MCAR") {
    # Scenario 1: Hardware glitch (Completely random)
    probs <- rep(1, length(valid_indices))
    
  } else if (mechanism == "MAR") {
    # Scenario 2: Fatigue based on word position (wordNo)
    # The higher the wordNo (closer to the end), the higher the probability of missingness
    pos_scaled <- (data$wordNo[valid_indices] - min(data$wordNo, na.rm=TRUE)) / 
      (max(data$wordNo, na.rm=TRUE) - min(data$wordNo, na.rm=TRUE))
    probs <- exp(pos_scaled * 4) 
    
  } else if (mechanism == "MNAR") {
    # Scenario 3: Cognitive overload based on pupil size (pupilDiamMax)
    # Right-tail missingness: The LARGER the pupil diameter, the exponentially HIGHER the chance of missingness
    pupil_scaled <- (data[[target_col]][valid_indices] - min(data[[target_col]], na.rm=TRUE)) / 
      (max(data[[target_col]], na.rm=TRUE) - min(data[[target_col]], na.rm=TRUE))
    probs <- exp(pupil_scaled * 8) 
  }
  
  # Prevent zero-weight vector
  if (sum(probs) == 0) probs <- rep(1, length(valid_indices)) 
  
  # Weighted sampling without replacement
  missing_indices <- sample(valid_indices, size = n_missing, replace = FALSE, prob = probs)
  
  # Apply mask (set to NA)
  df_masked[[target_col]][missing_indices] <- NA
  return(df_masked)
}

# =====================================================================
# 4. Batch Generate 6 Datasets (10% & 30%) and Correlation Matrices
# =====================================================================
# Define experimental group parameters
set_params <- list(
  list(name = "EyeTrack_MCAR_10", mech = "MCAR", rate = 0.10, seed = 401),
  list(name = "EyeTrack_MAR_10",  mech = "MAR",  rate = 0.10, seed = 402),
  list(name = "EyeTrack_MNAR_10", mech = "MNAR", rate = 0.10, seed = 403),
  list(name = "EyeTrack_MCAR_30", mech = "MCAR", rate = 0.30, seed = 404),
  list(name = "EyeTrack_MAR_30",  mech = "MAR",  rate = 0.30, seed = 405),
  list(name = "EyeTrack_MNAR_30", mech = "MNAR", rate = 0.30, seed = 406)
)

output_files <- c() # Record generated files for zipping

for (params in set_params) {
  name <- params$name
  
  # A. Generate masked data and save
  masked_data <- generate_eye_missing(df, "pupilDiamMax", params$mech, params$rate, params$seed)
  data_filename <- paste0(name, "_data.csv")
  write.csv(masked_data, data_filename, row.names = FALSE)
  output_files <- c(output_files, data_filename)
  
  # B. Generate Correlation Matrix
  # Create Missingness Indicator M_pupil (1 = missing, 0 = observed)
  M_pupil <- ifelse(is.na(masked_data$pupilDiamMax), 1, 0)
  
  # Extract all numeric columns from the ORIGINAL dataframe to keep the true pupil sizes
  numeric_cols <- df %>% select_if(is.numeric)
  
  # Calculate point-biserial correlation
  corr_matrix <- cor(numeric_cols, M_pupil, use = "pairwise.complete.obs")
  
  # Format as standard Dataframe
  corr_df <- data.frame(
    Variable = rownames(corr_matrix),
    Correlation_with_M = as.numeric(corr_matrix)
  )
  
  # =====================================================================
  # THE MNAR TRAP: Crucial for your Trust/Epistemic Humility Experiment
  # If the mechanism is MNAR, physically remove the target variable row 
  # so LLMs and Human Analysts cannot see the ground truth.
  # =====================================================================
  if (params$mech == "MNAR") {
    corr_df <- corr_df %>% filter(Variable != "pupilDiamMax")
  }
  
  corr_filename <- paste0(name, "_correlation_matrix.csv")
  write.csv(corr_df, corr_filename, row.names = FALSE)
  output_files <- c(output_files, corr_filename)
}

# =====================================================================
# 5. Zip Outputs and Cleanup Intermediate Files
# =====================================================================
zip_filename <- "eye_movements_missingness_datasets.zip"
zip::zip(zipfile = zip_filename, files = output_files)

# Remove the 12 individual CSVs to keep the directory clean
file.remove(output_files)

cat("\n============================================\n")
cat("✅ Execution Successful!\n")
cat("6 Eye-Tracking datasets and 6 correlation matrices have been generated and zipped.\n")
cat("ZIP file location:", file.path(getwd(), zip_filename), "\n")
cat("============================================\n")
