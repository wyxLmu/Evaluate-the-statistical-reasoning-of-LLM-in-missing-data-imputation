# Step 1: Automatically detect, install, and load all necessary dependencies
required_packages <- c("OpenML", "dplyr", "farff", "stringr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing missing dependency:", pkg))
    install.packages(pkg)
  }
}

# Load packages
library(OpenML)
library(dplyr)
library(stringr)
library(farff) # Explicitly loaded to ensure the ARFF parsing engine is ready

message("Fetching global dataset list from OpenML...")
all_datasets <- listOMLDataSets()

# Step 2: Basic structural filtering (to ensure the scientific validity of the empirical evaluation)
filtered_datasets <- all_datasets %>%
  filter(
    number.of.instances >= 3000,
    number.of.instances <= 100000,
    number.of.features >= 5,
    number.of.features <= 25,
    number.of.missing.values == 0,
    status == "active"
  )

# Step 3: Extract candidate pool and shuffle
set.seed(2026) # Ensures reproducibility for every extraction
candidate_pool <- sample_n(filtered_datasets, min(100, nrow(filtered_datasets)))

# Step 4: Batch fetch background stories, execute automatic "story length" review
message(" Starting sociological background review for candidate datasets (looking for rich stories with > 200 characters)...")

rich_datasets <- data.frame()

for (i in 1:nrow(candidate_pool)) {
  current_id <- candidate_pool$data.id[i]
  current_name <- candidate_pool$name[i]
  
  # Add tryCatch to prevent interruption due to a server error for a specific dataset
  oml_dataset <- tryCatch({
    getOMLDataSet(data.id = current_id)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(oml_dataset)) next
  
  # Extract background description, clean up extra line breaks and spaces
  desc_text <- oml_dataset$desc$description
  if (is.null(desc_text) || is.na(desc_text)) desc_text <- ""
  clean_desc <- str_squish(desc_text)
  
  # Core review: Background story must be greater than 200 characters
  if (nchar(clean_desc) > 200) {
    message("aptured high-quality story dataset: ", current_name, " (Length: ", nchar(clean_desc), ")")
    
    rich_datasets <- bind_rows(rich_datasets, data.frame(
      data.id = current_id,
      name = current_name,
      desc_length = nchar(clean_desc),
      preview = substr(clean_desc, 1, 150) # Extract the first 150 characters as a preview for your selection
    ))
  }
  
  # Efficiency optimization: Stop downloading immediately once 20 premium datasets are found
  if (nrow(rich_datasets) >= 25) {
    message(" Successfully collected 20 candidate datasets. Terminating search early to save time!")
    break
  }
}

# Step 5: Sort by story richness (character count) from longest to shortest
rich_datasets <- rich_datasets %>% arrange(desc(desc_length))

message("\n=======================================================")
message(" Congratulations! Here is your curated menu of high-quality datasets (for you to pick the final 10):")
print(rich_datasets[, c("data.id", "name", "desc_length")])

# If you want to view their stories in RStudio like in Excel:
# View(rich_datasets)

# ---------------------------------------------------------
# Assuming you want to look at the 1st dataset from the rich_datasets menu
# If you want to view a specific ID, you can change this to target_id <- 12345
# ---------------------------------------------------------
target_id <- rich_datasets$data.id[1] 
target_name <- rich_datasets$name[1]

message("\nDownloading and parsing the complete dataset for you: ", target_name, " (ID: ", target_id, ") ...")

# 1. Download the complete package for this dataset
oml_dataset <- getOMLDataSet(data.id = target_id)

# 2. Extract the raw data table and background story
raw_data <- oml_dataset$data
dataset_background <- oml_dataset$desc$description

# 3. Display data: Open the data table in RStudio like Excel
# This will pop up a new tab in the top left of RStudio, allowing you to visually inspect all columns and numbers
View(raw_data)

# 4. Display story: Format and print the complete background story in the console
# Professor's tip: We use cat() instead of print() here because cat() perfectly parses line breaks in the text, letting you read it like an article!
cat("\n=======================================================\n")
cat("Complete sociological background story for [", target_name, "]\n")
cat("=======================================================\n\n")
cat(dataset_background)
cat("\n\n=======================================================\n")


# 1. Create a dedicated folder in your current R working directory
export_dir <- "OpenML_Golden_Datasets"
if (!dir.exists(export_dir)) {
  dir.create(export_dir)
  message("Successfully created local folder: ", export_dir)
}

message("Starting batch download and packaging of 20 candidate datasets. Please be patient (this may take a few minutes)...")

# 2. Start looping through the rich_datasets menu you just found
for (i in 1:nrow(rich_datasets)) {
  target_id <- rich_datasets$data.id[i]
  
  # Clean up special characters in the dataset name to prevent errors when saving as a filename
  target_name <- gsub("[^A-Za-z0-9]", "_", rich_datasets$name[i]) 
  
  message(sprintf("packaging dataset %d/20: %s (ID: %d)", i, target_name, target_id))
  
  # Download the dataset (wrapped in tryCatch as a safety net)
  oml_dataset <- tryCatch({
    getOMLDataSet(data.id = target_id)
  }, error = function(e) {
    message("  Download failed, skipping this dataset.")
    return(NULL)
  })
  
  # If download failed, skip to the next iteration
  if (is.null(oml_dataset)) next
  
  # Extract raw data and background story
  raw_data <- oml_dataset$data
  dataset_background <- oml_dataset$desc$description
  if (is.null(dataset_background)) dataset_background <- "No description provided."
  
  # 3. Build the save paths
  # Format: Folder/12345_DatasetName_data.csv
  csv_file_path <- file.path(export_dir, paste0(target_id, "_", target_name, "_data.csv"))
  txt_file_path <- file.path(export_dir, paste0(target_id, "_", target_name, "_story.txt"))
  
  # 4. Write to local hard drive
  # Export pure tabular data to CSV (can be opened directly with Excel)
  write.csv(raw_data, file = csv_file_path, row.names = FALSE)
  
  # Export background story to TXT text (can be opened directly with Notepad)
  writeLines(dataset_background, con = txt_file_path)
}



