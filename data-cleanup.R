library(data.table)
library(haven)
library(stringr)
library(jsonlite)
library(dplyr)
library(forcats)


# Function to parse labels from the .csv file, stopping at "writeplan"
parse_label_csv <- function(csv_file) {
  # Read the .csv file
  labels_dt <- fread(csv_file, encoding = "UTF-8")
  
  # Identify the row where the "writeplan" variable appears
  stop_row <- which(labels_dt$variable == "writeplan")[1]
  
  # Subset the data to only include rows up to the "writeplan" variable
  labels_dt <- labels_dt[1:stop_row, ]
  
  # Recode missing data labels (.m, .n, .l, .d) to 996, 997, 998, and 999
  labels_dt[value == ".m", value := 996]
  labels_dt[value == ".n", value := 997]
  labels_dt[value == ".l", value := 998]
  labels_dt[value == ".d", value := 999]
  
  # Remove all types of apostrophes from the labels
  labels_dt[, desc := gsub("[‘’'`]", "", desc)]
  
  return(labels_dt)
}



# Apply labels to each variable in the dataset using the .csv file
apply_labels_to_data_csv <- function(data, labels_dt) {
  if (is.null(labels_dt)) {
    return(data)  # No labels, return the data as-is
  }
  
  unique_variables <- unique(labels_dt$variable)
  
  for (var in unique_variables) {
    if (var %in% names(data)) {
      var_labels <- labels_dt[variable == var]
      
      # Check if there are labels for values below 900 (indicating a categorical variable)
      has_labels_below_900 <- any(as.numeric(var_labels$value) < 900, na.rm = TRUE)
      
      if (has_labels_below_900) {
        # Create a named vector for labels using numeric values
        label_map <- setNames(var_labels$desc, as.character(var_labels$value))  # Ensure labels use character values for matching
        
        # Convert the variable values to character for comparison with label_map
        data[[var]] <- as.character(data[[var]])
        
        # Create the "_label" column with the mapped labels
        data[[paste0(var, "_label")]] <- ifelse(data[[var]] %in% names(label_map), label_map[data[[var]]], NA)
        
        # Convert the original column back to numeric
        data[[var]] <- as.numeric(data[[var]])
        
      }
    }
  }
  
  return(data)
}


# Directory containing the .csv files
csv_dir <- "ml-data-prep/download-nsch-data"

# Process each year's data
all_data_list <- list()

# Create a table to store the proportions of imputed values for each year
imputation_proportions <- data.frame(
  year = integer(),
  category = integer(),
  proportion = numeric()
)

# Placeholder for 2016 special case
imputed_data_2016 <- NULL

for (year in 2016:2023) {
  dta_file <- file.path(csv_dir, paste0("nsch_", year, "_topical.dta"))
  csv_file <- file.path(csv_dir, paste0("nsch_", year, "_topical.do.define.csv"))
  
  # Read the .dta file using haven (this will automatically handle tagged NA values)
  raw_data <- read_dta(dta_file)
  
  # Identify and replace tagged NA values with numeric codes
  for (col in names(raw_data)) {
    raw_data[[col]][is_tagged_na(raw_data[[col]], "m")] <- 996
    raw_data[[col]][is_tagged_na(raw_data[[col]], "n")] <- 997
    raw_data[[col]][is_tagged_na(raw_data[[col]], "l")] <- 998
    raw_data[[col]][is_tagged_na(raw_data[[col]], "d")] <- 999
  }
  
  # Handle the 'stratum' column: replace '2A' (non-case sensitive) with '2' and convert to numeric
  if ("stratum" %in% names(raw_data)) {
    # Convert to character to handle case-insensitive replacement
    raw_data$stratum <- as.character(raw_data$stratum)
    
    # Replace '2A' or '2a' with '2'
    raw_data$stratum[grepl("^2a?$", raw_data$stratum, ignore.case = TRUE)] <- "2"
    
    # Convert the 'stratum' column to numeric
    raw_data$stratum <- as.numeric(raw_data$stratum)
  }
  
  
  # Parse labels from the .csv file
  parsed_labels <- parse_label_csv(csv_file)
  
  # Remove stratum entries from 2016's parsed labels
  if (year == 2016) {
    parsed_labels <- parsed_labels[variable != "stratum"]
  }
  
  # Apply labels to the data
  labeled_data <- apply_labels_to_data_csv(raw_data, parsed_labels)
  
  # This section is to handle the imputations needed for a1_grade, higrade, and higrade_tvis in 2016
  # For year 2016, split off the imputation flag and a1_grade_i columns
  if (year == 2016) {
    imputed_data_2016 <- labeled_data[, c("hhid", "a1_grade_if", "a1_grade_i"), with = FALSE]
  }
  
  
  # For years 2017-2022, calculate proportions for imputed values in a1_grade
  if (year >= 2017) {
    # Filter for rows where imputation flag is true
    imputed_subset <- labeled_data[labeled_data$a1_grade_if == TRUE, ]
    
    # Calculate proportions for each category in a1_grade
    prop_table <- prop.table(table(imputed_subset$a1_grade))
    
    # Add to the imputation proportions table
    for (category in names(prop_table)) {
      imputation_proportions <- rbind(
        imputation_proportions,
        data.frame(year = year, category = as.integer(category), proportion = prop_table[category])
      )
    }
  }
  
  # Print the unique stratum values for the current year
  cat(paste("\n[DEBUG] Unique stratum values for year:", year, "\n"))
  print(unique(labeled_data$stratum))
  
  # Store processed data
  all_data_list[[as.character(year)]] <- data.table(labeled_data)
}


for (year in 2016:2023) {
  cat(paste0("\n[", year, "] Table for hoursleep:\n"))
  this_year_data <- all_data_list[[as.character(year)]]
  print(table(this_year_data$hoursleep, useNA = "ifany"))
}

for (year in 2016:2023) {
  cat(paste0("\n[", year, "] Table for hoursleep05:\n"))
  this_year_data <- all_data_list[[as.character(year)]]
  print(table(this_year_data$hoursleep05, useNA = "ifany"))
}


# Function to recode, collapse, or reassign categories in a variable
apply_transformations <- function(data, transformations, year) {
  for (variable_name in names(transformations)) {
    details <- transformations[[variable_name]]
    
    if (year %in% details$years && variable_name %in% names(data)) {
      # Access the "_label" column
      label_col <- paste0(variable_name, "_label")
      
      # Apply JSON transformations
      for (i in seq_along(details$value)) {
        old_val <- details$value[i]
        new_val <- details$new_value[i]
        new_label <- details$new_label[i]
        
        # Update values: change raw data values if `old_val` and `new_val` are different
        data[[variable_name]][data[[variable_name]] == as.numeric(old_val)] <- as.numeric(new_val)
        
        # Update labels: set new labels for updated values
        data[[label_col]][data[[variable_name]] == new_val] <- new_label
      }
    }
  }
  
  return(data)
}



# Function to handle merging of two variables
merge_columns <- function(data, merge_config, year) {
  for (variable_name in names(merge_config)) {
    details <- merge_config[[variable_name]]
    
    if (year %in% details$years && details$column_1 %in% names(data) && details$column_2 %in% names(data)) {
      column_1 <- data[[details$column_1]]
      column_2 <- data[[details$column_2]]
      
      # Combine the labels from both columns
      label_col_1 <- paste0(details$column_1, "_label")
      label_col_2 <- paste0(details$column_2, "_label")
      
      combined_labels <- ifelse(is.na(column_1), data[[label_col_2]], data[[label_col_1]])
      
      # Merge the numeric columns
      new_column <- ifelse(is.na(column_1), column_2, column_1)
      data[[variable_name]] <- as.numeric(new_column)  # Ensure the merged column is numeric
      data[[paste0(variable_name, "_label")]] <- combined_labels
    }
  }
  return(data)
}

# Function to rename a variable
rename_columns <- function(data, rename_config, year) {
  for (old_name in names(rename_config)) {
    details <- rename_config[[old_name]]
    if (year %in% details$years && old_name %in% names(data)) {
      new_name <- details$new_name
      setnames(data, old_name, new_name)
      # Also rename the "_label" column
      setnames(data, paste0(old_name, "_label"), paste0(new_name, "_label"))
    }
  }
  return(data)
}







# Load configuration JSON
config <- fromJSON("ml-data-prep/variable-config.json")
desired_variables <- config$desired_variables

# Process all yearly datasets
processed_datasets <- lapply(names(all_data_list), function(year) {
  # Access the original data table using the year name
  original_data <- all_data_list[[as.character(year)]]
  
  # Apply transformations using the year
  transformed_data <- apply_transformations(original_data, config$transformations$transform, year)
  
  print(unique(transformed_data$k2q01_d_label))
  
  # Apply column renaming
  renamed_data <- rename_columns(transformed_data, config$transformations$rename_columns, year)
  
  # Apply column merges
  merged_data <- merge_columns(renamed_data, config$transformations$merge_columns, year)
  
  # Subset to desired variables, including their corresponding _label columns
  label_columns <- paste0(desired_variables, "_label")
  existing_label_columns <- intersect(label_columns, names(merged_data))
  
  # Combine desired variables and the existing label columns for subsetting
  subset_columns <- c(desired_variables, existing_label_columns)
  
  # Perform the subsetting
  subset_data <- merged_data[, ..subset_columns]
  
  return(subset_data)
})




# Combine all data into one data.table
combined_data <- rbindlist(processed_datasets, use.names = TRUE, fill = TRUE)








# Function to replace missing numeric data codes with NA and handle factor conversion
final_conversion_to_factors <- function(data) {
  # Define the missing data labels for categorical variables
  na_labels <- c("No valid response", "Not in universe", "Logical skip", "Suppressed for confidentiality")
  
  # Define the missing numeric data codes
  na_numeric_codes <- c(996, 997, 998, 999)
  
  for (var in names(data)) {
    label_col <- paste0(var, "_label")
    
    print(var)
    
    # Check if there is a corresponding "_label" column (indicating this is a categorical variable)
    if (label_col %in% names(data)) {
      
      
      # Create a named vector to map values to labels
      value_label_map <- setNames(unique(data[[label_col]]), unique(data[[var]]))
      
      # Sort the map by the numeric values
      values <- as.numeric(names(value_label_map))
      sorted_labels <- value_label_map[order(values)]
      sorted_values <- as.numeric(names(sorted_labels))
      
      if (var == "k2q35d"){
        print(sorted_values)
        print(str(sorted_labels))
        print(sorted_labels)
      }
      
      
      # Convert the original column to a factor using the labels from the "_label" column
      # OLD LINE THAT WAS BROKEN data[[var]] <- factor(data[[var]], levels = order(sorted_values), labels = sorted_labels) #
      data[[var]] <- factor(data[[var]], levels = sorted_values, labels = sorted_labels)
      # Replace missing data labels with NA for categorical columns
      levels(data[[var]])[levels(data[[var]]) %in% na_labels] <- NA
      
      # Remove the "_label" column as it is no longer needed
      data[[label_col]] <- NULL
    } else {
      # If there is no label column, treat this as a numeric column
      if (is.numeric(data[[var]])) {
        # Replace numeric missing data codes with NA
        data[[var]][data[[var]] %in% na_numeric_codes] <- NA
      }
    }
  }
  
  return(data)
}



# Convert labeled vectors to factors and handle missing data
combined_data <- final_conversion_to_factors(combined_data)


# In this section of code we handle special cases




# The fipsst codes processed from the individual define.csv files always seem to result in unexpected missing data
# When attempting to create a state variable with state names
# The workaround is to manually use this map to create a state name variable

# Correct fipsst to state name map
fips_to_state <- setNames(
  c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
    "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", 
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
    "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
    "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
    "Wisconsin", "Wyoming"), 
  as.character(c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 
                 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 
                 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56))
)

# Create a state category that has the state name instead of the code
combined_data$state <- fips_to_state[as.character(combined_data$fipsst)]


print(class(combined_data$family))



# Now we need to collapse grandparent and single father categories in family into other relative
# This is because these two categories are missing from the 2016 data
combined_data$family <- fct_recode(combined_data$family,
                                   "Other relation" = "Single father",
                                   "Other relation" = "Grandparent household")



# Ensure the data is ordered on hhid and that the imputation data for 2016 is also ordered
imputed_data_2016 <- imputed_data_2016[order(imputed_data_2016$hhid),]
combined_data <- combined_data[order(combined_data$hhid),]


# This is a large section of code but this is where we use the existing 2016 imputed values
# to fill in a1_grade higrade and higrade_tvis
# Higrade is a direct transfer of the a1_grade_i values
# A1_grade requires us to randomly assign categories based on average proportions of imputed data in 2017-2022
# Based on the imputed values we made for A1 grade we can assign higrade_tvis which is just a slightly more detailed version of higrade
# This results in no missing data for all three variables across the entire dataset.


# Copy a1_grade_i to higrade_impute as strings
imputed_data_2016$higrade_impute <- as.character(imputed_data_2016$a1_grade_i)

# Map numeric values from a1_grade_i to their corresponding string descriptions
imputed_data_2016$higrade_impute[imputed_data_2016$a1_grade_i == 1] <- "Less than high school"
imputed_data_2016$higrade_impute[imputed_data_2016$a1_grade_i == 2] <- "High school (including vocational, trade, or business school)"
imputed_data_2016$higrade_impute[imputed_data_2016$a1_grade_i == 3] <- "More than high school"


# Step 1: Calculate the average proportion for each category
average_proportions <- imputation_proportions %>%
  group_by(category) %>%
  summarise(avg_proportion = mean(proportion))

# Create a new column a1_grade_impute from a1_grade_i as a character
imputed_data_2016$a1_grade_impute <- as.character(imputed_data_2016$a1_grade_i)

# Define the possible values for each category
grade_1_options <- c("8th grade or less", "9th-12th grade; No diploma")
grade_2_options <- c("High School Graduate or GED Completed", "Completed a vocational, trade, or business school program")
grade_3_options <- c("Some College Credit, but No Degree", "Associate Degree (AA, AS)", "Bachelors Degree (BA, BS, AB)", 
                     "Masters Degree (MA, MS, MSW, MBA)", "Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, JD)")

# weight schemes pulled from average proportions of imputed data from other years
grade_1_weights <- c(average_proportions[1,2], average_proportions[2,2])
grade_2_weights <- c(average_proportions[3,2], average_proportions[4,2])
grade_3_weights <- c(average_proportions[5,2], average_proportions[6,2],
                     average_proportions[7,2], average_proportions[8,2], 
                     average_proportions[9,2])

# Function to randomly assign based on proportions
assign_grade <- function(value, options, weights) {
  sample(options, size = 1, prob = weights)
}

# Assign values based on a1_grade_i
imputed_data_2016$a1_grade_impute[imputed_data_2016$a1_grade_i == 1] <- sapply(
  imputed_data_2016$a1_grade_i[imputed_data_2016$a1_grade_i == 1],
  function(x) assign_grade(x, grade_1_options, grade_1_weights)
)

imputed_data_2016$a1_grade_impute[imputed_data_2016$a1_grade_i == 2] <- sapply(
  imputed_data_2016$a1_grade_i[imputed_data_2016$a1_grade_i == 2],
  function(x) assign_grade(x, grade_2_options, grade_2_weights)
)

imputed_data_2016$a1_grade_impute[imputed_data_2016$a1_grade_i == 3] <- sapply(
  imputed_data_2016$a1_grade_i[imputed_data_2016$a1_grade_i == 3],
  function(x) assign_grade(x, grade_3_options, grade_3_weights)
)




# Assign the new column 'higrade_tvis' in imputed_data_2016 based on the values in 'a1_grade_impute'
imputed_data_2016$higrade_tvis_impute <- case_when(
  imputed_data_2016$a1_grade_impute %in% c("8th grade or less", "9th-12th grade; No diploma") ~ "Less than high school",
  imputed_data_2016$a1_grade_impute %in% c("High School Graduate or GED Completed", "Completed a vocational, trade, or business school program") ~ "High school (including vocational, trade, or business school)",
  imputed_data_2016$a1_grade_impute %in% c("Some College Credit, but No Degree", "Associate Degree (AA, AS)") ~ "Some college or Associate Degree",
  imputed_data_2016$a1_grade_impute %in% c("Bachelors Degree (BA, BS, AB)", "Masters Degree (MA, MS, MSW, MBA)", "Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, JD)") ~ "College degree or higher",
  TRUE ~ NA_character_  # Set any unmatched cases to NA
)


# Assign the imputed values to the missing data cells in combined dataset
combined_data$higrade[is.na(combined_data$higrade)] <- imputed_data_2016$higrade_impute[is.na(combined_data$higrade)]
combined_data$a1_grade[is.na(combined_data$a1_grade)] <- imputed_data_2016$a1_grade_impute[is.na(combined_data$a1_grade)]
combined_data$higrade_tvis[is.na(combined_data$higrade_tvis)] <- imputed_data_2016$higrade_tvis_impute[is.na(combined_data$higrade_tvis)]




# For this study it was requested that we use the developmental screening compound variable from cahmi
# Since this data doesn't rely on imputed or weighted values we have just joined it from the cahmi datasets.

# Load develeopmental screening dataset that has been extracted from the yearly cahmi datasets
devscrnng_data <- readRDS("ml-data-prep/devscrnng_2016_2023.rds")

# Ensure that hhid is a key in both data.tables
setkey(combined_data, hhid)
setkey(devscrnng_data, hhid)

# Join the decscrnng variable to the combined dataset
final_data <- combined_data[devscrnng_data, nomatch = 0]



# Save the final data
saveRDS(final_data, "ml-data-prep/clean-data-2016-2023.rds")


str(final_data$k4q02_r)
table(final_data$k4q02_r, useNA = "ifany")
