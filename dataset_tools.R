
# Check for missing values in the dataset
naCheck <- function(set) {
  na_sum <- colSums(is.na(set))
  na_columns <- names(na_sum[na_sum != 0])
  
  if (length(na_columns) > 0) {
    print(set[, na_columns])
  } else {
    print("No missing values in columns.")
  }
}

# Read data from file into dataframe
read_data <- function(file_path, col_names) {
  data <- read.table(file_path, header = FALSE, sep = ",")
  colnames(data) <- col_names
  return(data)
}

# Normalize data
normalize_data <- function(df, to_analyze_cols) {
  df_normalized <- df
  
  data_normalized <- scale(subset(df, select = to_analyze_cols))
  df_normalized[to_analyze_cols] <- data_normalized
  return(df_normalized)
}

# Calculate correlation matrix and filter features with high correlation
filter_high_correlation <- function(data, tresh, to_analyze_cols) {
  
  corr_matrix <- cor(subset(data,select=to_analyze_cols))
  ggcorrplot(corr_matrix)
  
  overcorr_cells_indexes <- which(abs(corr_matrix) > tresh, arr.ind = TRUE)  # Find cells with correlation above threshold as cell indexes
  num_of_overcorr = dim(overcorr_cells_indexes)[1]  # Count the number of overcorrelations
  not_overcorrelated_features <- vector("list", length = length(to_analyze_cols))  # Initialize a list to store the features names, the ones which are overcorraleted, will be removed. 
  names(not_overcorrelated_features) <- to_analyze_cols  # Set names 
  
  for (i in 1:num_of_overcorr) {
    checking <- to_analyze_cols[overcorr_cells_indexes[i, 1]]  # Identify the first feature in the pair
    correlated_column <- to_analyze_cols[overcorr_cells_indexes[i, 2]]  # Identify the second feature in the pair
    
    if (checking != correlated_column) {
      not_overcorrelated_features[[checking]] <- c(not_overcorrelated_features[[checking]], correlated_column)  # Append correlated columns
      
    }
    
    # Check if to_cancel exists in not_overcorrelated_features and remove it
    to_cancel <- which(not_overcorrelated_features==correlated_column)  # Get the list of columns to cancel
    if (length(to_cancel) != 0) {
      not_overcorrelated_features <-not_overcorrelated_features[-to_cancel]
    }
  }
  
  
  not_overcorrelated_features_names <- names(not_overcorrelated_features)  # Extract names of not overcorrelated features
  overcorrelated_features_names <- to_analyze_cols[!to_analyze_cols %in% not_overcorrelated_features_names]  # Identify overcorrelated features
  
  
  
  return(not_overcorrelated_features_names)
}

# Show the Q-Q plots for each feature
check_normal_distribution <- function(data,y = "blue") {
  
  features <- names(data)[1:(ncol(data)-1)]  # Exclude the last column (Class)
  
  point_colors <- as.matrix(y) 
  point_colors[point_colors == 0] <- "blue"
  point_colors[point_colors == 1] <- "red"
  
  
  for (feature in features) {
    qqnorm(data[[feature]], main = paste("Q-Q Plot for", feature), col = point_colors, pch = 20)
    qqline(data[[feature]], col = "red", lwd = 2)
  }
}

# Splits between train and test
split <- function(df, train_percent = 40/60, case_percent = 25/40, control_percent = 15/40) {
  # Extract cases and controls
  cases <- subset(df, df$Class == 1)
  controls <- subset(df, df$Class == 0)
  N <- nrow(df)
  N_train <-round(N*train_percent)
  case  <- round(N_train*case_percent)
  control  <- N_train-case
  
  # Sample specified number of cases and controls for both train and test sets
  
  train_cases <- data.frame(cases[sample(1:nrow(cases), case), ])
  train_controls <- data.frame(controls[sample(1:nrow(controls), control), ])
  
  # Combine train and test sets
  train_set <- rbind(train_cases, train_controls)
  test_set <- df[!(rownames(df) %in% rownames(train_set)),]
  
  # Return the train and test sets
  return(list(train_set = train_set, test_set = test_set))
}