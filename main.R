## Install and load required libraries
install_and_load_libraries <- function() {
  # install.packages("corrr")
  # install.packages("ggcorrplot")
  # install.packages("FactoMineR")
  # install.packages("factoextra")
  # install.packages("scatterplot3d")
  # install.packages("e1071")
  
  library("corrr")
  library("ggcorrplot")
  library("FactoMineR")
  library("factoextra")
  library("scatterplot3d")
  library("e1071")
}

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
  data_normalized <- as.data.frame(scale(subset(df, select = to_analyze_cols)))
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

# Principal Component Analysis (PCA)
perform_pca <- function(data,corr_mat, num_of_pc, to_analyze_cols, show_analysis = FALSE) {
  
  data.pca <- princomp(corr_mat)
  summary(data.pca)
  if(show_analysis){
    fviz_eig(data.pca, addlabels = TRUE)
    fviz_pca_var(data.pca, col.var = "blue")
    fviz_cos2(data.pca, choice = "var", axes = 1:num_of_pc)
  }
      
  projections_coefficients <- as.matrix(data.pca$loadings[, 1:num_of_pc])
  
  return(projections_coefficients)
}
project <- function(data,coeffs){
  to_project <- as.matrix(data)
  return(to_project %*% coeffs)
}

# Visualization of PCA results
visualize_pca <- function(projections, y,label = "") {
  num_of_pc = ncol(projections)
  plot_colors <- as.matrix(y)
  plot_colors[plot_colors == 0] <- "blue"
  plot_colors[plot_colors == 1] <- "red"
  if(num_of_pc == 3){
      scatterplot3d(projections,color = plot_colors)
    return()
  }
  if(num_of_pc == 2){
    plot( x = projections,
          col = plot_colors,
          xlab = 'Comp1', ylab = 'Comp2',
          main = paste("PCA Projection",label))
    
    legend("topleft", 
           legend=c("Case", "Control"),
           inset=.02,
           fill=c("red", "blue"), 
           cex=.9)
    return()
  }
  print("Cannot visualize more than 3 principal components")
  return()
} 

# Mean PCA projections for a patient, over every kind of voice sample 
mean_over_samples <- function(data,dframe){
    # you can leave samples_class = -1 where you don't want a sample to be analized
    N = nrow(data)
    M = ncol(data)
    
    ids = unique(dframe["PatientID"])
  
    
    mean_for_patient = cbind(ids,matrix(0,length(ids),M+1),dframe[rownames(ids),"Class"])
    colnames(mean_for_patient) <- c("PatientID",colnames(data),"SamplesCount","Class")
    
    
    for(i in 1:N){
      p <- dframe[i,"PatientID"]
      if(dframe[i,"SampleClass"] != -1){
          row_to_update <- which(mean_for_patient["PatientID"] == p)
          mean_for_patient[row_to_update,2:(M+2)] <- mean_for_patient[row_to_update,2:(M+2)] + cbind(data[i,],array(1,length(ids)))
      }
    }

  
    
    
   
    mean_for_patient[,1:M+1] <- mean_for_patient[,1:M+1]/ mean_for_patient$SamplesCount
    return(mean_for_patient)
}

# SVM model training and visualization
train_svm <- function(data,y,kernel = "linear", type ='nu-classification',show = FALSE) {
  data <- as.data.frame(x=data)
  data["Class"] <- y
  svmmodel = svm(Class ~ ., data = data  , kernel = kernel,type = type)
  if(show) show_svm(svmmodel,data,y,"SVM Over Train Data")
  return(svmmodel)
}
test_svm <- function(svmmodel, data, y,show = TRUE) {
  
  if(show) show_svm(svmmodel,data,y,"SVM Over Test Data")
  return(as.matrix(predict(svmmodel,data)) == y)
}

confusion_matrix <- function(svmmodel,data,y){
  pred <- as.matrix(predict(svmmodel,data))

  pp <- sum(pred == "1")
 
  pn <- sum(pred == "0")
  tp <- sum(pred == "1" &  y == 1)
  tn <- sum(pred == "0" & y == 0)
 
  fp <- pp-tp
 
  fn <- pn-tn
  return(matrix(c(tn,fn,fp,tp),2,2))
}
show_svm <- function(svmmodel,data,y,label="SVM"){

  X1 = seq(min(data[, 1])-.2, max(data[, 1])+.2, by = .1)
  X2 = seq(min(data[, 2])-.2, max(data[, 2])+.2, by = .1)
  grid_set = expand.grid(X1, X2)

  colnames(grid_set) = c('Comp.1', 'Comp.2')
 
  y_grid = predict(svmmodel, grid_set)

  plot(data[, -3], main = label,
       xlab = 'Comp1', ylab = 'Comp2',
       xlim = range(X1), ylim = range(X2))
  
  plot_colors <- as.matrix(y)
  plot_colors[plot_colors == 0] <- "blue"
  plot_colors[plot_colors == 1] <- "red"
  point_colors <- as.matrix(y_grid) 
  point_colors[point_colors == 0] <- "blue4"
  point_colors[point_colors == 1] <- "red4"
  
  # contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE,pch = 1)
  points(grid_set, pch = 15, col = point_colors,cex = 5)
  points(data, pch = 21, bg = plot_colors)
  
  legend("topright", legend = c("Case", "Control"), inset = 0.02,
         fill = c("red", "blue"), cex = 0.9)
  
}
evaluate_feature_reduction_svm <- function(train,test,to_analyze_cols,over_corr_tresh_seq = seq(87,99, by = 1),out_model = FALSE){
    
    last_feat_number <- -1
    feat_number <-1
    best_accuracy <- 0
    best_sensitivity <- 0
    best_precision <- 0
    best_score <- 0
    best_model <- NULL
    best_model_test <- NULL
    best_model_train <- NULL
    best_treshold <-0
    for(over_cor_tresh in over_corr_tresh_seq){
      
      # Filter features with high correlation
      not_overcorrelated_features <- filter_high_correlation(train, over_cor_tresh/100 ,to_analyze_cols)
     
      corr_matrix_filtered_train <- cor(subset(train,select=not_overcorrelated_features))
      feat_number <- length(not_overcorrelated_features)
      if(last_feat_number != -1)if(last_feat_number == feat_number) next
      # Perform PCA
      projections_coefficients <- perform_pca(train,corr_matrix_filtered_train, 2,not_overcorrelated_features, TRUE)
     
      train_projections <- project(train[not_overcorrelated_features],projections_coefficients)
      visualize_pca(train_projections,train["Class"],"Train Set")
 
      # Train and visualize SVM with radial kernel
      # svmfit_linear <- train_svm(mean_projections, train_patients_class, "linear")
      svmfit_radial<- train_svm(train_projections, train["Class"], "radial",show= out_model)
    
      test_projections <-project(test[not_overcorrelated_features],projections_coefficients)
      visualize_pca(test_projections,test["Class"],"Test Set")
      
      correct <- test_svm(svmfit_radial,test_projections, test["Class"],show = out_model)
      last_feat_number <- feat_number
      conf_mat <- confusion_matrix(svmfit_radial,test_projections, test["Class"])
     
      accuracy <- (conf_mat[1,1]+conf_mat[2,2])/nrow(test_projections)
      precision <- conf_mat[2,2]/(conf_mat[2,1]+conf_mat[2,2])
      sensitivity <- conf_mat[2,2]/(conf_mat[1,2]+conf_mat[2,2])
      accuracy <- round(accuracy,3)
      precision <- round(precision,3)
      sensitivity <-  round(sensitivity,3)
      
      score <- sum(c(precision,accuracy,sensitivity))
     
      if(score > best_score){
        best_accuracy <- round(accuracy,3)
        best_sensitivity <- round(sensitivity,3)
        best_precision <- round(precision,3)
        best_treshold <- over_cor_tresh
        best_model <- svmfit_radial
        best_model_test <- cbind(test_projections,test["Class"])
        best_model_train <- cbind(train_projections,train["Class"])
     
        best_score <- score
      }
      
      print(paste("For over correlated feature treshold at", over_cor_tresh,": Accuracy =",accuracy, ", Precision = ",precision,", Sensitivity =",sensitivity))
      
    }
    if(out_model) {
      return(svmfit_radial)
    }
    
    print(paste("Best at", best_treshold,": Accuracy =",best_accuracy, ", Precision = ",best_precision,", Sensitivity =",best_sensitivity))
    show_svm(best_model,best_model_train[,1:2],best_model_train["Class"],"SVM Train")
    show_svm(best_model,best_model_test[,1:2],best_model_test["Class"],"SVM Test")
    return(best_model)
  
}
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
  
  # print(nrow(train_controls))
  # print(nrow(test_cases))
  # print(nrow(test_controls))
  
  
  # Combine train and test sets
  train_set <- rbind(train_cases, train_controls)
  test_set <- df[!(rownames(df) %in% rownames(train_set)),]
  
  
  # Sample the specified number of additional rows for the training set
  # additional_train_rows <- df[sample(setdiff(1:nrow(df), rownames(train_set)), train_cases - nrow(train_set)), ]
  # train_set <- rbind(train_set, additional_train_rows)
  
  # Return the train and test sets
  return(list(train_set = train_set, test_set = test_set))
}
# Main script

# Install and load libraries
install_and_load_libraries()

# Define features and data paths
features <- c(
  "PatientID","Jitter (local)", "Jitter (local, absolute)", "Jitter (rap)", "Jitter (ppq5)", "Jitter (ddp)",
  "Shimmer (local)", "Shimmer (local, dB)", "Shimmer (apq3)", "Shimmer (apq5)", "Shimmer (apq11)", "Shimmer (dda)",
  "AC", "NTH", "HTN",
  "Median pitch", "Mean pitch", "Standard deviation", "Minimum pitch", "Maximum pitch",
  "Number of pulses", "Number of periods", "Mean period", "Standard deviation of period",
  "Fraction of locally unvoiced frames", "Number of voice breaks", "Degree of voice breaks","UPDRS", "Class"
)
added_features = c("SampleClass")
categorical <- c("PatientID", "Class","SampleClass")
predictive <- c("UPDRS")
features_not_to_analyze <- c(categorical, predictive)
non_predictive <- c(features[!(features %in% predictive)])
with_samples <- c(non_predictive,"SampleClass")
features_to_analyze <- features[!(features %in% features_not_to_analyze)]

train_samples <- c("A", "O", "U", 4:26)
test_samples <- c(rep("A", 3), rep("O", 3))

# Read data
data_train <- read_data('train_data.txt', features)
data_test <- read_data('test_data.txt', non_predictive)
data_train["SampleClass"] <- rep(train_samples,nrow(data_train)/length(train_samples))
data_test["SampleClass"] <- rep(test_samples,nrow(data_test)/length(test_samples))
data_test["PatientID"] <- data_test["PatientID"] + max(data_train["PatientID"])
df<- rbind.data.frame(subset(data_train,select = with_samples),subset(data_test,select = with_samples))
df <- mean_over_samples(df[features_to_analyze],df)
split_data <- split(df,60/100)
# Access train and test sets
df_train <- split_data$train_set
df_test <- split_data$test_set
colnames(df_train) <- colnames(df)
colnames(df_test) <- colnames(df)

# Check for missing values
naCheck(df_train)
naCheck(df_test)

# Normalize data
df_train_normalized <- normalize_data(df_train, features_to_analyze)
df_test_normalized <- normalize_data(df_test, features_to_analyze)
best_svm <- evaluate_feature_reduction_svm(df_train_normalized,df_test_normalized,features_to_analyze)

# check_normal_distribution(df_train_normalized, df_train["Class"])



