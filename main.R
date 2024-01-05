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
normalize_data <- function(df, to_analyze_cols, non_predictive_cols) {
  df_normalized <- subset(df, select = non_predictive_cols)
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
visualize_pca <- function(projections, y) {
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
          main = "PCA Projections (Train Set)")
    
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
mean_over_samples <- function(data,patients_id,samples_class){
    # you can leave samples_class = -1 where you don't want a sample to be analized
  
    N = length(as.matrix(patients_id))
    P = max(patients_id)
    n_feat = ncol(data)
    mean_of_data <- data.frame(matrix(0,P,n_feat))
    num_samples_of_patient <- array(0,P)
    
    for(i in 1:N){
      p <- patients_id[i,]
      if(samples_class[i,] != -1){
          mean_of_data[p,] <- data[i,] + mean_of_data[p,]
          num_samples_of_patient[p] <- num_samples_of_patient[p] + 1
      }
    }
    mean_of_data <- mean_of_data/num_samples_of_patient
    return(mean_of_data)
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
show_svm <- function(svmmodel,data,y,label="SVM"){
  X1 = seq(min(data[, 1])-.2, max(data[, 1])+.2, by = .1)
  X2 = seq(min(data[, 2])-.2, max(data[, 2])+.2, by = .1)
  grid_set = expand.grid(X1, X2)
  colnames(grid_set) = c('X1', 'X2')
  
  y_grid = predict(svmmodel, grid_set)
  plot(data[, -3], main = label,
       xlab = 'Comp1', ylab = 'Comp2',
       xlim = range(X1), ylim = range(X2))
  
  plot_colors <- as.matrix(y)
  plot_colors[plot_colors == 0] <- "blue"
  plot_colors[plot_colors == 1] <- "red"
  point_colors <- as.matrix(y_grid) 
  point_colors[point_colors == 0] <- "blue3"
  point_colors[point_colors == 1] <- "red3"
  
  # contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE,pch = 1)
  points(grid_set, pch = 15, col = point_colors,cex = 5)
  points(data, pch = 21, bg = plot_colors)
  
  legend("topright", legend = c("Case", "Control"), inset = 0.02,
         fill = c("red", "blue"), cex = 0.9)
  
}
evaluate_feature_reduction_svm <- function(train,test,over_corr_tresh_seq = seq(80,99, by = 1),out_model = FALSE){
    
    last_feat_number <- -1
    feat_number <-1
    best <- c(0,0)
    for(over_cor_tresh in over_corr_tresh_seq){
      
      # Filter features with high correlation
      not_overcorrelated_features <- filter_high_correlation(train, over_cor_tresh/100 ,features_to_analyze)
      corr_matrix_filtered_train <- cor(subset(train,select=not_overcorrelated_features))
      feat_number <- length(not_overcorrelated_features)
      if(last_feat_number != -1)if(last_feat_number == feat_number) next
      
      # Perform PCA
      projections_coefficients <- perform_pca(train,corr_matrix_filtered_train, 2,not_overcorrelated_features, TRUE)
      train_projections <- project(train[not_overcorrelated_features],projections_coefficients)
      # visualize_pca(train_projections,train["Class"])
      
      # Mean PCA projections over patients
      class_to_consider <- df_train["SampleClass"]
      # class_to_consider[class_to_consider != "A"] <- -1
      # class_to_consider[train["SampleClass"] == "O"] <- "O"
      mean_projections <- mean_over_samples(train_projections,train["PatientID"], class_to_consider)
      # visualize_pca(mean_projections,train_patients_class)
      
      
      # Train and visualize SVM with linear kernel
      # svmfit_linear <- train_svm(mean_projections, train_patients_class, "linear")
      
      # Train and visualize SVM with radial kernel
      svmfit_radial<- train_svm(mean_projections, train_patients_class, "radial",show=out_model)
      test_projections <-project(test[not_overcorrelated_features],projections_coefficients)
      mean_test_projections <- mean_over_samples(test_projections,test["PatientID"],test["SampleClass"])
      
      correct <- test_svm(svmfit_radial,mean_test_projections, test_patients_class,show = out_model)
      last_feat_number <- feat_number
      accuracy <- sum(correct)/nrow(mean_test_projections)
      if(accuracy > best[1]){
        best[1] <- accuracy
        best[2] <- over_cor_tresh
      }
      if(!out_model)print(paste("For treshold at", over_cor_tresh,": Accuracy =",accuracy))
      
    }
    if(out_model) {
      return(svmfit_radial)
    }
    print(paste("Best : treshold = ",best[2],"Accuracy =",best[1]))
    return(evaluate_feature_reduction_svm(train,test,over_corr_tresh_seq = seq(best[2],best[2]),out_model = TRUE))
  
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
non_predictive <- features[!(features %in% predictive)]
features_to_analyze <- features[!(features %in% features_not_to_analyze)]

train_samples <- c("A", "O", "U", 4:26)
test_samples <- c(rep("A", 3), rep("O", 3))

# Read data
df_train <- read_data('train_data.txt', features)
df_test <- read_data('test_data.txt', non_predictive)
df_train["SampleClass"] <- rep(train_samples,length(df_train)/length(train_samples))
df_test["SampleClass"] <- rep(test_samples,length(df_test)/length(test_samples))
train_patients_class <- array()
test_patients_class <- array()
for(i in 1:max(df_train["PatientID"])){
  train_patients_class[i] <- df_train[df_train["PatientID"] == i,"Class"][1]
}
for(i in 1:max(df_test["PatientID"])){
  test_patients_class[i] <- df_test[df_test["PatientID"] == i,"Class"][1]
}
# Check for missing values
naCheck(df_train)
naCheck(df_test)

# Normalize data
df_train_normalized <- normalize_data(df_train, features_to_analyze, non_predictive)
df_test_normalized <- normalize_data(df_test, features_to_analyze)
best_svm <- evaluate_feature_reduction_svm(df_train_normalized,df_test_normalized)



