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

source("SupportVectorMachine.R")  
source("pca_tools.R")  
source("dataset_tools.R")


# Mean projections for a patient (looping over the column "main_label"), over every kind of voice sample 
extract_features <- function(dframe,to_analyze_cols,samples_class_name = "SampleClass", main_id = "PatientID",main_label= "Class"){
    # you can leave samples_class = -1 where you don't want a sample to be analized
    N_obs = nrow(dframe)
    M_obs = length(to_analyze_cols)
    
    ids_of_patients = unique(dframe[main_id])
    N_patients <- max(ids_of_patients) 
    M_patients <- M_obs
  
    
    # by selecting rownames(ids), is taken the row of the first apparence of that patient in the df,then with main_label, the class of that patient is selected.
    classes_of_patients <- dframe[rownames(ids_of_patients),main_label]
    
    # initialize an empty matrix that contains the new features for each patient + 1 column which will store the number of samples
    new_features_matrix <- matrix(0,N_patients,M_patients + 1)
    
    # creates dataframes storing the ids, the features, the count of samples and the class of each patient
    
    mean_for_patient = cbind(new_features_matrix,classes_of_patients)
    colnames(mean_for_patient) <- c(to_analyze_cols,"SamplesCount",main_label)
    rownames(mean_for_patient) <- 1:N_patients
    
    std_for_patient = cbind(new_features_matrix,classes_of_patients)
    colnames(std_for_patient) <- c(to_analyze_cols,"SamplesCount",main_label)
    rownames(std_for_patient) <- 1:N_patients
    
    energy_for_patient = cbind(new_features_matrix,classes_of_patients)
    colnames(energy_for_patient) <- c(to_analyze_cols,"SamplesCount",main_label)
    rownames(energy_for_patient) <- 1:N_patients
   
    rms_for_patient = cbind(new_features_matrix,classes_of_patients)
    colnames(rms_for_patient) <- c(to_analyze_cols,"SamplesCount",main_label)
    rownames(rms_for_patient) <- 1:N_patients
   
    for(i in 1:N_patients){
      rows_of_patient_in_dframe <- dframe[main_id] == i
      num_samp<- sum(rows_of_patient_in_dframe)
      
      mean_for_patient[i,to_analyze_cols] <- colMeans(dframe[rows_of_patient_in_dframe,to_analyze_cols])
      mean_for_patient[i,"SamplesCount"] <- num_samp
      
      std_for_patient[i,to_analyze_cols] <- apply(dframe[rows_of_patient_in_dframe,to_analyze_cols],2,sd)
      std_for_patient[i,"SamplesCount"] <- num_samp
      
      energy_for_patient[i,to_analyze_cols] <- colMeans(dframe[rows_of_patient_in_dframe,to_analyze_cols]^2)
      energy_for_patient[i,"SamplesCount"] <- num_samp
      
      rms_for_patient[i,to_analyze_cols] <- sqrt(colMeans(dframe[rows_of_patient_in_dframe,to_analyze_cols]^2))
      rms_for_patient[i,"SamplesCount"] <- num_samp
    }
    
    new__features <- {
        list(
          mean = as.data.frame(mean_for_patient),
          std = as.data.frame(std_for_patient),
          energy = as.data.frame(energy_for_patient),
          rms = as.data.frame(rms_for_patient)
      )
    }
    
    return(new__features)
}

evaluate_feature_reduction_svm <- function(train_set,test_set,to_analyze_cols){
    svmmodel<- SupportVectorMachine
    corr_matrix <- cor(subset(train_set,select=to_analyze_cols) )
    projections_coefficients <- perform_pca(train_set,corr_matrix,2,to_analyze_cols,TRUE)
    train_projections <- project(train_set[to_analyze_cols],projections_coefficients)
    svmmodel <- train(svmmodel, cbind(train_projections,train_set["Class"]))
    test_projections <-project(test_set[to_analyze_cols],projections_coefficients)
    svmmodel <- test(svmmodel, cbind(test_projections,test_set["Class"]))
    return(svmmodel)
}


# Install and load libraries
install_and_load_libraries()

# Define features in the legacy train set
legacy_features_train <- c(
  "PatientID","Jitter (local)", "Jitter (local, absolute)", "Jitter (rap)", "Jitter (ppq5)", "Jitter (ddp)",
  "Shimmer (local)", "Shimmer (local, dB)", "Shimmer (apq3)", "Shimmer (apq5)", "Shimmer (apq11)", "Shimmer (dda)",
  "AC", "NTH", "HTN",
  "Median pitch", "Mean pitch", "Standard deviation", "Minimum pitch", "Maximum pitch",
  "Number of pulses", "Number of periods", "Mean period", "Standard deviation of period",
  "Fraction of locally unvoiced frames", "Number of voice breaks", "Degree of voice breaks","UPDRS", "Class"
)
# Define features in the legacy test set
legacy_features_test <- setdiff(legacy_features_train,"UPDRS")
  
# Define features added by user
added_features = c("SampleClass")

# Define all the features which will not be treated as numerical values, so won't be learned by models
labels_features <- c("PatientID", "Class","SampleClass")

# Define all the features which will be treated as predictions (labels or targets), so won't be learned by models 
predictive_features <- c("UPDRS","Class","SampleClass")

# Only the features listed here will be used to teach the models
train_features <- {
  x <- c(legacy_features_train,added_features)
  x <- setdiff(x,labels_features)
  x <- setdiff(x,predictive_features)
}

# Only the features listed here will be used to test the models
test_features <- {
  intersect(predictive_features,labels_features)
}

# This is the feature that will be used as an id
id_feature <- "PatientID"


train_samples <- c("A", "O", "U", 4:26)
test_samples <- c(rep("A", 3), rep("O", 3))

# Read data
data_train <- read_data('train_data.txt', legacy_features_train)
data_test <- read_data('test_data.txt', legacy_features_test)

data_train[added_features] <- {
  rep(train_samples,nrow(data_train)/length(train_samples))
}

data_test[added_features] <- {
  rep(test_samples,nrow(data_test)/length(test_samples))
}

data_test[id_feature] <- data_test[id_feature] + max(data_train[id_feature])

# Merge the two datasets
df<- rbind.data.frame(subset(data_train,select = c(id_feature,train_features,test_features)),
                      subset(data_test,select=c(id_feature,train_features,test_features)))

# Deal with the presence of different samples regarding the same patient
datasets <- extract_features(df,train_features)
names(datasets) <- c("Mean","Std","Energy","RMS")

for(i in 1:length(datasets)){
  mean_score <- 0
  mean_accuracy <- 0
  mean_precision <- 0
  mean_sensitivity <- 0
  mean_RMS <- 0
  iterations <- 25
  for(j in 1:iterations){
    df <- datasets[[i]]
    split_data <- split(df,60/100)
    # Access train and test sets
    df_train <- split_data$train_set
    df_test <- split_data$test_set
    colnames(df_train) <- colnames(df)
    colnames(df_test) <- colnames(df)
    
    df_train_normalized <- normalize_data(df_train, train_features)
    df_test_normalized <- normalize_data(df_test, train_features)
    output <- evaluate_feature_reduction_svm(df_train_normalized,df_test_normalized,train_features)
    
    mean_accuracy <- mean_accuracy + getTestMetric(output,"accuracy")
    mean_precision <- mean_precision + getTestMetric(output,"precision")
    mean_sensitivity <- mean_sensitivity + getTestMetric(output,"sensitivity")
    mean_score <- mean_score + getTestScore(output)
  }
  show(output,names(datasets)[i])
  mean_accuracy <- round(mean_accuracy/iterations,4)
  mean_precision <- round(mean_precision/iterations,4)
  mean_sensitivity <- round(mean_sensitivity/iterations,4)
  mean_score <- round(mean_score/iterations,4)
  print("-----------------------------------------")
  print(paste("Mean results for feature", names(datasets)[i], ":"))
  print(paste("Accuracy :",mean_accuracy))
  print(paste("Precision :",mean_precision))
  print(paste("Sensitivity :",mean_sensitivity))
  print(paste("Cumulative Score :",mean_score))
}



