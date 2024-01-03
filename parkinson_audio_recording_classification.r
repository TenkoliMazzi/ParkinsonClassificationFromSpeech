# install.packages("devtools")
# library("devtools")
# install_github("kassambara/factoextra")
# install.packages("scatterplot3d") 
# install.packages("e1071")

library("corrr")
library("ggcorrplot")
library("FactoMineR")
library("factoextra")
library("scatterplot3d") # load
library("e1071")
features <- c(
  "PatientID","Jitter (local)", "Jitter (local, absolute)", "Jitter (rap)", "Jitter (ppq5)", "Jitter (ddp)",
  "Shimmer (local)", "Shimmer (local, dB)", "Shimmer (apq3)", "Shimmer (apq5)", "Shimmer (apq11)", "Shimmer (dda)",
  "AC", "NTH", "HTN",
  "Median pitch", "Mean pitch", "Standard deviation", "Minimum pitch", "Maximum pitch",
  "Number of pulses", "Number of periods", "Mean period", "Standard deviation of period",
  "Fraction of locally unvoiced frames", "Number of voice breaks", "Degree of voice breaks","UPDRS", "Class"
)
categorical <- c("PatientID","Class")
predictive <- c("UPDRS")
features_not_to_analize <- c(categorical,predictive)
non_predictive <- features[!(features %in% predictive )]
features_to_analize <- features[!(features %in% features_not_to_analize )]


found <- function(x) is.logical(x == x) && length(x == x) == 1 && !is.na(x == x) 


# read file as dataframe
file_path <- 'train_data.txt'  # Specify the file path
df <- read.table(file_path, header = FALSE, sep =",")  # Read the file into a dataframe
colnames(df) <- features  # Set column names to the predefined 'features'

# handling nans
na_sum <- colSums(is.na(df))  # Sum the NaN values per column
na_columns <- names(na_sum[na_sum != 0])  # Identify columns with NaN values

if (length(na_columns) > 0) {
  print(df[, na_columns])  # Print columns with missing values if any
} else {
  print("Nessun valore mancante nelle colonne.")  # Print message if no missing values
}
head(df)  # Display the first few rows of the dataframe

# normalizing data
df_normalized <- subset(df, select = non_predictive)  # Select non-predictive columns
data_normalized <- as.data.frame(scale(subset(df, select = features_to_analize)))  # Normalize 'to_analize' columns
df_normalized[features_to_analize] <- data_normalized  # Replace original columns with normalized ones

# correlation between variables
corr_matrix <- cor(data_normalized)  # Calculate the correlation matrix
ggcorrplot(corr_matrix)  # Plotting the correlation matrix

# drop features with correlation > tresh
tresh <- .9  # Set the correlation threshold

overcorr_cells_indexes <- which(abs(corr_matrix) > tresh, arr.ind = TRUE)  # Find cells with correlation above threshold as cell indexes
num_of_overcorr = dim(overcorr_cells_indexes)[1]  # Count the number of overcorrelations
not_overcorrelated_features <- vector("list", length = length(features_to_analize))  # Initialize a list to store the features names, the ones which are overcorraleted, will be removed. 
names(not_overcorrelated_features) <- features_to_analize  # Set names 

for (i in 1:num_of_overcorr) {
  checking <- features_to_analize[overcorr_cells_indexes[i, 1]]  # Identify the first feature in the pair
  correlated_column <- features_to_analize[overcorr_cells_indexes[i, 2]]  # Identify the second feature in the pair
  
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
overcorrelated_features_names <- features_to_analize[!features_to_analize %in% not_overcorrelated_features_names]  # Identify overcorrelated features


# discard overcorrelated
data_filtered <- subset(df_normalized,select=not_overcorrelated_features_names)
corr_matrix_filtered <- cor(data_filtered)
ggcorrplot(corr_matrix_filtered)

# pca analysis
data.pca <- princomp(corr_matrix)
data_filtered.pca <- princomp(corr_matrix_filtered)
summary(data_filtered.pca)
num_of_pc = 2

projections_coefficients <- as.matrix(data_filtered.pca$loadings[, 1:num_of_pc])
fviz_eig(data_filtered.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "blue")
fviz_pca_var(data_filtered.pca, col.var = "blue",title="Variables (after removing over correlations) - PCA")


fviz_cos2(data_filtered.pca, choice = "var", axes = 1:num_of_pc)


df_filtered <- subset(df_normalized,select = !(colnames(df_normalized) %in% overcorrelated_features_names))
to_project <- as.matrix(df_filtered[not_overcorrelated_features_names])
plot_colors <- df_filtered["Class"]
plot_colors[plot_colors == 0] <- "blue"
plot_colors[plot_colors == 1] <- "red"
pca_projections <- to_project %*% projections_coefficients
if(num_of_pc == 3)scatterplot3d(pca_projections,color = plot_colors[,"Class"])
if(num_of_pc == 2)plot(pca_projections,col = plot_colors[,"Class"],main = "PCA Projections (Train Set)")
legend("topleft", legend=c("Case", "Control"),inset=.02,
       fill=c("red", "blue"), cex=.9)

fviz_pca_var(data_filtered.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)


#Now we do the same thing but mean over the patients :
mean_pca_projection <- data.frame(matrix(1,max(df["PatientID"]),num_of_pc+1),row.names = 1:max(df["PatientID"]))
colnames(mean_pca_projection)[num_of_pc+1] <- "Class"
color_sequence = array()
for(i in 1:max(df["PatientID"])){
  mean_pca_projection[i,1:num_of_pc] <- colMeans(pca_projections[which(df["PatientID"] == i),])
  mean_pca_projection[i,"Class"] <- df[df["PatientID"] == i,"Class"][1]
  color_sequence[i] <- ifelse(mean_pca_projection[i,"Class"] == 1,"red", "blue")
}
if(num_of_pc == 3)scatterplot3d(mean_pca_projection[1:num_of_pc],color =color_sequence)
if(num_of_pc == 2)plot(mean_pca_projection[1:num_of_pc],col = color_sequence,pch=19,main = "Mean of PCA Projections (Train Set)")
legend("topleft", legend=c("Case", "Control"),inset=.02,
       fill=c("red", "blue"), cex=.9)

# applying SVM
svmfit = svm(Class ~ ., data = mean_pca_projection  , kernel = "linear",type ='C-classification',)
X1 = seq(min(mean_pca_projection[, 1])-.2, max(mean_pca_projection[, 1])+.2, by = 0.08)
X2 = seq(min(mean_pca_projection[, 2])-.2, max(mean_pca_projection[, 2])+.2, by = 0.08)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('X1', 'X2')
y_grid = predict(svmfit, grid_set)
plot(mean_pca_projection[, -3], main = 'SVM Linear Kernel (Train set)',
     xlab = 'Comp1', ylab = 'Comp2',
     xlim = range(X1), ylim = range(X2))

# contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE,pch = 1)
points(grid_set, pch = '-', col = ifelse(y_grid  == 1, 'tomato', 'royalblue3'))
points(mean_pca_projection, pch = 21, bg = ifelse(mean_pca_projection[, 3] == 1, 'red3', 'blue'))
legend("topleft", legend=c("Case", "Control"),inset=.02,
       fill=c("red", "blue"), cex=.9)

# applying SVM
svmfit = svm(Class ~ ., data = mean_pca_projection  , kernel = "radial",type ='nu-classification')
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('X1', 'X2')
y_grid = predict(svmfit, grid_set)
plot(mean_pca_projection[, -3], main = 'SVM Radial Kernel (Train set)',
     xlab = 'Comp1', ylab = 'Comp2',
     xlim = range(X1), ylim = range(X2))
# contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE,pch = 1)
points(grid_set, pch = '-', col = ifelse(y_grid  == 1, 'tomato', 'royalblue3'))
points(mean_pca_projection, pch = 21, bg = ifelse(mean_pca_projection[, 3] == 1, 'red3', 'blue'))
legend("topleft", legend=c("Case", "Control"),inset=.02,
       fill=c("red", "blue"), cex=.9)
