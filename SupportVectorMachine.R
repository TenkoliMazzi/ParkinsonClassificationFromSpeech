# setClass(
#     "Model",
#     slots = list(
#     accuracy =  "numeric",
#     sensitivity =  "numeric",
#     precision =  "numeric",
#     score =  "numeric",
#     model =  "list",
#     model_test =  "data.frame",
#     model_train =  "data.frame",
#     predictions = "list",
#     treshold = "numeric",
#     conf_mat = "matrix"
#   ),
#     prototype = list(
#     accuracy =  0,
#     sensitivity =  0,
#     precision =  0,
#     score =  0,
#     model =  NULL,
#     model_test =  NULL,
#     model_train =  NULL,
#     predictions = NULL,
#     treshold = 0,
#     conf_mat = "",
#   )
# )
SupportVectorMachine <- list(
    model =  NULL,
    dataset_train =  NULL,
    dataset_test =  NULL,
    dataset_train_labels = NULL,
    dataset_test_labels = NULL,
    predictions_train = NULL,
    predictions_test = NULL,
    treshold = 0,
    conf_mat_train = NULL,
    conf_mat_test = NULL,
    kernel = "radial",
    classification_type = "nu-classification",
    metrics = c("accuracy","precision","sensitivity")
)
class(SupportVectorMachine) <- "SupportVectorMachine"

train <- function(obj,dataset){
  UseMethod("train")
}
test <- function(obj,dataset){
  UseMethod("test")
}
show <- function(obj,additional_label = ""){
  UseMethod("show")
}
getTrainMetric <- function(obj,metric){
  UseMethod("getTrainMetric")
}
getTestMetric <- function(obj,metric){
  UseMethod("getTestMetric")
}
getTestScore <- function(obj){
  UseMethod("getTestScore")
}

confusion_matrix <- function(y_pred,y){

  
  pp <- sum(y_pred == "1")
  
  pn <- sum(y_pred == "0")
  tp <- sum(y_pred == "1" &  y == 1)
  tn <- sum(y_pred == "0" & y == 0)
  
  fp <- pp-tp
  
  fn <- pn-tn
  return(matrix(c(tn,fn,fp,tp),2,2))
}

plot_svm <- function(svmmodel,data,y,label){
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


train.SupportVectorMachine  <- function(obj,dataset){
 
  obj$dataset_train <- dataset[setdiff(colnames(dataset),c("Class"))]
  obj$dataset_train_labels <- dataset["Class"]
 
  obj$model <- svm(
                  Class ~ ., 
                  data = dataset, 
                  kernel = obj$kernel,
                  type = obj$classification_type
                )
  obj$predictions_train <- predict(obj$model,obj$dataset_train)
  obj$conf_mat_train <- confusion_matrix(obj$predictions_train,obj$dataset_train_labels)
  return(obj)
}

test.SupportVectorMachine  <- function(obj,dataset){
  obj$dataset_test <- dataset[setdiff(colnames(dataset),"Class")]
  obj$dataset_test_labels <- dataset["Class"]
 
  obj$predictions_test <- predict(obj$model,obj$dataset_test)
  obj$conf_mat_test <- confusion_matrix(obj$predictions_train,obj$dataset_train_labels)
  return(obj)
}

getTrainMetric.SupportVectorMachine <- function(obj,metric){
  if(!metric %in% obj$metrics){
    print(paste("Metric '",metric,"' is not defined for this model"))
    print("Aviable metrics are :")
    print(obj$metrics)
  }
  return(metric(obj$conf_mat_train))
}

getTestMetric.SupportVectorMachine <- function(obj,metric){
  if(!metric %in% obj$metrics){
    print(paste("Metric '",metric,"' is not defined for this model."))
    print("Aviable metrics are :")
    print(obj$metrics)
    return(0)
  }
  if(is.null(obj$conf_mat_test)){
    print(obj)
    print("You have to test the model before getting any test metric.")
    return(0)
  }
  
  return(get(metric)(obj$conf_mat_test))
}
getTestScore.SupportVectorMachine <- function(obj){
  return(getTestMetric(obj,"accuracy")+getTestMetric(obj,"precision")+getTestMetric(obj,"sensitivity"))
}

accuracy <- function(conf_mat){
  return(round((conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat),3 ))
}
precision <- function(conf_mat){
  return(round(conf_mat[2,2]/(conf_mat[1,2]+conf_mat[2,2]),3))
}
sensitivity <- function(conf_mat){
  return(round(conf_mat[2,2]/(conf_mat[2,1]+conf_mat[2,2]),3))
}

show.SupportVectorMachine <- function(obj, additional_label =""){
  plot_svm(obj$model,obj$dataset_train,obj$dataset_train_labels,paste("SVM Train",additional_label))
  plot_svm(obj$model,obj$dataset_test,obj$dataset_test_labels,paste("SVM Test",additional_label))
}
  
  
