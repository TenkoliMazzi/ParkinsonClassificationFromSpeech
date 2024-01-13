
# Principal Component Analysis (PCA)
perform_pca <- function(data,corr_mat, num_of_pc, to_analyze_cols, show_analysis = FALSE) {
  
  data.pca <- princomp(corr_mat)
  summary(data.pca)
  if(show_analysis){
    fviz_eig(data.pca, addlabels_features = TRUE)
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