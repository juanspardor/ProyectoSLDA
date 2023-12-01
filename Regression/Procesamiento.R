#Procesamiento de Datos
summary(trainReg) #Revisando los datos, vemos que todos los valores de V4 son 0
#Entonces vale la pega eliminarlos
trainReg = trainReg[,-4]
head(trainReg)

matrizCorrelaciones = cor(trainReg)
library(corrplot)
corrplot(matrizCorrelaciones, method = "color")


#Dado que las covarianzas son informacion redundante, y vienen en parejas usamos
#el siguiente algoritmo de Khun:
# 1. Calculate the correlation matrix of the predictors.
# 2. Determine the two predictors associated with the largest absolute pairwise
# correlation (call them predictors A and B).
# 3. Determine the average correlation between A and the other variables.
# Do the same for predictor B.
# 4. If A has a larger average correlation, remove it; otherwise, remove predic-
#   tor B.
# 5. Repeat Steps 2–4 until no absolute correlations are above the threshold.

#Para eso usamos una funcion
remove_high_correlation <- function(data, threshold = 0.8) {
  while (TRUE) {
    # Calculate the correlation matrix of the predictors
    corr_matrix <- cor(data)
    
    # Set the diagonal elements to zero to exclude self-correlations
    diag(corr_matrix) <- 0
    
    # Find the maximum absolute correlation
    max_corr <- max(abs(corr_matrix))
    
    # Exit the loop if no absolute correlations are above the threshold
    if (max_corr < threshold) {
      break
    }
    
    # Find the indices of the maximum absolute correlation
    indices <- which(abs(corr_matrix) == max_corr, arr.ind = TRUE)
    A <- rownames(corr_matrix)[indices[1, 1]]
    B <- colnames(corr_matrix)[indices[1, 2]]
    
    # Check if the correlation is exactly 1
    if (abs(max_corr - 1) < 1e-10) {
      warning("Variables with correlation equal to 1 found: ", A, " and ", B)
      break  # Exit the loop to avoid an infinite loop
    }
    
    # Determine the average correlation between A and the other variables
    avg_corr_A <- mean(abs(corr_matrix[A, ]))
    
    # Do the same for predictor B
    avg_corr_B <- mean(abs(corr_matrix[B, ]))
    
    # If A has a larger average correlation, remove it; otherwise, remove predictor B
    if (avg_corr_A > avg_corr_B) {
      data <- data[, !(colnames(data) %in% A)]
    } else {
      data <- data[, !(colnames(data) %in% B)]
    }
  }
  
  return(data)
}


#Datos en X pre limpieza
preX = trainReg[,-1]


datosNuevos = remove_high_correlation(preX, 0.5)
dim(datosNuevos)
head(datosNuevos)

matrizNueva = cor(datosNuevos)
corrplot(matrizNueva, method = "color")

datosY = trainReg$V1
matrizNueva
combined_df <- cbind(datosY, datosNuevos)



