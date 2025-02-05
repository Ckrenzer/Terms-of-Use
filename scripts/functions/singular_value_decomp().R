# Singular Value Decomposition--requires a matrix as input
singular_value_decomp <- function(A, singular_values_only = FALSE){
  # EIGENVALUES AND EIGENVECTORS -------------------------------------------
  # The eigenvalues and eigenvectors
  # are already in descending order--no
  # need to sort the values
  eigs <- eigen(t(A) %*% A, symmetric = TRUE)
  
  
  # adjusting for tolerance--ATA matrices are positive definite
  # so we don't have to worry about losing negative eigenvalues
  eig_vals <- eigs$values[eigs$values > 1e-12]
  eig_vecs <- eigs$vectors[, eigs$values > 1e-12, drop = FALSE]
  
  
  # SIGMA ------------------------------------------------------------------
  # the singular values
  singular_values <- sqrt(eig_vals)
  
  
  # optional choice to end the function early and only return
  # singular values
  if(singular_values_only){
    return(list(singular_values = singular_values)) 
  }
  
  
  # the dimensions of sigma
  dimensions <- length(singular_values)
  # An all-zero square matrix of the appropriate dimensions
  sigma <- matrix(nrow = dimensions, ncol = dimensions, data = 0)
  
  # inserting singular values along the diagonal
  for(i in 1:dimensions){
    sigma[i, i] <- singular_values[i]
  }
  
  
  # V-TRANSPOSE ------------------------------------------------------------
  v_transpose <- t(eig_vecs)
  
  
  
  # U ----------------------------------------------------------------------
  U <- matrix(nrow = nrow(A), ncol = dimensions, data = 0.0)
  A_eigs = A %*% eig_vecs;
  for(i in 1:ncol(A)){
    U[, i] <- A_eigs[, i] / singular_values[i];
  }
  
  # providing the words for a word similarity search
  colnames(U) <- colnames(A)
  rownames(U) <- rownames(A)
  
  
  return(list(u = U, sigma = sigma, v_transpose = v_transpose, singular_values = singular_values))  
}
