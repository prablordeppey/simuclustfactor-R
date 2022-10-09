#' Folding matrix back to tensor by mode.
#'
#' X_i_jk => X_i_j_k, X_j_ki => X_i_j_k, X_k_ij => X_i_j_k
#'
#' @param X data matrix to fold
#' @param mode mode of operation
#' @param shape dimension of original tensor
#'
#' @return tensor
#' @export
#'
#' @examples
#' >> X_i_jk = generate_dataset()$X_i_jk
#' >> Fold(X_i_jk, mode=1, shape=c(I=8,J=5,K=4)) # X_i_j_k
#' >> Fold(X_j_ki, mode=2, shape=c(I=8,J=5,K=4)) # X_i_j_k
#' >> Fold(X_k_ij, mode=3, shape=c(I=8,J=5,K=4)) # X_i_j_k
Fold = function(X, mode, shape){

  I = shape[1]
  J = shape[2]
  K = shape[3]

  folded = array(dim = c(I,J,K))

  # (I,JK) => (I,J,K)
  if (mode == 1){
    for (k in 1:K){
        folded[,,k] = X[,(J*(k-1)+1):(J*k)]
    }
  }

  # (J,KI) => (I,J,K)
  if (mode==2){
    for (k in 1:K){
      for (i in 1:I){
        folded[i,,k] = X[,(K*(i-1)+k)]
      }
    }
  }

  # (K,IJ) => (I,J,K)
  if (mode==3){
    # matricizing each face and stacking ontop
    for (k in 1:K){
      folded[,,k] = matrix(X[k,], nrow = I, ncol = J)
    }
  }

  return(folded)
}

#' Tensor matricization
#'
#' Unfold/matricize tensor. convert matrix to tensor by mode.
#'
#' @param tensor three-mode tensor array
#' @param mode mode of operation
#'
#' @return matrix
#' @export
#'
#' @examples
#' >> X_i_jk = generate_dataset()$X_i_jk
#' >> X_i_j_k = Fold(X_i_jk, mode=1, shape=c(I=8,J=5,K=4))
#' >> Unfold(X=X_i_j_k, mode=1) # X_i_jk
#' >> Unfold(X=X_i_j_k, mode=2) # X_j_ki
#' >> Unfold(X=X_i_j_k, mode=3) # X_k_ij
Unfold = function(tensor, mode){

  I = dim(tensor)[1]
  J = dim(tensor)[2]
  K = dim(tensor)[3]

  # (I,J,K) => (I,JK)
  if (mode==1){
    unfolded = array(dim = c(I,J*K,1))
    for (k in 1:K){
      unfolded[,(J*(k-1)+1):(J*k),1] = tensor[,,k]
    }
  }

  # (I,J,K) => (J,KI)
  if (mode==2){

    unfolded = array(dim = c(J,K*I,1))

    for (i in 1:I){

      # create face with rows transposed for each occasion
      face = array(dim = c(J,K,1))
      for (k in 1:K){
        face[,k,1] = tensor[i,,k]
      }

      # main result update
      unfolded[,(K*(i-1)+1):(K*i),1] = face[,,1]
    }
  }

  # (I,J,K) => (K,IJ)
  if (mode==3){
    unfolded = array(dim = c(K,I*J,1))
    for (k in 1:K){
      unfolded[k,,1] = matrix(tensor[,,k])
    }
  }

  return(unfolded[,,1])
}


