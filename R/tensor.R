#' Folding Matrix to Tensor by Mode.
#'
#' X_i_jk => X_i_j_k, X_j_ki => X_i_j_k, X_k_ij => X_i_j_k
#'
#' @param X Data matrix to fold.
#' @param mode Mode of operation.
#' @param shape Dimension of original tensor.
#'
#' @return X_i_j_k Three-mode tensor.
#' @export
#'
#' @examples
#' X_i_jk = generate_dataset()$X_i_jk
#' X_i_j_k = fold(X_i_jk, mode=1, shape=c(I=8,J=5,K=4)) # X_i_j_k
#'
fold = function(X, mode, shape){

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

#' Tensor Matricization
#'
#' Unfold/Matricize tensor. convert matrix to tensor by mode.
#'
#' @param tensor Three-mode tensor array.
#' @param mode Mode of operation.
#'
#' @return Matrix
#' @export
#'
#' @examples
#' X_i_jk = generate_dataset()$X_i_jk
#' X_i_j_k = fold(X_i_jk, mode=1, shape=c(I=8,J=5,K=4))
#' unfold(X_i_j_k, mode=1) # X_i_jk
#'
unfold = function(tensor, mode){

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


