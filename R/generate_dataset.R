
#' Three-Mode Dataset Generator for Simulations
#'
#' Generate G clustered synthetic dataset of I objects measured on J variables
#' for K occasions with additive noise.
#'
#' @param I Number of objects.
#' @param J Number of variables per occasion.
#' @param K Number of occasions.
#' @param G Number of clusters.
#' @param Q Number of factors for the variables.
#' @param R Number of factors for the occasions.
#' @param noise_mean Mean of noise to generate.
#' @param noise_stdev Noise effect level/spread/standard deviation.
#' @param seed Seed for random sequence generation.
#' @param centroids_spread interval from which to uniformly pick the centroids.
#'
#' @returns Z_i_jk: Component scores in the full space.
#' @returns E_i_jk: Generated noise at the given noise level.
#' @returns X_i_jk: Dataset with noise level set to stdev specified.
#' @returns Y_g_qr: Centroids matrix in the reduced space.
#' @returns U_i_g: Stochastic membership function matrix.
#' @returns B_j_q: Objects component scores matrix.
#' @returns C_k_r: Occasions component scores matrix.
#'
#' @importFrom stats rnorm
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' generate_dataset(seed=0)
#'
generate_dataset = function(I=8,J=5,K=4,G=3,Q=3,R=2, centroids_spread=c(0,1), noise_mean=0, noise_stdev=0.5, seed=NULL){

  set.seed(seed)

  # 8x3
  U_i_g_ = generate_rmfm(I=I, G=G, seed=seed)

  # 6x3
  B_j_q_ = generate_rmfm(I=J, G=Q, seed = seed)  # contruct membership matrix
  weights = 1/colSums(B_j_q_)**0.5  # compute weights for each factor
  facts = col(B_j_q_)[which(B_j_q_==1)]  # get corresponding factors for each var
  B_j_q_[B_j_q_==1] = weights[facts]  # update weight of var-factor entry

  # 4x2
  C_k_r_ = generate_rmfm(I=K, G=R, seed = seed)  # contruct membership matrix
  weights = 1/colSums(C_k_r_)**0.5  # compute weights for each occasion factor
  facts = col(C_k_r_)[which(C_k_r_==1)]  # get corresponding factors for each occasion
  C_k_r_[C_k_r_==1] = weights[facts]  # update weight of occasion-factor entry

  Y_g_qr_ = matrix(runif(n=G*Q*R, min=centroids_spread[1], max=centroids_spread[2] ), nrow = G) # 3x6

  # 8x6
  Z_i_jk_ = U_i_g_ %*% Y_g_qr_ %*% t(kronecker(C_k_r_,B_j_q_))

  U_labels = apply(U_i_g_, 1, function(x) which(x>0))
  B_labels = apply(B_j_q_, 1, function(x) which(x>0))
  C_labels = apply(C_k_r_, 1, function(x) which(x>0))

  # noise creation
  E_i_jk_ = matrix(rnorm(I*J*K, mean=noise_mean, sd=noise_stdev), I, J*K)

  # noise addition
  X_i_jk_ = Z_i_jk_ + E_i_jk_

  output = list(Z_i_jk=Z_i_jk_, E_i_jk=E_i_jk_, X_i_jk=X_i_jk_, Y_g_qr=Y_g_qr_, U_i_g=U_i_g_, B_j_q=B_j_q_, C_k_r=C_k_r_, U_labels=U_labels,B_labels=B_labels,C_labels=C_labels)

  return(output)

}

