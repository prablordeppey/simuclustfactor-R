
#' Dataset Generation for simulation
#'
#' Generate G clustered synthetic dataset of I objects measured on J variables
#' for K occasions with additive noise.
#'
#' @param I number of objects.
#' @param J number of variables per occasion
#' @param K number of occasions
#' @param G number of clusters
#' @param Q number of factors for the variables
#' @param R number of factors for the occasions
#' @param mean noise effect mean
#' @param stdev noise effect level/spread/standard deviation
#' @param random_state seed for random number generation
#'
#' @returns Z_i_jk: component scores in the full space
#' @returns X_i_jk: dataset with noise level set to stdev specified
#' @returns Y_g_qr: centroids matrix in the reduced space
#' @returns U_i_g: stochastic membership function matrix
#' @returns B_j_q: objects component scores matrix
#' @returns C_k_r: occasions component scores matrix
#' @export
#'
#' @examples
#' >> X_i_jk = generate_dataset(random_state=0)
generate_dataset = function(I=8,J=5,K=4,G=3,Q=3,R=2, mean=0, stdev=0.5, random_state=NULL){

  set.seed(random_state)

  # 8x3
  U_i_g_ = RandomMembershipMatrix(I=I, G=G, seed = random_state)

  # 6x3
  B_j_q = RandomMembershipMatrix(I=J, G=Q, seed = random_state)  # contruct membership matrix
  weights = 1/colSums(B_j_q)**0.5  # compute weights for each factor
  facts = col(B_j_q)[which(B_j_q==1)]  # get corresponding factors for each var
  B_j_q[B_j_q==1] = weights[facts]  # update weight of var-factor entry

  # 4x2
  C_k_r_ = RandomMembershipMatrix(I=K, G=R, seed = random_state)  # contruct membership matrix
  weights = 1/colSums(C_k_r_)**0.5  # compute weights for each occasion factor
  facts = col(C_k_r_)[which(C_k_r_==1)]  # get corresponding factors for each occasion
  C_k_r_[C_k_r_==1] = weights[facts]  # update weight of occasion-factor entry

  Y_g_qr_ = matrix(runif(G*Q*R), nrow = G) # 3x6

  # 8x6
  Z_i_jk_ = U_i_g_ %*% Y_g_qr_ %*% t(kronecker(C_k_r_,B_j_q_))

  U_labels = apply(U_i_g_, 1, function(x) which(x>0))
  B_labels = apply(B_j_q_, 1, function(x) which(x>0))
  C_labels = apply(C_k_r_, 1, function(x) which(x>0))

  # noise creation
  E_i_jk_ = matrix(rnorm(I*J*K, mean=mean, sd=stdev), I, J*K)

  # noise addition
  X_i_jk_ = Z_i_jk_ + E_i_jk_

  output = list(Z_i_jk=Z_i_jk_, X_i_jk=X_i_jk_, Y_g_qr=Y_g_qr_, U_i_g=U_i_g_, B_j_q=B_j_q_, C_k_r=C_k_r_)

  return(output)

}

