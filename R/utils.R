
#' PseudoF Score in the Reduced-Space
#'
#' Computes the PseudoF score in the reduced space.
#'
#' @param bss Between sums of squared deviations between clusters.
#' @param wss Within sums of squared deviations within clusters.
#' @param full_tensor_shape Dimensions of the tensor in the original space.
#' @param reduced_tensor_shape Dimension of the tensor in the reduced space.
#'
#' @return PseudoF score
#' @export
#'
#' @references
#' \insertRef{pseudoF}{simuclustfactor}
#'
#' @examples
#' pseudof.reduced(12,6,c(8,5,4),c(3,3,2))
#'
pseudof.reduced = function(bss, wss, full_tensor_shape, reduced_tensor_shape){

  # dimensions of tensor in the full space
  I = full_tensor_shape[1] # objects

  # dimensions of centroids tensor in the reduced space.
  G = reduced_tensor_shape[1] # number of clusters
  Q = reduced_tensor_shape[2] # number of factors for variables
  R = reduced_tensor_shape[3] # number of factors for occasions

  db = G - 1
  dw = I*Q*R - G

  return (bss/db)/(wss/dw)
}


#' PseudoF Score in the Full-Space
#'
#' Computes the PseudoF score in the full space.
#'
#' @param bss Between sums of squared deviations between clusters.
#' @param wss Within sums of squared deviations within clusters.
#' @param full_tensor_shape Dimensions of the tensor in the original space.
#' @param reduced_tensor_shape Dimension of the tensor in the reduced space.
#'
#' @return PseudoF score
#' @export
#'
#' @references
#' \insertRef{pseudoF}{simuclustfactor}
#' \insertRef{t3clus}{simuclustfactor}
#'
#' @examples
#' pseudof.full(12,6,c(8,5,4),c(3,3,2))
pseudof.full = function(bss, wss, full_tensor_shape, reduced_tensor_shape){

  I = full_tensor_shape[1] # number of objects
  J = full_tensor_shape[2] # number of variables
  K = full_tensor_shape[3] # number of occasions

  # dimensions of centroids tensor in the reduced space.
  G = reduced_tensor_shape[1] # number of clusters
  Q = reduced_tensor_shape[2] # number of factors for variables
  R = reduced_tensor_shape[3] # number of factors for occasions

  db = (G-1)*Q*R + (J-Q)*Q + (K-R)*R
  dw = I*J*K - (G*Q*R + (J-Q)*Q + (K-R)*R)

  return (bss/db)/(wss/dw)
}


#' Random Membership Function Matrix Generator
#'
#' Generates random binary stochastic membership function matrix for the I objects.
#'
#' @param I Number of objects.
#' @param G Number of groups/clusters.
#' @param seed Seed for random number generation.
#'
#' @return U_i_g, binary stochastic membership matrix.
#' @export
#'
#' @examples
#' generate_rmfm(I=8,G=3)
#'
generate_rmfm <- function(I,G, seed=NULL){

  set.seed(seed)

  # initialize U_i_g
  U_i_g = matrix(0,nrow = I, ncol = G)
  U_i_g[1:G,] = diag(G)  # first G assignments to unique clusters. To ensure no cluster is empty

  # assign random clusters to remaining objects
  if (I>G){
    for (p in (G+1):I){
      c = round(runif(1, min = 1, max = G))
      U_i_g[p,c] = 1  # assign object p to cluster c
    }
  }

  return(U_i_g)
}


#' One-run of the K-means clustering technique
#'
#' Initializes centroids based on a given membership function matrix or randomly.
#' Iterate once over the input data to update the membership function matrix
#' assigning objects to the closest centroids.
#'
#' @param Y_i_qr Input data to group/cluster.
#' @param G Number of clusters to find.
#' @param U_i_g Initial membership matrix for the I objects.
#' @param seed Seed for random values generation.
#'
#' @return updated membership matrix U_i_g.
#' @export
#'
#' @references
#'  \insertRef{k_meansMethods}{simuclustfactor}
#'
#' @examples
#' X_i_jk = generate_dataset(seed=0)$X_i_jk
#' onekmeans(X_i_jk, G=5)
#'
onekmeans <- function(Y_i_qr, G, U_i_g=NULL, seed=NULL){

  # defining random generator with no seed to random results.
  set.seed(seed)

  I = nrow(Y_i_qr)

  # initialize centroids matrix
  if (is.null(U_i_g)){
    U_i_g = generate_rmfm(I, G, seed=seed)
  }

  Y_g_qr = diag(1/colSums(U_i_g)) %*% t(U_i_g) %*% Y_i_qr

  U_i_g = matrix(0, nrow = I, ncol=G)

  # assign each object to the respective cluster
  for (i in 1:I){
    #dist = rowSums((Y_i_qr[i,]-Y_g_qr)**2)  # calculate distance between obj and centroids.
    dist = rowSums(sweep(Y_g_qr,2,Y_i_qr[i,])^2)
    min_dist_cluster = which.min(dist)  # get cluster with smallest distancee from object.
    U_i_g[i, min_dist_cluster] = 1  # assign the object to that cluster.
  }

  # possibility of observing empty clusters
  C_g = colSums(U_i_g)  # get count of members in each cluster

  # ------- case 1: repeat until no empty clustering
  while (0 %in% C_g){

    LC = which.max(C_g)    # select the largest cluster
    EC = which(C_g==0)  # select next empty cluster

    LC_members = which(U_i_g[,LC]==1)  # objects in the largest cluster indices
    M = length(LC_members)  # number of objects in the largest cluster.
    LC_scores = Y_i_qr[LC_members,]  # get scores in the largest cluster

    U_i_g = split_update(LC, LC_members, LC_scores, EC, U_i_g, C_g, seed)  # splitting cluster into 2 sub-clusters and updating U_i_g

    C_g = colSums(U_i_g)  # to check empty clusters
  }

  return(U_i_g)
}


#' Split Member of Largest cluster with An Empty cluster.
#'
#' If there is an empty cluster share members of largest cluster with
#' empty cluster via the k-means clustering technique
#'
#' @param LC Largest cluster index.
#' @param EC Empty cluster index to share members of LC with.
#' @param U_i_g Current membership function matrix with empty cluster to update.
#' @param LC_members Members of largest cluster.
#' @param LC_scores Scores of largest cluster.
#' @param C_g Number of members in each cluster.
#' @param seed Seed for random number generation.
#'
#'
#' @keywords internal
#'
#' @return U_i_g, the updated membership matrix.
split_update = function(LC, LC_members, LC_scores, EC, U_i_g, C_g, seed){

  M = length(LC_members)  # number of objects in the largest cluster.

  # perform k-means on LC members
  U_m_2 = generate_rmfm(I=M, G=2, seed = seed)  # initialize matrix with 2 groups
  Y_2_qr = diag(1/colSums(U_m_2)) %*% t(U_m_2) %*% LC_scores  # 2xQR centroids matrix for subclusters

  # assign each cluster member to the respective sub-cluster
  for (i in 1:dim(LC_scores)[1]){
    # dist = rowSums((LC_scores[i,]-Y_2_qr)**2)  # calculate distance between obj and the 2 sub-centroids.
    dist = rowSums(sweep(Y_2_qr,2,LC_scores[i,])^2)
    min_dist_cluster = which.min(dist)  # get cluster with smallest distance from object.

    if (min_dist_cluster == 1){
      U_i_g[LC_members[i],] = 0  # unassign the obj from the large cluster.
      U_i_g[LC_members[i], EC] = 1  # assign the obj to the empty cluster.
    }
  }

  return(U_i_g)
}
