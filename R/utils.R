### ========  START OF PseudoF

#' PseudoF sore in reduced space
#'
#' computes the PseudoF score in the reduced space.
#'
#' @param bss between sums of squared deviations between clusters
#' @param wss within sums of squared deviations within clusters
#' @param full_tensor_shape dimensions of the tensor in the original space
#' @param reduced_tensor_shape dimension of the tensor in the reduced space.
#'
#' @return PseudoF score
#' @export
#'
#' @examples
#' PseudoF_reduced(12,6,c(8,5,4),c(3,3,2))
PseudoF_Reduced = function(bss, wss, full_tensor_shape, reduced_tensor_shape){

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

#' PseudoF sore in Full space
#'
#' computes the PseudoF score in the full space.
#'
#' @param bss between sums of squared deviations between clusters
#' @param wss within sums of squared deviations within clusters
#' @param full_tensor_shape dimensions of the tensor in the original space
#' @param reduced_tensor_shape dimension of the tensor in the reduced space.
#'
#' @return PseudoF score
#' @export
#'
#' @examples
#' PseudoF_full(12,6,c(8,5,4),c(3,3,2))
PseudoF_Full = function(bss, wss, full_tensor_shape, reduced_tensor_shape){

  I = full_tensor_shape[1] # number of objects
  J = full_tensor_shape[2] # number of variables
  K = full_tensor_shape[3] # number of occasions

  # dimensions of centroids tensor in the reduced space.
  G = reduced_tensor_shape[1] # number of clusters
  Q = reduced_tensor_shape[2] # number of factors for variables
  R = reduced_tensor_shape[3] # number of factors for occasions

  db = (G-1)*Q*R + (J-Q)*Q + (K-R)*R
  dw = I*J*K - G*Q*R + (J-Q)*Q + (K-R)*R

  return (bss/db)/(wss/dw)
}

### ========  END OF PseudoF


### ========  START OF EigenVectors

#' Eigenvectors Extraction
#'
#' Returns the first D real eigenvectors of a given covariance matrix
#'
#' @param X covariance matrix to exttrat first D unitlength eigenvectors
#' @param D number of orthonormal eigenvectors to return
#'
#' @return first D eigenvectors of the covariance matrix
#' @export
#'
#' @examples
#' >> X_i_jk = generate_dataset(random_state=0)$X_i_jk
#' >> norm(EigenVectors(X_i_jk %*% t(X_i_jk),3),'F')  # 1.732051
EigenVectors <- function(X, D){
  return(eigen(X, symmetric = T)$vectors[,1:D])
}

### ========  END OF SingularVectors


### ========  START OF MEMBERSHIP FUNCTION MATRIX

#' Random Membership Matrix Generator
#'
#' Generates binary stochastic membership function matrix for the I objects.
#'
#' @param I number of objects.
#' @param G number of groups/clusters.
#' @param seed random_state seed.
#'
#' @return U_i_g, binary stochastic membership matrix
#' @export
#'
#' @examples
#' >> RandomMembershipMatrix(I=8,G=3)
RandomMembershipMatrix <- function(I,G, seed=NULL){

  #set.seed(seed)
  if (!is.null(seed)){
    if (is.null(.Random.seed)){ set.seed(seed) }
  }

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

### ========  END OF MEMBERSHIP FUNCTION MATRIX


### ========  START OF OneKMeans

#' One-run of the K-means clustering technique
#'
#' initializes centroids based on input centroid or randomly. iterate once over
#' input data to update the centroids assigning objects to the closest centroids.
#'
#' @param Y_i_qr # input data to group/cluster.
#' @param G # number of clusters to find.
#' @param U_i_g # initial membership matrix for the I objects
#' @param seed # random_state for random values generation.
#'
#' @return updated membership matrix U_i_g
#' @export
#'
#' @examples
#' >> X_i_jk = generate_dataset(random_state=0)$X_i_jk
#' >> OneKMeans(X_i_jk, G=5)
OneKMeans <- function(Y_i_qr, G, U_i_g=NULL, seed=NULL){

  # defining random generator with no seed to radnom results.
  if (!is.null(seed)){
    if (is.null(.Random.seed)){ set.seed(seed) }
  }

  I = nrow(Y_i_qr)

  # initialize centroids matrix
  if (is.null(U_i_g)){
    U_i_g = RandomMembershipMatrix(I,G)
  }

  Y_g_qr = diag(1/colSums(U_i_g)) %*% t(U_i_g) %*% Y_i_qr

  U_i_g = matrix(0, nrow = I, ncol=G)

  #' Split largest cluster with chosen empty cluster.
  #'
  #' if there is an empty cluster share members of largest cluster with
  #' empty cluster.
  #'
  #' @param LC largest cluster index
  #' @param EC empty cluster index to share members of LC with
  #' @param U_i_g current membership function matrix with empty cluster
  #'
  #' @return U_i_g, the updated membership matrix.
  split_update = function(LC, EC, U_i_g){

    cluster_members_indices = which(U_i_g[,LC]==1)  # objects in the largest cluster
    M = length(cluster_members_indices)  # number of objects in the largest cluster.

    # U_m_2 = RandomMembershipMatrix(I=M, G=2)  # initialize matrix with 2 groups
    # Y_2_qr = diag(1/colSums(U_m_2)) %*% t(U_m_2) %*% Y_i_qr[cluster_members_indices,]  # 2xQR centroids matrix for subclusters
    #
    # # assign each cluster member to the respective sub-cluster
    # for (i in 1:cluster_members_indices){
    #   dist = rowSums((Y_i_qr[i,]-Y_2_qr)**2)  # calculate distance between obj and the 2 sub-centroids.
    #   min_dist_cluster = which.min(dist)  # get cluster with smallest distance from object.
    #
    #   # obj reassignment
    #   if (min_dist_cluster == 1){
    #     U_i_g[i, LC] = 0  # unassign the obj from the large cluster.
    #     U_i_g[i, EC] = 1  # assign the obj to the empty cluster.
    #   }
    # }

    # split members into 2 groups and assign to clusters
    for (i in 1:floor(M/2)){
      U_i_g[cluster_members_indices[i], LC] = 0  # unassign the obj from the large cluster.
      U_i_g[cluster_members_indices[i], EC] = 1  # assign the obj to the empty cluster.
    }
    return(U_i_g)
  }

  # assign each object to the respective cluster
  for (i in 1:I){
    dist = rowSums((Y_i_qr[i,]-Y_g_qr)**2)  # calculate distance between obj and centroids.
    min_dist_cluster = which.min(dist)  # get cluster with smallest distancee from object.
    U_i_g[i, min_dist_cluster] = 1  # assign the object to that cluster.
  }

  # possibility of observing empty clusters
  C_g = colSums(U_i_g)  # get count of members in each cluster

  # ------- case 1: repeat until no empty clustering
  while (0 %in% C_g){
    LC = which.max(C_g)    # select the largest cluster
    EC = which(C_g==0)  # select next empty cluster
    U_i_g = split_update(LC,EC,U_i_g)  # splitting cluster into 2 sub-clusters and updating U_i_g
    C_g = colSums(U_i_g)  # ensure the alg stops
  }

  return(U_i_g)
}

### ========  END OF OneKMeans



