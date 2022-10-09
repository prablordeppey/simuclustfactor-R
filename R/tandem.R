# define custom data type. multiple
setClassUnion("numericORnull", c("numeric", "NULL"))
setClassUnion("matrixORnull", c("matrix", "NULL"))
setClassUnion("logicalORnull", c("logical", "NULL"))
setClassUnion("integerORnull", c("integer", "NULL"))


### RESULT OUTPUT

setClass("result.tandem",
         slots = c(
           U_i_g0="matrixORnull",            # initial object membership function matrix
           B_j_q0="matrixORnull",            # initial factor/component matrix for the variables
           C_k_r0="matrixORnull",            # initial factor/component matrix for the occasions
           U_i_g="matrixORnull",             # final/updated object membership function matrix
           B_j_q="matrixORnull",             # final/updated factor/component matrix for the variables
           C_k_r="matrixORnull",             # final/updated factor/component matrix for the occasions
           Y_g_qr="matrixORnull",            # derived centroids in the reduced space (data matrix)
           X_i_jk_scaled="matrixORnull",     # standardized dataset matrix

           BestTimeElapsed="numericORnull",  # execution time for the best iterate
           BestLoop="numericORnull",         # loop that obtained the best iterate
           BestKmIteration="numericORnull",  # number of iteration until best iterate for the K-means
           BestFaIteration="numericORnull",  # number of iteration until best iterate for the FA.
           FaConverged="logicalORnull",      # flag to check if algorithm converged for the K-means
           KmConverged="logicalORnull",      # flag to check if algorithm converged for the Factor Decomposition
           nKmConverges = "numericORnull",   # number of loops that converged for the K-means
           nFaConverges = "numericORnull",   # number of loops that converged for the Factor decomposition

           TSS_full='numericORnull',         # Total deviance in the fullspace
           BSS_full='numericORnull',         # Between deviance in the reducedspace
           RSS_full='numericORnull',         # Residual deviance in the reducedspace
           PF_full='numericORnull',          # PseudoF in the fullspace

           TSS_reduced='numericORnull',     # Total deviance in the reducedspace
           BSS_reduced='numericORnull',     # Between deviance in the reducedspace
           RSS_reduced='numericORnull',     # Residual deviance in the reducedspace
           PF_reduced='numericORnull',      # PseudoF in the reducedspace
           PF='numericORnull',

           Labels='integerORnull',          # object cluster assignments

           FsKM='numericORnull',            # objective function values for the KM best iterate
           FsFA='numericORnull',            # objective function values for the FA best iterate
           Enorm='numericORnull'            # average l2 norm of residual norm
         ),
         prototype = c(
           U_i_g0=NULL,
           B_j_q0=NULL,
           C_k_r0=NULL,
           U_i_g=NULL,
           B_j_q=NULL,
           C_k_r=NULL,
           Y_g_qr=NULL,
           X_i_jk_scaled=NULL,

           BestTimeElapsed=NULL,
           BestLoop=NULL,
           BestKmIteration=NULL,
           BestFaIteration=NULL,
           FaConverged=NULL,
           KmConverged=NULL,

           TSSFull=NULL,
           BSSFull=NULL,
           RSSFull=NULL,
           PFFull=NULL,

           TSSReduced=NULL,
           BSSReduced=NULL,
           RSSReduced=NULL,
           PFReduced=NULL,

           Labels=NULL,

           FsKM=NULL,
           FsFA=NULL,
           Enorm=NULL
         )
)


setClassUnion("numericORnull", c("numeric", "NULL"))

setClass("tandem",
         slots=c(
           random_state='numericORnull',   # seed for random sequence generation. Defaults to None.
           verbose='logical',              # whether to display executions output or not. Defaults to False.
           init='character',               # the parameter initialization method. Defaults to svd.
           n_max_iter='numeric',           # maximum number of iterations. Defaults to 10.
           n_loops='numeric',              # number of random initializations to gurantee global results. Defaults to 10.
           tol='numeric',                  # tolerance level/acceptable error. Defaults to 1e-5.
           U_i_g='numericORnull',          # (I,G) initial stochastic membership function matrix.
           B_j_q='numericORnull',          # (J,Q) initial component weight matrix for variables.
           C_k_r='numericORnull'           # (K,R) initial component weight matrix for occasions.
          )
)


# Tandem Class Constructor
#'
#' Initialization for the tandem models.
#'
#' @param random_state for random number generation
#' @param verbose flag to display iteration outputs
#' @param init parameter initialization method, 'svd' or 'random'
#' @param n_max_iter maximum number of iteration to optimize the objective function
#' @param n_loops maximum number of loops/runs for global results
#' @param tol allowable tolerance to check convergence
#' @param U_i_g initial membership function matrix for the objects
#' @param B_j_q initial component scores matrix for the variables
#' @param C_k_r initial component sores matrix for the occasions
#'
#' @export
#'
tandem <- function(random_state=NULL, verbose=TRUE, init='svd', n_max_iter=10, n_loops=10, tol=1e-5, U_i_g=NULL, B_j_q=NULL, C_k_r=NULL){
  new("tandem",
      random_state=random_state,
      verbose=verbose,
      init=init,
      n_max_iter=n_max_iter,
      n_loops=n_loops,
      tol=tol,
      U_i_g=U_i_g,
      B_j_q=B_j_q,
      C_k_r=C_k_r
    )
}

# Simultaneous Models
setClass('twcfta', contains='tandem')
setClass('twfcta', contains='tandem')


#' TWCFTA model
#'
#' Implements K-means clustering and afterwards factorial reduction
#' in a sequential fashion
#'
#' @param model initialized tandem model
#' @param X_i_jk matricized tensor along mode-1 (I objects)
#' @param full_tensor_shape dimensions of the tensor in full space
#' @param reduced_tensor_shape dimensions of tensor in the reduced space
#'
#' @export
#' @name fit.twcfta
#' @docType methods
#' @rdname tandem-twcfta
#'
#' @examples
#' X_i_jk = generate_dataset()
#' model = tandem()
#' twcfta = fit.twcfta(model, X_i_jk, c(8,5,4), c(3,3,2))
#'
setGeneric('fit.twcfta',
           function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){
             standardGeneric('fit.twcfta')}
           )

#' TWFCTA model
#'
#' Implements factorial reduction and then K-means clustering
#' in a sequential fashion
#'
#' @param model initialized tandem model
#' @param X_i_jk matricized tensor along mode-1 (I objects)
#' @param full_tensor_shape dimensions of the tensor in full space
#' @param reduced_tensor_shape dimensions of tensor in the reduced space
#'
#' @export
#' @name fit.twfcta
#' @docType methods
#' @rdname tandem-twfcta
#'
#' @examples
#' X_i_jk = generate_dataset()
#' model = tandem()
#' twfCta = fit.twfcta(model, X_i_jk, c(8,5,4), c(3,3,2))
#'
setGeneric('fit.twfcta',
           function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){
             standardGeneric('fit.twfcta')}
           )


# ------------ TWCFTA ------------

#' TWCFTA model
#'
#' @aliases TWCFTA,twcfta-model
#' @rdname tandem-methods
#'
setMethod('fit.twcfta',
          signature=('tandem'),
          function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){

            # ------------ Initialization ------------

            # I,J,K and G,Q,R declare
            I=full_tensor_shape[1]
            J=full_tensor_shape[2]
            K=full_tensor_shape[3]

            G=reduced_tensor_shape[1]
            Q=reduced_tensor_shape[2]
            R=reduced_tensor_shape[3]

            # standardizing dataset
            X_centered = scale(X_i_jk) # center X
            X_i_jk = X_centered/(colSums(X_centered**2)/nrow(X_centered))**0.5

            if (isTRUE(model@verbose)){
              print(c('Loop','Best KM Iter','Best FA Iter','Loop Time','BSS Full (%)', 'BSS Reduced (%)', 'PF Full', 'PF Reduced', 'KM Converged','FA Converged'))
            }

            n_faConverges = 0  # number of converges for factor analysis
            n_kmConverges = 0  # number of converges for kmeans

            # ------------ Loop/Run Start ------------

            # Factorial reduction on centroids (via T2 applied to the centroids matrix X_g_jk_bar)
            for(loop in 1:model@n_loops){

              start_time = Sys.time()

              # ------------ KMeans Clustering ------------

              # given directly as parameters
              U_i_g0 = model@U_i_g
              km_iter = 0
              km_converged = FALSE
              Fs_km = c()

              if (is.null(U_i_g0)){

                U_i_g0 = RandomMembershipMatrix(I,G,seed=model@random_state)
                U_i_g_init = U_i_g0

                # initial objective
                X_g_jk0 = diag(1/colSums(U_i_g0)) %*% t(U_i_g0) %*% X_i_jk  # compute centroids matrix
                F0 = norm(U_i_g0 %*% X_g_jk0, type = 'F')  # objective to maximize
                Fs_km = c(Fs_km,F0)

                # clustering on objects (via KMeans applied to X_i_jk)
                conv = 2*model@tol

                # iterates init
                best_km_iter = 1
                best_U_i_g = U_i_g0

                while (conv > model@tol){

                  km_iter = km_iter + 1

                  # get random centroids
                  U_i_g = OneKMeans(X_i_jk, G, U_i_g=U_i_g0, seed=model@random_state)  # updated membership matrix
                  X_g_jk = diag(1/colSums(U_i_g)) %*% t(U_i_g) %*% X_i_jk  # compute centroids matrix

                  # check if maximizes orbjective or minimizes the loss
                  F = norm(U_i_g %*% X_g_jk,type = 'F')  # frobenius norm
                  conv = abs(F-F0)

                  if (F >= F0){
                    F0 = F
                    Fs_km = c(Fs_km, F)
                    best_U_i_g = U_i_g
                    best_km_iter = km_iter
                  }

                  if (conv < model@tol){
                    km_converged = TRUE
                    n_kmConverges = n_kmConverges+1
                    break
                  }

                  if (km_iter == model@n_max_iter){
                    if (isTRUE(model@verbose)){
                      print("KM Maximum iterations reached.")
                    }
                    break
                  }

                  U_i_g0 = U_i_g

                }

              }else{
                U_i_g_init = U_i_g0
                best_U_i_g = U_i_g_init
              }

              # updated centroids in the full space
              X_g_jk = diag(1/colSums(best_U_i_g)) %*% t(best_U_i_g) %*% X_i_jk

              # ------------ Factor Decomposition ------------

              fa_converged = FALSE
              fa_iter = 0
              Fs_fa = c()  # objective function values

              # matricize centroid tensor
              X_g_j_k = Fold(X_g_jk, mode=1, shape=c(G,J,K))
              X_j_kg = Unfold(X_g_j_k, mode=2)
              X_k_gj = Unfold(X_g_j_k, mode=3)

              # as direct input
              B_j_q0 = model@B_j_q
              C_k_r0 = model@C_k_r

              # initialize B and C
              if (model@init == 'svd'){
                if (is.null(B_j_q0)){B_j_q0 = EigenVectors(X_j_kg %*% t(X_j_kg), Q)}
                if (is.null(C_k_r0)){C_k_r0 = EigenVectors(X_k_gj %*% t(X_k_gj), R)}
              }else{
                if (is.null(B_j_q0)) {
                  B_rand = matrix(runif(J*J), nrow=J, ncol=J)
                  B_j_q0 = EigenVectors(B_rand %*% t(B_rand),  Q)
                  remove(B_rand)
                  } # random initialization
                if (is.null(C_k_r0)) {
                  C_rand = matrix(runif(K*K), nrow=K, ncol=K)
                  C_k_r0 = EigenVectors(C_rand %*% t(C_rand), R)
                  remove(C_rand)
                  }
              }

              I_g_g = diag(1,G)

              # to return as initializers
              B_j_q_init = B_j_q0
              C_k_r_init = C_k_r0

              # updated centroids matrix
              Z_g_jk = X_g_jk %*% kronecker(C_k_r0%*%t(C_k_r0), B_j_q0%*%t(B_j_q0))

              F0 = norm(Z_g_jk, type = 'F')
              Fs_fa = c(Fs_fa, F0)
              conv = 2*model@tol

              # iterates init
              best_fa_iter = 1
              best_B_j_q = B_j_q0
              best_C_k_r = C_k_r0

              while (conv > model@tol){

                fa_iter = fa_iter + 1

                B_j_j = X_j_kg %*% kronecker(I_g_g, C_k_r0%*%t(C_k_r0)) %*% t(X_j_kg)
                B_j_q = EigenVectors(B_j_j, Q)

                # updating C_k_r
                C_k_k = X_k_gj %*% kronecker(B_j_q%*%t(B_j_q), I_g_g) %*% t(X_k_gj)
                C_k_r = EigenVectors(C_k_k, R)

                # updated centroids matrix
                Z_g_jk = X_g_jk %*% kronecker(C_k_r%*%t(C_k_r), B_j_q%*%t(B_j_q))

                # compute L2 norm of reconstruction error
                F = norm(Z_g_jk,type='F')
                conv = abs(F-F0)

                # convergence check
                if (F >= F0){
                  Fs_fa = c(Fs_fa, F)
                  F0 = F
                  best_B_j_q = B_j_q
                  best_C_k_r = C_k_r
                  best_fa_iter = fa_iter
                }

                if (conv < model@tol){
                  fa_converged = TRUE
                  n_faConverges = n_faConverges+1
                  break
                }

                if (fa_iter == model@n_max_iter){
                  if (isTRUE(model@verbose)){print("FA Maximum iterations reached.")}
                  break
                }

                B_j_q0 = B_j_q
                C_k_r0 = C_k_r

              }

              # ----------- Compute metrics for loop/run --------------

              time_elapsed = as.numeric(Sys.time()-start_time)

              # updating X
              Y_i_qr = X_i_jk %*% kronecker(best_C_k_r, best_B_j_q)
              Z_i_qr = best_U_i_g %*% diag(1/colSums(best_U_i_g)) %*% t(best_U_i_g) %*% Y_i_qr
              Z_i_jk = Z_i_qr %*% t(kronecker(best_C_k_r, best_B_j_q))

              TSS_full = sum(diag(X_i_jk%*%t(X_i_jk)))
              BSS_full = sum(diag(Z_i_jk%*%t(Z_i_jk)))
              RSS_full = sum(diag((X_i_jk-Z_i_jk)%*%t(X_i_jk-Z_i_jk)))

              TSS_reduced = sum(diag(Y_i_qr%*%t(Y_i_qr)))
              BSS_reduced = sum(diag(Z_i_qr%*%t(Z_i_qr)))
              RSS_reduced = sum(diag((Y_i_qr-Z_i_qr)%*%t(Y_i_qr-Z_i_qr)))

              # pseudoF scores
              pseudoF_full = PseudoF_Full(
                BSS_full, RSS_full, full_tensor_shape=full_tensor_shape,
                reduced_tensor_shape=reduced_tensor_shape)
              pseudoF_reduced = PseudoF_Reduced(
                BSS_reduced, RSS_reduced, full_tensor_shape=full_tensor_shape,
                reduced_tensor_shape=reduced_tensor_shape)

              # output results
              if (isTRUE(model@verbose)){
                BSS_percent_full = (BSS_full/TSS_full) * 100  # between cluster deviance
                BSS_percent_reduced = (BSS_reduced/TSS_reduced) * 100  # between cluster deviance
                print(c(loop, best_km_iter, best_fa_iter, round(time_elapsed,4), round(BSS_percent_full,2), round(BSS_percent_reduced,2), round(pseudoF_full,4), round(pseudoF_reduced,4), km_converged, fa_converged))
              }

              # tracking the best loop iterates
              if (loop == 1){
                B_j_q_simu = best_B_j_q
                C_k_r_simu = best_C_k_r
                U_i_g_simu = best_U_i_g
                km_iter_simu = best_km_iter
                fa_iter_simu = best_fa_iter
                loop_simu = 1
                km_converged_simu = km_converged
                fa_converged_simu = fa_converged
                Fs_fa = Fs_fa
                Fs_km = Fs_km
                pseudoF_full_simu = pseudoF_full
                TSS_full_simu = TSS_full
                BSS_full_simu = BSS_full
                RSS_full_simu = RSS_full
                pseudoF_reduced_simu = pseudoF_reduced
                TSS_reduced_simu = TSS_reduced
                BSS_reduced_simu = BSS_reduced
                RSS_reduced_simu = RSS_reduced
                U_i_g_init_simu = U_i_g_init
                B_j_q_init_simu = B_j_q_init
                C_k_r_init_simu = C_k_r_init
                best_time_elapsed_simu = time_elapsed
              }

              if (pseudoF_full > pseudoF_full_simu){
                B_j_q_simu = best_B_j_q
                C_k_r_simu = best_C_k_r
                U_i_g_simu = best_U_i_g
                km_iter_simu = best_km_iter  # number of iterations until convergence
                fa_iter_simu = best_fa_iter
                loop_simu = loop  # best loop so far
                km_converged_simu = km_converged  # if there was a convergence
                fa_converged_simu = fa_converged  # if there was a convergence
                Fs_fa = Fs_fa  # objective function values for FA
                Fs_km = Fs_km
                pseudoF_full_simu = pseudoF_full
                TSS_full_simu = TSS_full
                BSS_full_simu = BSS_full
                RSS_full_simu = RSS_full
                pseudoF_reduced_simu = pseudoF_reduced
                TSS_reduced_simu = TSS_reduced
                BSS_reduced_simu = BSS_reduced
                RSS_reduced_simu = RSS_reduced
                U_i_g_init_simu = U_i_g_init
                B_j_q_init_simu = B_j_q_init
                C_k_r_init_simu = C_k_r_init
                best_time_elapsed_simu = time_elapsed
              }

            }
            # ----------- Result update for best loop --------------

            Y_i_qr = X_i_jk %*% kronecker(C_k_r_simu, B_j_q_simu)
            Y_g_qr = diag(1/colSums(U_i_g_simu)) %*% t(U_i_g_simu) %*% Y_i_qr
            Z_i_qr = U_i_g_simu %*% Y_g_qr

            # factor matrices and centroid matrices

            return(new("result.tandem",
                       U_i_g0=U_i_g_init_simu,
                       B_j_q0=B_j_q_init_simu,
                       C_k_r0=C_k_r_init_simu,
                       U_i_g=U_i_g_simu,
                       B_j_q=B_j_q_simu,
                       C_k_r=C_k_r_simu,
                       Y_g_qr=Y_g_qr,
                       X_i_jk_scaled=X_i_jk,

                       BestTimeElapsed=best_time_elapsed_simu,
                       BestLoop=loop_simu,
                       BestKmIteration=km_iter_simu,
                       BestFaIteration=fa_iter_simu,
                       FaConverged=fa_converged_simu,
                       KmConverged=km_converged_simu,
                       nKmConverges=n_kmConverges,
                       nFaConverges=n_faConverges,

                       TSS_full=TSS_full_simu,
                       BSS_full=BSS_full_simu,
                       RSS_full=RSS_full_simu,
                       PF_full=pseudoF_full,
                       TSS_reduced=TSS_reduced_simu,
                       BSS_reduced=BSS_reduced_simu,
                       RSS_reduced=RSS_reduced_simu,
                       PF_reduced=pseudoF_reduced,
                       PF = pseudoF_full,

                       FsKM=Fs_km,
                       FsFA=Fs_fa,
                       Enorm=1/I*norm(X_i_jk - Z_i_qr %*% t(kronecker(C_k_r_simu, B_j_q_simu)), type='F'),

                       Labels=which(U_i_g_simu==1, arr.ind = TRUE)[,1]
                      )
                   )

          })

# ------------ TWFCTA ------------

#' TWFCTA model
#'
#' @aliases TWFCTA,twfcta-model
#' @rdname tandem-methods
#'
setMethod('fit.twfcta',
          signature=('tandem'),
          function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){

            # ------------ Initialization ------------

            # I,J,K and G,Q,R declare
            I=full_tensor_shape[1]
            J=full_tensor_shape[2]
            K=full_tensor_shape[3]

            G=reduced_tensor_shape[1]
            Q=reduced_tensor_shape[2]
            R=reduced_tensor_shape[3]

            # standardizing dataset
            X_centered = scale(X_i_jk) # center X
            X_i_jk = X_centered/(colSums(X_centered**2)/nrow(X_centered))**0.5

            if (isTRUE(model@verbose)){
              print(c('Loop','Best KM Iter','Best FA Iter','Loop Time','BSS Full (%)', 'BSS Reduced (%)', 'PF Full', 'PF Reduced', 'KM Converged','FA Converged'))
            }

            n_faConverges = 0  # number of converges for factor analysis
            n_kmConverges = 0  # number of converges for kmeans

            # ------------ Loop/Run Start ------------

            # Factorial reduction on centroids (via T2 applied to the centroids matrix X_g_jk_bar)
            for(loop in 1:model@n_loops){

              start_time = Sys.time()

              # ------------ Factor Decomposition ------------

              fa_converged = FALSE
              fa_iter = 0
              Fs_fa = c()  # objective function values

              # matricize centroid tensor
              X_i_j_k = Fold(X_i_jk, mode=1, shape=c(I,J,K))
              X_j_ki = Unfold(X_i_j_k, mode=2)
              X_k_ij = Unfold(X_i_j_k, mode=3)

              # as direct input
              B_j_q0 = model@B_j_q
              C_k_r0 = model@C_k_r

              # initialize B and C
              if (model@init == 'svd'){
                if (is.null(B_j_q0)){B_j_q0 = EigenVectors(X_j_ki %*% t(X_j_ki), Q)}
                if (is.null(C_k_r0)){C_k_r0 = EigenVectors(X_k_ij %*% t(X_k_ij), R)}
              }else{
                if (is.null(B_j_q0)) {
                  B_rand = matrix(runif(J*J), nrow=J, ncol=J)
                  B_j_q0 = EigenVectors(B_rand %*% t(B_rand),  Q)
                  remove(B_rand)
                } # random initialization
                if (is.null(C_k_r0)) {
                  C_rand = matrix(runif(K*K), nrow=K, ncol=K)
                  C_k_r0 = EigenVectors(C_rand %*% t(C_rand), R)
                  remove(C_rand)
                }
              }

              I_i_i = diag(1,I)

              # to return as initializers
              B_j_q_init = B_j_q0
              C_k_r_init = C_k_r0

              # updated centroids matrix
              Z_i_jk = X_i_jk %*% kronecker(C_k_r0%*%t(C_k_r0), B_j_q0%*%t(B_j_q0))

              F0 = norm(Z_i_jk, type = 'F')
              Fs_fa = c(Fs_fa, F0)
              conv = 2*model@tol

              # iterates init
              best_fa_iter = 1
              best_B_j_q = B_j_q0
              best_C_k_r = C_k_r0

              while (conv > model@tol){

                fa_iter = fa_iter + 1

                B_j_j = X_j_ki %*% kronecker(I_i_i, C_k_r0%*%t(C_k_r0)) %*% t(X_j_ki)
                B_j_q = EigenVectors(B_j_j, Q)

                # updating C_k_r
                C_k_k = X_k_ij %*% kronecker(B_j_q%*%t(B_j_q), I_i_i) %*% t(X_k_ij)
                C_k_r = EigenVectors(C_k_k, R)

                # updated centroids matrix
                Z_i_jk = X_i_jk %*% kronecker(C_k_r%*%t(C_k_r), B_j_q%*%t(B_j_q))

                # compute L2 norm of reconstruction error
                F = norm(Z_i_jk,type='F')
                conv = abs(F-F0)

                # convergence check
                if (F >= F0){
                  Fs_fa = c(Fs_fa, F)
                  F0 = F
                  best_B_j_q = B_j_q
                  best_C_k_r = C_k_r
                  best_fa_iter = fa_iter
                }

                if (conv < model@tol){
                  fa_converged = TRUE
                  n_faConverges = n_faConverges+1
                  break
                }

                if (fa_iter == model@n_max_iter){
                  if (isTRUE(model@verbose)){print("FA Maximum iterations reached.")}
                  break
                }

                B_j_q0 = B_j_q
                C_k_r0 = C_k_r

              }

              Y_i_qr = X_i_jk %*% kronecker(best_C_k_r, best_B_j_q)

              # ----------- KMeans Clustering --------------

              # given directly as parameters
              U_i_g0 = model@U_i_g
              km_iter = 0
              km_converged = FALSE
              Fs_km = c()

              if (is.null(U_i_g0)){

                U_i_g0 = RandomMembershipMatrix(I,G,seed=model@random_state)
                U_i_g_init = U_i_g0

                # initial objective
                Y_g_qr0 = diag(1/colSums(U_i_g0)) %*% t(U_i_g0) %*% Y_i_qr  # compute centroids matrix
                F0 = norm(U_i_g0 %*% Y_g_qr0, type = 'F')  # objective to maximize
                Fs_km = c(Fs_km,F0)

                # clustering on objects (via KMeans applied to Y_i_qr)
                conv = 2*model@tol

                # iterates init
                best_km_iter = 1
                best_U_i_g = U_i_g0

                while (conv > model@tol){

                  km_iter = km_iter + 1

                  # get random centroids
                  U_i_g = OneKMeans(Y_i_qr, G, U_i_g=U_i_g0, seed=model@random_state)  # updated membership matrix
                  Y_g_qr = diag(1/colSums(U_i_g)) %*% t(U_i_g) %*% Y_i_qr  # compute centroids matrix

                  # check if maximizes objective or minimizes the loss
                  F = norm(U_i_g %*% Y_g_qr,type = 'F')  # frobenius norm
                  conv = abs(F-F0)

                  if (F >= F0){
                    F0 = F
                    Fs_km = c(Fs_km, F)
                    best_U_i_g = U_i_g
                    best_km_iter = km_iter
                  }

                  if (conv < model@tol){
                    km_converged = TRUE
                    n_kmConverges = n_kmConverges+1
                    break
                  }

                  if (km_iter == model@n_max_iter){
                    if (isTRUE(model@verbose)){
                      print("KM Maximum iterations reached.")
                    }
                    break
                  }

                  U_i_g0 = U_i_g

                }

              }else{
                U_i_g_init = U_i_g0
                best_U_i_g = U_i_g_init
              }

              # ----------- Compute metrics for loop/run --------------

              time_elapsed = as.numeric(Sys.time()-start_time)

              # updating X
              Y_i_qr = X_i_jk %*% kronecker(best_C_k_r, best_B_j_q)
              Z_i_qr = best_U_i_g %*% diag(1/colSums(best_U_i_g)) %*% t(best_U_i_g) %*% Y_i_qr
              Z_i_jk = Z_i_qr %*% t(kronecker(best_C_k_r, best_B_j_q))

              TSS_full = sum(diag(X_i_jk%*%t(X_i_jk)))
              BSS_full = sum(diag(Z_i_jk%*%t(Z_i_jk)))
              RSS_full = sum(diag((X_i_jk-Z_i_jk)%*%t(X_i_jk-Z_i_jk)))

              TSS_reduced = sum(diag(Y_i_qr%*%t(Y_i_qr)))
              BSS_reduced = sum(diag(Z_i_qr%*%t(Z_i_qr)))
              RSS_reduced = sum(diag((Y_i_qr-Z_i_qr)%*%t(Y_i_qr-Z_i_qr)))

              # pseudoF scores
              pseudoF_full = PseudoF_Full(
                BSS_full, RSS_full, full_tensor_shape=full_tensor_shape,
                reduced_tensor_shape=reduced_tensor_shape)
              pseudoF_reduced = PseudoF_Reduced(
                BSS_reduced, RSS_reduced, full_tensor_shape=full_tensor_shape,
                reduced_tensor_shape=reduced_tensor_shape)

              # output results
              if (isTRUE(model@verbose)){
                BSS_percent_full = (BSS_full/TSS_full) * 100  # between cluster deviance
                BSS_percent_reduced = (BSS_reduced/TSS_reduced) * 100  # between cluster deviance
                print(c(loop, best_km_iter, best_fa_iter, round(time_elapsed,4), round(BSS_percent_full,2), round(BSS_percent_reduced,2), round(pseudoF_full,4), round(pseudoF_reduced,4), km_converged, fa_converged))
              }

              # tracking the best loop iterates
              if (loop == 1){
                B_j_q_simu = best_B_j_q
                C_k_r_simu = best_C_k_r
                U_i_g_simu = best_U_i_g
                km_iter_simu = best_km_iter
                fa_iter_simu = best_fa_iter
                loop_simu = 1
                km_converged_simu = km_converged
                fa_converged_simu = fa_converged
                Fs_fa = Fs_fa
                Fs_km = Fs_km
                pseudoF_full_simu = pseudoF_full
                TSS_full_simu = TSS_full
                BSS_full_simu = BSS_full
                RSS_full_simu = RSS_full
                pseudoF_reduced_simu = pseudoF_reduced
                TSS_reduced_simu = TSS_reduced
                BSS_reduced_simu = BSS_reduced
                RSS_reduced_simu = RSS_reduced
                U_i_g_init_simu = U_i_g_init
                B_j_q_init_simu = B_j_q_init
                C_k_r_init_simu = C_k_r_init
                best_time_elapsed_simu = time_elapsed
              }

              if (pseudoF_full > pseudoF_full_simu){
                B_j_q_simu = best_B_j_q
                C_k_r_simu = best_C_k_r
                U_i_g_simu = best_U_i_g
                km_iter_simu = best_km_iter  # number of iterations until convergence
                fa_iter_simu = best_fa_iter
                loop_simu = loop  # best loop so far
                km_converged_simu = km_converged  # if there was a convergence
                fa_converged_simu = fa_converged  # if there was a convergence
                Fs_fa = Fs_fa  # objective function values for FA
                Fs_km = Fs_km
                pseudoF_full_simu = pseudoF_full
                TSS_full_simu = TSS_full
                BSS_full_simu = BSS_full
                RSS_full_simu = RSS_full
                pseudoF_reduced_simu = pseudoF_reduced
                TSS_reduced_simu = TSS_reduced
                BSS_reduced_simu = BSS_reduced
                RSS_reduced_simu = RSS_reduced
                U_i_g_init_simu = U_i_g_init
                B_j_q_init_simu = B_j_q_init
                C_k_r_init_simu = C_k_r_init
                best_time_elapsed_simu = time_elapsed
              }
            }
            # ------------ Result update for best loop ------------

            Y_i_qr = X_i_jk %*% kronecker(C_k_r_simu, B_j_q_simu)
            Y_g_qr = diag(1/colSums(U_i_g_simu)) %*% t(U_i_g_simu) %*% Y_i_qr
            Z_i_qr = U_i_g_simu %*% Y_g_qr

            # factor matrices and centroid matrices

            return(new("result.tandem",
                       U_i_g0=U_i_g_init_simu,
                       B_j_q0=B_j_q_init_simu,
                       C_k_r0=C_k_r_init_simu,
                       U_i_g=U_i_g_simu,
                       B_j_q=B_j_q_simu,
                       C_k_r=C_k_r_simu,
                       Y_g_qr=Y_g_qr,
                       X_i_jk_scaled=X_i_jk,

                       BestTimeElapsed=best_time_elapsed_simu,
                       BestLoop=loop_simu,
                       BestKmIteration=km_iter_simu,
                       BestFaIteration=fa_iter_simu,
                       FaConverged=fa_converged_simu,
                       KmConverged=km_converged_simu,
                       nKmConverges=n_kmConverges,
                       nFaConverges=n_faConverges,

                       TSS_full=TSS_full_simu,
                       BSS_full=BSS_full_simu,
                       RSS_full=RSS_full_simu,
                       PF_full=pseudoF_full,
                       TSS_reduced=TSS_reduced_simu,
                       BSS_reduced=BSS_reduced_simu,
                       RSS_reduced=RSS_reduced_simu,
                       PF_reduced=pseudoF_reduced,
                       PF = pseudoF_full,

                       FsKM=Fs_km,
                       FsFA=Fs_fa,
                       Enorm=1/I*norm(X_i_jk - Z_i_qr %*% t(kronecker(C_k_r_simu, B_j_q_simu)), type='F'),

                       Labels=which(U_i_g_simu==1, arr.ind = TRUE)[,1]
                  ))

            })



