# define custom data type. multiple
setClassUnion("numericORnull", c("numeric", "NULL"))
setClassUnion("matrixORnull", c("matrix", "NULL"))
setClassUnion("logicalORnull", c("logical", "NULL"))
setClassUnion("integerORnull", c("integer", "NULL"))


### RESULT OUTPUT

#' Simultaneous results attributes
#'
#' @slot U_i_g0 matrix. Initial object membership function matrix
#' @slot B_j_q0 matrix. Initial factor/component matrix for the variables
#' @slot C_k_r0 matrix. Initial factor/component matrix for the occasions
#' @slot U_i_g matrix. Final/updated object membership function matrix
#' @slot B_j_q matrix. Final/updated factor/component matrix for the variables
#' @slot C_k_r matrix. Final/updated factor/component matrix for the occasions
#' @slot Y_g_qr matrix. Derived centroids in the reduced space (data matrix)
#' @slot X_i_jk_scaled matrix. Standardized dataset matrix
#' @slot BestTimeElapsed numeric. Execution time for the best iterate
#' @slot BestLoop numeric. Loop that obtained the best iterate
#' @slot BestIteration numeric. Iteration yielding the best results
#' @slot Converged numeric. Flag to check if algorithm converged for the K-means
#' @slot nConverges numeric. Number of loops that converged for the K-means
#' @slot TSS_full numeric. Total deviance in the full-space
#' @slot BSS_full numeric. Between deviance in the reduced-space
#' @slot RSS_full numeric. Residual deviance in the reduced-space
#' @slot PF_full numeric. PseudoF in the full-space
#' @slot TSS_reduced numeric. Total deviance in the reduced-space
#' @slot BSS_reduced numeric. Between deviance in the reduced-space
#' @slot RSS_reduced numeric. Residual deviance in the reduced-space
#' @slot PF_reduced numeric. PseudoF in the reduced-space
#' @slot PF numeric. Weighted PseudoF score
#' @slot Labels integer. Object cluster assignments
#' @slot Fs numeric. Objective function values for the KM best iterate
#' @slot Enorm numeric. Average l2 norm of the residual norm.
#'
#' @importFrom methods setClass new
#'
setClass("attributes.simultaneous",
         slots = c(
           U_i_g0="matrixORnull",
           B_j_q0="matrixORnull",
           C_k_r0="matrixORnull",
           U_i_g="matrixORnull",
           B_j_q="matrixORnull",
           C_k_r="matrixORnull",
           Y_g_qr="matrixORnull",
           X_i_jk_scaled="matrixORnull",

           BestTimeElapsed="numericORnull",
           BestLoop="numericORnull",
           BestIteration="numericORnull",
           Converged="logicalORnull",
           nConverges = "numericORnull",

           TSS_full='numericORnull',
           BSS_full='numericORnull',
           RSS_full='numericORnull',
           PF_full='numericORnull',

           TSS_reduced='numericORnull',
           BSS_reduced='numericORnull',
           RSS_reduced='numericORnull',
           PF_reduced='numericORnull',
           PF='numericORnull',

           Labels='integerORnull',

           Fs='numericORnull',
           Enorm='numericORnull'
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
           BestIteration=NULL,
           Converged=NULL,
           nConverges=NULL,

           TSS_full=NULL,
           BSS_full=NULL,
           RSS_full=NULL,
           PF_full=NULL,

           TSS_reduced=NULL,
           BSS_reduced=NULL,
           RSS_reduced=NULL,
           PF_reduced=NULL,
           PF=NULL,

           Labels=NULL,

           Fs=NULL,
           Enorm=NULL
         )
)


#' Simultaneous Model
#'
#' @slot seed numeric. Seed for random sequence generation. Defaults to None.
#' @slot verbose logical. Whether to display executions output or not. Defaults to False.
#' @slot init character. The parameter initialization method. Defaults to 'svd'.
#' @slot n_max_iter numeric. Maximum number of iterations. Defaults to 10.
#' @slot n_loops numeric. Number of initialization to guarantee global results. Defaults to 10.
#' @slot tol numeric. Tolerance level/acceptable error. Defaults to 1e-5.
#' @slot U_i_g numeric. (I,G) initial stochastic membership function matrix.
#' @slot B_j_q numeric. (J,Q) initial component weight matrix for variables.
#' @slot C_k_r numeric. (K,R) initial component weight matrix for occasions.
#'
#' @importFrom methods setClass
#'
#' @export
#'
setClass("simultaneous",
         slots=c(
           seed='numericORnull',
           verbose='logical',
           init='character',
           n_max_iter='numeric',
           n_loops='numeric',
           tol='numeric',
           U_i_g='numericORnull',
           B_j_q='numericORnull',
           C_k_r='numericORnull'
         )
)

#' Simultaneous Model Constructor
#'
#' Initialize model object required by the simultaneous methods.
#'
#' @param seed Seed for random sequence generation.
#' @param verbose Flag to display output result for each loop.
#' @param init The initialization method for the model parameters. Values could be 'svd','random','twcfta' or 'twfcta' Defaults to svd.
#' @param n_max_iter Maximum number of iterations to optimize objective function.
#' @param n_loops Number of runs/loops in search of the global result.
#' @param tol Acceptable tolerance level.
#' @param U_i_g Membership function matrix for the objects.
#' @param B_j_q Component matrix for the variables.
#' @param C_k_r Component matrix for the occasions.
#'
#' @return An object of class "simultaneous".
#'
#' @details {
#'    Two simultaneous models T3Clus and 3FKMeans are the implemented methods.
#'    \itemize{
#'       \item T3Clus finds B_j_q and C_k_r such that the between-clusters
#'       deviance of the component scores is maximized.
#'      \item 3FKMeans finds B_j_q and C_k_r such that the within-clusters
#'      deviance of the component scores is minimized.
#'    }
#' }
#'
#' @note {
#'    The model finds the best partition described by the best orthogonal
#'    linear combinations of the variables and orthogonal linear combinations
#'    of the occasions.
#' }
#'
#' @seealso {
#'  \code{\link{fit.t3clus}} \code{\link{fit.3fkmeans}}
#'  \code{\link{fit.ct3clus}} \code{\link{tandem}}
#' }
#'
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references
#' \insertRef{tucker1966}{simuclustfactor}
#' \insertRef{VichiRocciKiers}{simuclustfactor}
#'
#' @examples
#' simultaneous()
#'
simultaneous <- function(seed=NULL, verbose=TRUE, init='svd', n_max_iter=10, n_loops=10, tol=1e-5, U_i_g=NULL, B_j_q=NULL, C_k_r=NULL){
  new("simultaneous",
      seed=seed,
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
setClass('t3clus', contains='simultaneous')
setClass('3fkmeans', contains='simultaneous')
setClass('ct3clus', contains='simultaneous')


#' T3Clus Model
#'
#' Implements simultaneous version of TWCFTA
#'
#' @param model Initialized simultaneous model.
#' @param X_i_jk Matricized tensor along mode-1 (I objects).
#' @param full_tensor_shape Dimensions of the tensor in full-space.
#' @param reduced_tensor_shape Dimensions of tensor in the reduced-space.
#'
#' @return Output attributes accessible via the '@' operator.
#' \itemize{
#'    \item U_i_g0 - Initial object membership function matrix
#'    \item B_j_q0 - Initial factor/component matrix for the variables
#'    \item C_k_r0 - Initial factor/component matrix for the occasions
#'    \item U_i_g - Final/updated object membership function matrix
#'    \item B_j_q - Final/updated factor/component matrix for the variables
#'    \item C_k_r - Final/updated factor/component matrix for the occasions
#'    \item Y_g_qr - Derived centroids in the reduced space (data matrix)
#'    \item X_i_jk_scaled - Standardized dataset matrix
#'    \item BestTimeElapsed - Execution time for the best iterate
#'    \item BestLoop - Loop that obtained the best iterate
#'    \item BestIteration - Iteration yielding the best results
#'    \item Converged - Flag to check if algorithm converged for the K-means
#'    \item nConverges - Number of loops that converged for the K-means
#'    \item TSS_full - Total deviance in the full-space
#'    \item BSS_full - Between deviance in the reduced-space
#'    \item RSS_full - Residual deviance in the reduced-space
#'    \item PF_full - PseudoF in the full-space
#'    \item TSS_reduced - Total deviance in the reduced-space
#'    \item BSS_reduced - Between deviance in the reduced-space
#'    \item RSS_reduced - Residual deviance in the reduced-space
#'    \item PF_reduced - PseudoF in the reduced-space
#'    \item PF - Weighted PseudoF score
#'    \item Labels - Object cluster assignments
#'    \item Fs - Objective function values for the KM best iterate
#'    \item Enorm - Average l2 norm of the residual norm.
#' }
#'
#' @details {
#'    The procedure performs simultaneously the sequential TWCFTA model,
#'    finding B_j_q and C_k_r such that the between-clusters deviance of
#'    the component scores is maximized.
#' }
#'
#' @export
#'
#' @seealso {
#'  \code{\link{fit.ct3clus}} \code{\link{fit.3fkmeans}} \code{\link{simultaneous}}
#' }
#'
#' @name fit.t3clus
#' @rdname fit.t3clus
#'
#' @importFrom Rdpack reprompt
#'
#' @references
#' \insertRef{tucker1966}{simuclustfactor}
#' \insertRef{t3clus}{simuclustfactor}
#' \insertRef{VichiRocciKiers}{simuclustfactor}
#'
#' @examples
#' X_i_jk = generate_dataset()$X_i_jk
#' model = simultaneous()
#' t3clus = fit.t3clus(model, X_i_jk, c(8,5,4), c(3,3,2))
#'
setGeneric('fit.t3clus', function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){
  standardGeneric('fit.t3clus')
})

#' 3FKMeans Model
#'
#' Implements simultaneous version of TWFCTA
#'
#' @param model Initialized simultaneous model.
#' @param X_i_jk Matricized tensor along mode-1 (I objects).
#' @param full_tensor_shape Dimensions of the tensor in full-space.
#' @param reduced_tensor_shape Dimensions of tensor in the reduced-space.
#'
#' @return Output attributes accessible via the '@' operator.
#' \itemize{
#'    \item U_i_g0 - Initial object membership function matrix
#'    \item B_j_q0 - Initial factor/component matrix for the variables
#'    \item C_k_r0 - Initial factor/component matrix for the occasions
#'    \item U_i_g - Final/updated object membership function matrix
#'    \item B_j_q - Final/updated factor/component matrix for the variables
#'    \item C_k_r - Final/updated factor/component matrix for the occasions
#'    \item Y_g_qr - Derived centroids in the reduced space (data matrix)
#'    \item X_i_jk_scaled - Standardized dataset matrix
#'    \item BestTimeElapsed - Execution time for the best iterate
#'    \item BestLoop - Loop that obtained the best iterate
#'    \item BestIteration - Iteration yielding the best results
#'    \item Converged - Flag to check if algorithm converged for the K-means
#'    \item nConverges - Number of loops that converged for the K-means
#'    \item TSS_full - Total deviance in the full-space
#'    \item BSS_full - Between deviance in the reduced-space
#'    \item RSS_full - Residual deviance in the reduced-space
#'    \item PF_full - PseudoF in the full-space
#'    \item TSS_reduced - Total deviance in the reduced-space
#'    \item BSS_reduced - Between deviance in the reduced-space
#'    \item RSS_reduced - Residual deviance in the reduced-space
#'    \item PF_reduced - PseudoF in the reduced-space
#'    \item PF - Weighted PseudoF score
#'    \item Labels - Object cluster assignments
#'    \item Fs - Objective function values for the KM best iterate
#'    \item Enorm - Average l2 norm of the residual norm.
#' }
#'
#' @details {
#'    The procedure performs simultaneously the sequential TWFCTA model.
#'    The model finds B_j_q and C_k_r such that the within-clusters deviance of
#'    the component scores is minimized.
#' }
#'
#' @export
#'
#' @seealso {
#'  \code{\link{fit.t3clus}} \code{\link{fit.ct3clus}} \code{\link{simultaneous}}
#' }
#'
#' @importFrom Rdpack reprompt
#'
#' @references
#' \insertRef{tucker1966}{simuclustfactor}
#' \insertRef{3FKMeans}{simuclustfactor}
#' \insertRef{VichiRocciKiers}{simuclustfactor}
#'
#' @name fit.3fkmeans
#' @rdname fit.3fkmeans
#'
#' @examples
#' X_i_jk = generate_dataset()$X_i_jk
#' model = simultaneous()
#' tfkmeans = fit.3fkmeans(model, X_i_jk, c(8,5,4), c(3,3,2))
#'
setGeneric('fit.3fkmeans', function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){
  standardGeneric('fit.3fkmeans')
})

#' CT3Clus Model
#'
#' Implements simultaneous T3Clus and 3FKMeans integrating
#' an alpha value between 0 and 1 inclusive for a weighted result.
#'
#' @param model Initialized simultaneous model.
#' @param X_i_jk Matricized tensor along mode-1 (I objects).
#' @param full_tensor_shape Dimensions of the tensor in full space.
#' @param reduced_tensor_shape Dimensions of tensor in the reduced space.
#' @param alpha 0<alpha>1 hyper parameter. Model is T3Clus when alpha=1 and 3FKMeans when alpha=0.
#'
#' @return Output attributes accessible via the '@' operator.
#' \itemize{
#'    \item U_i_g0 - Initial object membership function matrix
#'    \item B_j_q0 - Initial factor/component matrix for the variables
#'    \item C_k_r0 - Initial factor/component matrix for the occasions
#'    \item U_i_g - Final/updated object membership function matrix
#'    \item B_j_q - Final/updated factor/component matrix for the variables
#'    \item C_k_r - Final/updated factor/component matrix for the occasions
#'    \item Y_g_qr - Derived centroids in the reduced space (data matrix)
#'    \item X_i_jk_scaled - Standardized dataset matrix
#'    \item BestTimeElapsed - Execution time for the best iterate
#'    \item BestLoop - Loop that obtained the best iterate
#'    \item BestIteration - Iteration yielding the best results
#'    \item Converged - Flag to check if algorithm converged for the K-means
#'    \item nConverges - Number of loops that converged for the K-means
#'    \item TSS_full - Total deviance in the full-space
#'    \item BSS_full - Between deviance in the reduced-space
#'    \item RSS_full - Residual deviance in the reduced-space
#'    \item PF_full - PseudoF in the full-space
#'    \item TSS_reduced - Total deviance in the reduced-space
#'    \item BSS_reduced - Between deviance in the reduced-space
#'    \item RSS_reduced - Residual deviance in the reduced-space
#'    \item PF_reduced - PseudoF in the reduced-space
#'    \item PF - Weighted PseudoF score
#'    \item Labels - Object cluster assignments
#'    \item Fs - Objective function values for the KM best iterate
#'    \item Enorm - Average l2 norm of the residual norm.
#' }
#'
#' @seealso {
#'  \code{\link{fit.t3clus}} \code{\link{fit.3fkmeans}} \code{\link{simultaneous}}
#' }
#'
#' @export
#' @name fit.ct3clus
#' @rdname fit.ct3clus
#'
#' @importFrom Rdpack reprompt
#'
#' @references
#' \insertRef{tucker1966}{simuclustfactor}
#' \insertRef{t3clus}{simuclustfactor}
#' \insertRef{3FKMeans}{simuclustfactor}
#' \insertRef{VichiRocciKiers}{simuclustfactor}
#'
#' @examples
#' X_i_jk = generate_dataset()$X_i_jk
#' model = simultaneous()
#' ct3clus = fit.ct3clus(model, X_i_jk, c(8,5,4), c(3,3,2), alpha=0.5)
#'
setGeneric('fit.ct3clus', function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape, alpha=0.5){
  standardGeneric('fit.ct3clus')
})


# ------------ T3CLUS IMPLEMENTATION ------------

#' @rdname fit.t3clus
setMethod('fit.t3clus',
          signature=('simultaneous'),
          function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){
            return(
              fit.ct3clus(model, X_i_jk=X_i_jk, full_tensor_shape=full_tensor_shape, reduced_tensor_shape=reduced_tensor_shape, alpha = 1)
            )
          }
)


# ------------ 3FKMEANS IMPLEMENTATION ------------

#' @rdname fit.3fkmeans
setMethod('fit.3fkmeans',
          signature=('simultaneous'),
          function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){
            return(
              fit.ct3clus(model, X_i_jk=X_i_jk, full_tensor_shape=full_tensor_shape, reduced_tensor_shape=reduced_tensor_shape, alpha = 0)
            )
          }
)


# ------------ CT3CLUS IMPLEMENTATION ------------

#' @rdname fit.ct3clus
setMethod('fit.ct3clus',
          signature=('simultaneous'),
          function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape, alpha=0.5){

            # ------------ Initialization ------------

            set.seed(model@seed)

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

            I_jk_jk = diag(J*K)
            I_i_i = diag(I)

            if (isTRUE(model@verbose)){
              print(c('Loop','Best Iter','Loop Time','BSS Full (%)', 'BSS Reduced (%)', 'PF Full', 'PF Reduced', 'Converged'))
            }

            n_converges = 0  # tracks the number of converges

            # ------------ Loop/Run Start ------------

            # Factorial reduction on centroids (via T2 applied to the centroids matrix X_g_jk_bar)
            for(loop in 1:model@n_loops){

              start_time = Sys.time()

              # ------------  Initialization ------------

              # given directly as parameters
              U_i_g0 = model@U_i_g
              B_j_q0 = model@B_j_q
              C_k_r0 = model@C_k_r
              iteration = 0
              converged = FALSE
              Fs = c()

              if (model@init == 'random'){  # random initialization
                if (is.null(B_j_q0)) {
                  B_rand = matrix(runif(J*J), nrow=J, ncol=J)
                  B_j_q0 = eigen(B_rand %*% t(B_rand), symmetric=T)$vectors[,1:Q]
                  remove(B_rand)
                  }
                if (is.null(C_k_r0)) {
                  C_rand = matrix(runif(K*K), nrow=K, ncol=K)
                  C_k_r0 = eigen(C_rand %*% t(C_rand), symmetric=T)$vectors[,1:R]
                  remove(C_rand)
                  }
              }else{  # svd initialization

                # matricize centroid tensor
                X_i_j_k = fold(X_i_jk, mode=1, shape=c(I,J,K))
                X_j_ki = unfold(X_i_j_k, mode=2)
                X_k_ij = unfold(X_i_j_k, mode=3)

                # initialize B_j_q
                if (is.null(B_j_q0)){
                  B_j_q0 = eigen(X_j_ki %*% t(X_j_ki), symmetric = T)$vectors[,1:Q]
                }

                # initialize C_k_r
                if (is.null(C_k_r0)){
                  C_k_r0 = eigen(X_k_ij %*% t(X_k_ij), symmetric = T)$vectors[,1:R]
                }
              }

              # initialize U_i_g
              U_i_g0 = generate_rmfm(I,G,seed=model@seed)

              # ----------- Start of Objective Function --------------

              U_i_g_init = U_i_g0
              B_j_q_init = B_j_q0
              C_k_r_init = C_k_r0

              # updating X_i_jk
              P = kronecker(C_k_r0%*%t(C_k_r0), B_j_q0%*%t(B_j_q0))
              X_i_jk_N = X_i_jk %*% (P + (alpha**0.5)*(I_jk_jk-P))

              Z_i_qr = U_i_g0 %*% diag(1/colSums(U_i_g0)) %*% t(U_i_g0) %*% X_i_jk_N %*% kronecker(C_k_r0, B_j_q0)

              F0 = norm(Z_i_qr,'F')
              conv = 2*model@tol
              Fs = c(Fs,F0)

              # best results
              best_U_i_g = U_i_g0
              best_B_j_q = B_j_q0
              best_C_k_r = C_k_r0
              best_iteration = 1

              # ----------- Start of Objective Function --------------

              while (conv > model@tol){

                iteration = iteration + 1

                # ----------- Start of factor matrices update --------------

                Hu_i_i = U_i_g0 %*% diag(1/colSums(U_i_g0)) %*% t(U_i_g0)

                # permuting X_i_jk_N by mode
                X_i_j_k = fold(X_i_jk_N, mode=1, shape=c(I,J,K))
                X_j_ki = unfold(X_i_j_k, mode=2)
                X_k_ij = unfold(X_i_j_k, mode=3)

                # updating B_j_q
                B_j_j = X_j_ki %*% kronecker(Hu_i_i-alpha*I_i_i, C_k_r0%*%t(C_k_r0)) %*% t(X_j_ki)
                B_j_q = eigen(B_j_j, symmetric = T)$vectors[,1:Q]

                # updating C_k_r
                C_k_k = X_k_ij %*% kronecker(B_j_q%*%t(B_j_q), Hu_i_i-alpha*I_i_i) %*% t(X_k_ij)
                C_k_r = eigen(C_k_k,symmetric = T)$vectors[,1:R]

                # ----------- Start of kmeans U_i_g update --------------

                # updating X_i_jk
                P = kronecker(C_k_r%*%t(C_k_r), B_j_q%*%t(B_j_q))
                X_i_jk_N = X_i_jk %*% (P + (alpha**0.5)*(I_jk_jk-P))

                # component scores Y_i_qr
                Y_i_qr = X_i_jk_N %*% kronecker(C_k_r, B_j_q) # component scores
                U_i_g = onekmeans(Y_i_qr, G, U_i_g=U_i_g0, seed=model@seed)  # updated membership matrix

                # ----------- Start of objective functions update --------------

                Z_i_qr = U_i_g %*% diag(1/colSums(U_i_g0)) %*% t(U_i_g) %*% Y_i_qr

                F = norm(Z_i_qr,'F')
                conv = abs(F-F0)

                if (F >= F0){
                  Fs = c(Fs,F)
                  F0 = F
                  best_B_j_q = B_j_q
                  best_C_k_r = C_k_r
                  best_U_i_g = U_i_g
                  best_iteration = iteration
                }

                if (conv < model@tol){
                  converged = T
                  n_converges = n_converges + 1
                  break
                }

                if (iteration == model@n_max_iter){
                  # if (model@verbose){print("Maximum iterations reached.")}
                  break
                }

                U_i_g0 = U_i_g
                B_j_q0 = B_j_q
                C_k_r0 = C_k_r
              }

              # ----------- Compute metrics for loop/run --------------

              time_elapsed = as.numeric(Sys.time()-start_time)

              # updating X_i_jk
              P = kronecker(best_C_k_r%*%t(best_C_k_r), best_B_j_q%*%t(best_B_j_q))
              X_i_jk_N = X_i_jk %*% (P + (alpha**0.5)*(I_jk_jk-P))

              Y_i_qr = X_i_jk_N %*% kronecker(best_C_k_r, best_B_j_q)
              Z_i_qr = best_U_i_g %*% diag(1/colSums(best_U_i_g)) %*% t(best_U_i_g) %*% Y_i_qr
              Z_i_jk = Z_i_qr %*% t(kronecker(best_C_k_r, best_B_j_q))

              TSS_full = sum(diag(X_i_jk_N%*%t(X_i_jk_N)))
              BSS_full = sum(diag(Z_i_jk%*%t(Z_i_jk)))
              RSS_full = sum(diag((X_i_jk_N-Z_i_jk)%*%t(X_i_jk_N-Z_i_jk)))

              TSS_reduced = sum(diag(Y_i_qr%*%t(Y_i_qr)))
              BSS_reduced = sum(diag(Z_i_qr%*%t(Z_i_qr)))
              RSS_reduced = sum(diag((Y_i_qr-Z_i_qr)%*%t(Y_i_qr-Z_i_qr)))

              # pseudoF scores
              pseudoF_full = pseudof.full(
                BSS_full, RSS_full, full_tensor_shape=full_tensor_shape,
                reduced_tensor_shape=reduced_tensor_shape)
              pseudoF_reduced = pseudof.reduced(
                BSS_reduced, RSS_reduced, full_tensor_shape=full_tensor_shape,
                reduced_tensor_shape=reduced_tensor_shape)

              PF = (1-alpha)*pseudoF_reduced + alpha*pseudoF_full

              # output results
              if (isTRUE(model@verbose)){
                BSS_percent_full = (BSS_full/TSS_full) * 100  # between cluster deviance
                BSS_percent_reduced = (BSS_reduced/TSS_reduced) * 100  # between cluster deviance
                print(c(loop, best_iteration, round(time_elapsed,4), round(BSS_percent_full,2), round(BSS_percent_reduced,2), round(pseudoF_full,4), round(pseudoF_reduced,4), converged))
              }

              # tracking the best loop iterates
              if (loop == 1){
                loop_simu = 1
                best_PF_simu = PF
                B_j_q_simu = best_B_j_q
                C_k_r_simu = best_C_k_r
                U_i_g_simu = best_U_i_g
                best_iteration_simu = best_iteration
                converged_simu = converged
                nconverges_simu = n_converges
                Fs_simu = Fs
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

              if (PF > best_PF_simu){
                best_PF_simu = PF
                B_j_q_simu = best_B_j_q
                C_k_r_simu = best_C_k_r
                U_i_g_simu = best_U_i_g
                best_iteration_simu = best_iteration  # number of iterations until convergence
                loop_simu = loop  # best loop so far
                converged_simu = converged  # if there was a convergence
                nconverges_simu = n_converges
                Fs_simu = Fs
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

            P = kronecker(C_k_r_simu%*%t(C_k_r_simu), B_j_q_simu%*%t(B_j_q_simu))
            X_i_jk_N = X_i_jk %*% (P + (alpha**0.5)*(I_jk_jk-P))

            Y_i_qr = X_i_jk_N %*% kronecker(C_k_r_simu, B_j_q_simu)
            Y_g_qr = diag(1/colSums(U_i_g_simu)) %*% t(U_i_g_simu) %*% Y_i_qr
            Z_i_qr = U_i_g_simu %*% Y_g_qr

            # factor matrices and centroid matrices
            return(new("attributes.simultaneous",
                       U_i_g0=U_i_g_init_simu,
                       B_j_q0=B_j_q_init_simu,
                       C_k_r0=C_k_r_init_simu,
                       U_i_g=U_i_g_simu,
                       B_j_q=B_j_q_simu,
                       C_k_r=C_k_r_simu,
                       Y_g_qr=Y_g_qr,
                       X_i_jk_scaled=X_i_jk_N,

                       BestTimeElapsed=best_time_elapsed_simu,
                       BestLoop=loop_simu,
                       BestIteration=best_iteration_simu,
                       Converged=converged_simu,
                       nConverges=n_converges,

                       TSS_full=TSS_full_simu,
                       BSS_full=BSS_full_simu,
                       RSS_full=RSS_full_simu,
                       PF_full=pseudoF_full,
                       TSS_reduced=TSS_reduced_simu,
                       BSS_reduced=BSS_reduced_simu,
                       RSS_reduced=RSS_reduced_simu,
                       PF_reduced=pseudoF_reduced,
                       PF=best_PF_simu,

                       Fs=Fs,
                       Enorm=1/I*norm(X_i_jk - Z_i_qr %*% t(kronecker(C_k_r_simu, B_j_q_simu)), type='F'),

                       Labels=which(U_i_g_simu==1, arr.ind = TRUE)[,1]
            ))

          })




