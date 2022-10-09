# slot types
setClassUnion("numericORnull", c("numeric", "NULL"))
setClassUnion("logicalORnull", c("logical", "NULL"))
setClassUnion("integerORnull", c("integer", "NULL"))


### RESULT OUTPUT

setClass("result.simultaneous",
         slots = c(
           U_i_g0="numericORnull",
           B_j_q0="numericORnull",
           C_k_r0="numericORnull",
           U_i_g="numericORnull",
           B_j_q="numericORnull",
           C_k_r="numericORnull",
           Y_g_qr="numericORnull",
           X_i_jk_scaled="numericORnull",

           BestTimeElapsed="numericORnull",
           BestLoop="numericORnull",
           BestIteration="numericORnull",

           TSSFull='numericORnull',        # Total deviance in the fullspace
           BSSFull='numericORnull',        # Between deviance in the reducedspace
           RSSFull='numericORnull',        # Residual deviance in the reducedspace
           PFFull='numericORnull',         # PseudoF in the fullspace

           TSSReduced='numericORnull',     # Total deviance in the reducedspace
           BSSReduced='numericORnull',     # Between deviance in the reducedspace
           RSSReduced='numericORnull',     # Residual deviance in the reducedspace
           PFReduced='numericORnull',      # PseudoF in the reducedspace

           Labels='integerORnull',

           Fs='numericORnull',
           Converged=NULL,
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

           TSSFull=NULL,
           BSSFull=NULL,
           RSSFull=NULL,
           PFFull=NULL,

           TSSReduced=NULL,
           BSSReduced=NULL,
           RSSReduced=NULL,
           PFReduced=NULL,

           Labels=NULL,

           Fs=NULL,
           Converged=NULL,
           Enorm=NULL
         )
)


### START OF MODEL

setClass("simultaneous",
         slots=c(
           random_state='numericORnull',   # seed for random sequence generation. Defaults to None.
           verbose='logical',        # whether to display executions output or not. Defaults to False.
           init='character',         # the parameter initialization method. Defaults to svd.
           n_max_iter='numeric',     # maximum number of iterations. Defaults to 10.
           n_loops='numeric',        # number of random initializations to gurantee global results. Defaults to 10.
           tol='numeric',            # tolerance level/acceptable error. Defaults to 1e-5.
           U_i_g='numericORnull',          # (I,G) initial stochastic membership function matrix.
           B_j_q='numericORnull',          # (J,Q) initial component weight matrix for variables.
           C_k_r='numericORnull'           # (K,R) initial component weight matrix for occasions.
         )
)

# Base Class Constructor
simultaneous <- function(random_state=NULL, verbose=TRUE, init='svd', n_max_iter=10, n_loops=10, tol=1e-5, U_i_g=NULL, B_j_q=NULL, C_k_r=NULL){
  new("simultaneous",
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
setClass('t3clus', contains='simultaneous')
setClass('3fkmeans', contains='simultaneous')
setClass('ct3clus', contains='simultaneous')


# Generic Methods Definition
setGeneric('fit.t3clus', function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){
  standardGeneric('fit.t3clus')
})

setGeneric('fit.3fkmeans', function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape){
  standardGeneric('fit.3fkmeans')
})

setGeneric('fit.ct3clus', function(model, X_i_jk, full_tensor_shape, reduced_tensor_shape, alpha=0.5){
  standardGeneric('fit.ct3clus')
})

## Methods Definition

# T3CLUS IMPLEMENTATION
setMethod('fit.t3clus',
          signature=('simultaneous'),
          function(model){
            print('fit.t3clus')
          }
)

#3FKMEANS IMPLEMENTATION
setMethod('fit.3fkmeans',
          signature=('simultaneous'),
          function(model){
            print('fit.3fkmeans')
          }
)

# CT3CLUS IMPLEMENTATION
setMethod('fit.ct3clus',
          signature=('simultaneous'),
          function(model, X_i_jk, alpha){

            print(alpha)
            C_k_r = 9

            return(new("result.simultaneous",
                       U_i_g0=12.23
                      )
                   )
          }
)




