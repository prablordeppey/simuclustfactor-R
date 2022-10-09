
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simuclustfactor

<!-- badges: start -->
<!-- badges: end -->

The goal of simuclustfactor is to perform:

-   tandem clustering and factor-decomposition procedures sequentially
    (TWCFTA and TWFCTA).
-   simultaneous clustering and factor decomposition procedures
    simultaneously (T3Clus and 3FKMeans).
-   combined T3Clus and 3FKMeans procedures simultaneously (CT3Clus).

## Installation

You can install the development version of simuclustfactor from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("prablordeppey/simuclustfactor-r")
```

## Implementations

Synthetic Dataset Generation (Additive noise)

``` r
library(simuclustfactor)

# dimensions of the tensor in full and reduced spaces
I=8; J=5; K=4 # number objects, variables and occasions respectively
G=3; Q=3; R=2 # number clusters, variable-factors and occasion-factors respectively
data = generate_dataset(I, J, K, G, Q, R, mean=0, stdev=0.5, random_state=0)

# get data attributes
Y_g_qr = data$Y_g_qr  # centroids matrix in the reduced space
Z_i_jk = data$Z_i_jk  # score/centroid matrix in the fullspace.
X_i_jk = data$X_i_jk  # dataset with noise

# ground-truth associations
U_i_g = data$U_i_g  # binary stochastic membership matrix
B_j_q = data$B_j_q  # variables factor matrix
C_k_r = data$C_k_r  # occasions factor matrix

# folding generated data matrices into tensors
X_i_j_k = Fold(X_i_jk, mode=1, shape=c(I,J,K))
Z_i_j_k = Fold(Z_i_jk, mode=1, shape=c(I,J,K))
Y_g_q_r = Fold(Y_g_qr, mode=1, shape=c(G,Q,R))
```

### Tandem Models

``` r
# initialize the model
tandem_model = tandem(random_state=NULL, verbose=TRUE, init='svd', n_max_iter=10, n_loops=10, tol=1e-5, U_i_g=NULL, B_j_q=NULL, C_k_r=NULL)
```

**TWCFTA**

``` r
twcfta = fit.twcfta(tandem_model, X_i_jk, full_tensor_shape=c(I,J,K), reduced_tensor_shape=c(G,Q,R))

# get values/attributes from the implementation with the '@' operator
U_i_g0 = twcfta@U_i_g0  # initial membership matrix
B_j_q0 = twcfta@B_j_q0  # initial variable-component matrix
C_k_r0 = twcfta@C_k_r0  # initial occasion-component matrix
U_i_g = twcfta@U_i_g  # final membership matrix
B_j_q = twcfta@B_j_q  # final variable-component matrix
C_k_r = twcfta@C_k_r  # final occasion-component matrix
...
```

**TWFCTA**

``` r
twfcta = fit.twfcta(tandem_model, X_i_jk, full_tensor_shape=c(I,J,K), reduced_tensor_shape=c(G,Q,R))

# get values/attributes from the implementation with the '@' operator
U_i_g0 = twfcta@U_i_g0  # initial membership matrix
B_j_q0 = twfcta@B_j_q0  # initial variable-component matrix
C_k_r0 = twfcta@C_k_r0  # initial occasion-component matrix
U_i_g = twfcta@U_i_g  # final membership matrix
B_j_q = twfcta@B_j_q  # final variable-component matrix
C_k_r = twfcta@C_k_r  # final occasion-component matrix
...
```

### Simultaneous Models

``` r
# initialize the model
simultaneous_model = simultaneous(random_state=NULL, verbose=TRUE, init='svd', n_max_iter=10, n_loops=10, tol=1e-5, U_i_g=NULL, B_j_q=NULL, C_k_r=NULL)
```

**T3Clus & 3FKMeans**

``` r
t3clus = fit.t3clus(simultaneous_model, X_i_jk, full_tensor_shape=c(I,J,K), reduced_tensor_shape=c(G,Q,R))
tfkmeans = fit.3fkmeans(simul_model, X_i_jk, full_tensor_shape=c(I,J,K), reduced_tensor_shape=c(G,Q,R))
```

**CT3Clus**

``` r
ct3clus = fit.ct3clus(simultaneous_model, X_i_jk, full_tensor_shape=c(I,J,K), reduced_tensor_shape=c(G,Q,R), alpha=0.5)
tfkmeans = fit.3fkmeans(simul_model, X_i_jk, full_tensor_shape=c(I,J,K), reduced_tensor_shape=c(G,Q,R))
```
