# context("utils")  # Our file is called "test-utils.R"
library(testthat)        # load testthat package
library(simuclustfactor)       # load our package

seed = 106382

# Test whether pseudof.reduced function produced the correct l2norm of centroids
test_that("pseudof.reduced() returns 50", {
  pf = pseudof.reduced(bss = 100,wss = 8,full_tensor_shape = c(8,5,4), reduced_tensor_shape = c(3,3,2))
  expect_equal(pf, 50)
})


# Test whether pseudof.full function produced the correct l2norm of centroids
test_that("pseudof.full() returns 4.55", {
  pf = pseudof.full(bss = 100,wss = 8,full_tensor_shape = c(8,5,4), reduced_tensor_shape = c(3,3,2))
  expect_equal(round(pf,2), 4.55)
})

# Test whether generate_rmfm function produced the correct membership matrix
test_that("generate_rmfm() returns the correct label set", {
  U_i_g = generate_rmfm(I=8,G=3,seed = seed)
  expect_equal(which(U_i_g==1), c(1,8,10,13,14,15,19,20))
})


# Test whether onekmeans function produced the correct membership matrix
test_that("onekmeans() returns correct label set", {
  # data matrix
  X_i_jk = generate_dataset(seed=seed)$X_i_jk

  # initial membership matrix
  U_i_g0 = generate_rmfm(I=8,G=3,seed = seed)
  U_i_g = U_i_g0

  # perform 10 runs of the onekmeans
  for(i in 1:1000){
    U_i_g = onekmeans(X_i_jk, U_i_g = U_i_g, G = 3)
  }

  # print(U_i_g)

  # check labelset for final iteration is correct
  expect_equal(which(U_i_g==1), c(1,8,10,13,14,15,19,20))
})


