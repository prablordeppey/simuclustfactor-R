#context("tensor")  # Our file is called "test-tensor.R"
library(testthat)        # load testthat package
library(simuclustfactor)       # load our package

seed = 106382

# Test whether fold function produced the correct tensor
test_that("fold() returns appropriate tensor", {
  I=8; J=5; K=4
  X_i_jk = generate_dataset(I,J,K, seed=seed)$X_i_jk

  X_i_j_k = fold(X_i_jk, mode = 1, shape = c(I,J,K))  # mode 1
  X_j_ki = unfold(X_i_j_k, mode = 2)
  X_k_ij = unfold(X_i_j_k, mode = 3)

  expect_equal(dim(X_i_j_k), c(I,J,K))  # check tensor dimensions
  expect_equal(dim(X_i_jk), c(I,J*K))  # check tensor dimensions
  expect_equal(dim(X_j_ki), c(J,K*I))  # check tensor dimensions
  expect_equal(dim(X_k_ij), c(K,I*J))  # check tensor dimensions

  X_i_j_k1 = fold(X_j_ki, mode = 2, shape = c(I,J,K))  # mode 2
  X_i_j_k2 = fold(X_k_ij, mode = 3, shape = c(I,J,K))  # mode 3
  expect_equal(X_i_j_k1, X_i_j_k)
  expect_equal(X_i_j_k2, X_i_j_k)

})

# Test whether unfold function produced the correct tensor
test_that("unfold() returns appropriate matrix", {
  I=8; J=5; K=4
  X_i_jk = generate_dataset(I,J,K, seed=seed)$X_i_jk
  X_i_j_k = fold(X_i_jk, mode = 1, shape = c(I,J,K))

  X_i_jk1 = unfold(X_i_j_k, mode = 1)
  X_j_ki = unfold(X_i_j_k, mode = 2)
  X_k_ij = unfold(X_i_j_k, mode = 3)

  expect_equal(dim(X_i_jk1), c(I,J*K))  # I,JK
  expect_equal(dim(X_j_ki), c(J,K*I))  # I,JK
  expect_equal(dim(X_k_ij), c(K,I*J))  # I,JK
  expect_equal(X_i_jk, X_i_jk1)  # check unfolding is same as generated
  expect_false(isTRUE(all.equal(X_j_ki, X_i_jk1)))  # check that X_i_jk != X_j_ki

})
