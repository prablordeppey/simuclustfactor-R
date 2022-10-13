#context("tensor")  # Our file is called "test-tensor.R"
library(testthat)        # load testthat package
library(simuclustfactor)       # load our package

seed = 106382

# Test whether fold function produced the correct tensor
test_that("fold() returns appropriate tensor", {
  I=8; J=5; K=4
  X_i_jk = generate_dataset(I,J,K, seed=seed)$X_i_jk
  X_i_j_k = fold(X_i_jk, mode = 1, shape = c(I,J,K))

  expect_equal(dim(X_i_j_k), c(I,J,K))  # check tensor dimensions
  expect_equal(round(sum(X_i_j_k^2),3),61.864)  # check tensor norm
})

# Test whether unfold function produced the correct tensor
test_that("unfold() returns appropriate tensor", {
  I=8; J=5; K=4
  X_i_jk = generate_dataset(I,J,K, seed=seed)$X_i_jk
  X_i_j_k = fold(X_i_jk, mode = 1, shape = c(I,J,K))

  X_i_jk1 = unfold(X_i_j_k, mode = 1)
  X_j_ki1 = unfold(X_i_j_k, mode = 2)

  expect_equal(dim(X_i_j_k), c(I,J,K))  # check tensor dimensions
  expect_equal(round(sum(X_i_j_k^2),3),61.864)  # check tensor norm
  expect_equal(X_i_jk, X_i_jk1)  # check unfolding is same as generated
  expect_false(isTRUE(all.equal(X_j_ki1, X_i_jk1)))  # check that X_i_jk != X_j_ki

})
