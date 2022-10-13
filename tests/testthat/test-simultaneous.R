#context("simultaneous")  # Our file is called "test-simultaneous.R"
library(testthat)        # load testthat package
library(simuclustfactor)       # load our package

seed = 106382

# Test whether twcfta function produced the correct l2norm of centroids
test_that("fit.ct3clus() returns correct  l2norms", {

  # Generate Data
  X_i_jk = generate_dataset(seed=seed)$X_i_jk
  simultaneous_model = simultaneous(verbose=F, seed=seed)

  # 3FKMeans
  O=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2), alpha = 0)
  expect_equal(round(sum(X_i_jk^2),3), 61.864)
  expect_equal(round(sum(O@B_j_q^2),3), 3)
  expect_equal(round(sum(O@C_k_r^2),3), 2)
  expect_equal(which(O@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O@Y_g_qr^2),3), 19.549)

  # CT3Clus (alpha=0.5)
  O=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2), alpha = 0.5)
  expect_equal(round(sum(X_i_jk^2),3), 61.864)
  expect_equal(round(sum(O@B_j_q^2),3), 3.0)
  expect_equal(round(sum(O@C_k_r^2),3), 2.0)
  expect_equal(which(O@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O@Y_g_qr^2),3), 20.258)

  # T3Clus
  O=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2), alpha = 1)
  expect_equal(round(sum(X_i_jk^2),3), 61.864)
  expect_equal(round(sum(O@B_j_q^2),3), 3.0)
  expect_equal(round(sum(O@C_k_r^2),3), 2.0)
  expect_equal(which(O@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O@Y_g_qr^2),3), 19.549)
})


# # Test whether t3clus function produced the correct l2norm of centroids
# test_that("fit.t3clus() returns l2norm of centroids", {
#   X_i_jk = generate_dataset(stdev = 0.01, random_state=seed)$X_i_jk
#   simultaneous_model = simultaneous(verbose=F, n_max_iter = 20, init = 'svd')
#   O=fit.t3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2))
#   expect_equal(as.integer(sum(X_i_jk^2)), 22)
#   expect_equal(as.integer(sum(O@B_j_q^2)), 3)
#   expect_equal(as.integer(sum(O@B_j_q0^2)), 3)
#   expect_equal(as.integer(sum(O@C_k_r^2)), 2)
#   expect_equal(as.integer(sum(O@Y_g_qr^2)), 70)
# })
#
# # Test whether 3fkmeans function produced the correct l2norm of centroids
# test_that("fit.3fkmeans() returns l2norm of centroids", {
#   X_i_jk = generate_dataset(stdev = 0.01, random_state=seed)$X_i_jk
#   simultaneous_model = simultaneous(verbose=F, n_max_iter = 20, init = 'svd')
#   O=fit.3fkmeans(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2))
#   expect_equal(as.integer(sum(X_i_jk^2)), 22)
#   expect_equal(as.integer(sum(O@B_j_q^2)), 3)
#   expect_equal(as.integer(sum(O@C_k_r^2)), 1)
#   # expect_equal(as.integer(sum(O@Y_g_qr^2)), 44)
# })
