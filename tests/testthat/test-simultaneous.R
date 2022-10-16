#context("simultaneous")  # Our file is called "test-simultaneous.R"
library(testthat)        # load testthat package
library(simuclustfactor)       # load our package

seed = 106382

# Test whether twcfta function produced the correct l2norm of centroids
test_that("fit.ct3clus() returns correct  l2norms", {

  # Generate Data
  X_i_jk = generate_dataset(seed=seed)$X_i_jk

  # Inits
  simultaneous_model = simultaneous(verbose=T, seed=seed, init='svd')
  O=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2))
  expect_equal(round(sum(X_i_jk^2),3), 61.864)
  expect_equal(round(sum(O@B_j_q^2),3), 3)
  expect_equal(round(sum(O@C_k_r^2),3), 2)
  expect_equal(which(O@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O@Y_g_qr^2),3), 20.258)

  simultaneous_model = simultaneous(verbose=F, seed=seed, init='random')
  O=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2))
  expect_equal(round(sum(X_i_jk^2),3), 61.864)
  expect_equal(round(sum(O@B_j_q^2),3), 3)
  expect_equal(round(sum(O@C_k_r^2),3), 2)
  expect_equal(which(O@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O@Y_g_qr^2),3), 19.009)

  simultaneous_model = simultaneous(verbose=F, seed=seed, init='twcfta')
  O=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2))
  expect_equal(round(sum(X_i_jk^2),3), 61.864)
  expect_equal(round(sum(O@B_j_q^2),3), 3)
  expect_equal(round(sum(O@C_k_r^2),3), 2)
  expect_equal(which(O@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O@Y_g_qr^2),3), 20.258)

  simultaneous_model = simultaneous(verbose=F, seed=seed, init='twfcta')
  O=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2))
  expect_equal(round(sum(X_i_jk^2),3), 61.864)
  expect_equal(round(sum(O@B_j_q^2),3), 3)
  expect_equal(round(sum(O@C_k_r^2),3), 2)
  expect_equal(which(O@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O@Y_g_qr^2),3), 20.258)

  # 3FKMeans
  simultaneous_model = simultaneous(verbose=F, seed=seed)
  O_ct3=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2), alpha = 0)
  expect_equal(round(sum(O_ct3@B_j_q^2),3), 3)
  expect_equal(round(sum(O_ct3@C_k_r^2),3), 2)
  expect_equal(which(O_ct3@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O_ct3@Y_g_qr^2),3), 19.549)

  O_3fk=fit.3fkmeans(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2))
  expect_equal(round(sum(O_3fk@B_j_q^2),3), 3)
  expect_equal(round(sum(O_3fk@C_k_r^2),3), 2)
  expect_equal(which(O_3fk@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O_3fk@Y_g_qr^2),3), 19.549)

  expect_equal(O_3fk@Y_g_qr, O_ct3@Y_g_qr)  # check 3FK output same as from ct3clus

  # CT3Clus (alpha=0.5)
  O=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2), alpha = 0.5)
  expect_equal(round(sum(X_i_jk^2),3), 61.864)
  expect_equal(round(sum(O@B_j_q^2),3), 3.0)
  expect_equal(round(sum(O@C_k_r^2),3), 2.0)
  expect_equal(which(O@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O@Y_g_qr^2),3), 20.258)

  # T3Clus
  O_ct3=fit.ct3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2), alpha = 1)
  expect_equal(round(sum(O_ct3@B_j_q^2),3), 3.0)
  expect_equal(round(sum(O_ct3@C_k_r^2),3), 2.0)
  expect_equal(which(O_ct3@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O_ct3@Y_g_qr^2),3), 19.549)

  O_t3=fit.t3clus(simultaneous_model, X_i_jk, c(8,5,4), c(3,3,2))
  expect_equal(round(sum(O_t3@B_j_q^2),3), 3)
  expect_equal(round(sum(O_t3@C_k_r^2),3), 2)
  expect_equal(which(O_t3@U_i_g==1), c(1,8,10,13,14,15,19,20))
  expect_equal(round(sum(O_t3@Y_g_qr^2),3), 19.549)

  expect_equal(O_t3@Y_g_qr, O_ct3@Y_g_qr)  # check t3clus output same as from ct3clus

})
