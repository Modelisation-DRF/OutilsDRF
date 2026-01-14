test_that("Evolution de la qualite mode DET", {
  expected <- as.data.frame(readRDS(test_path("fixtures/evol_qualite", "ex_qualite_evol_sortie.rds")))
  arbres <- as.data.frame(readRDS(test_path("fixtures/evol_qualite", "ex_qualite_evol.rds")))
  actual <- evol_qualite(arbres, "DET")
  expect_equal(actual, expected, ignore_attr = TRUE)
})


test_that("Evolution de la qualite mode STO", {
  expected <- as.data.frame(readRDS(test_path("fixtures/evol_qualite", "attendu_sto.rds")))
  arbres <- as.data.frame(readRDS(test_path("fixtures/evol_qualite", "ex_qualite_evol_sto.rds")))
  actual <- evol_qualite(arbres, "STO", nb_iter = 5, seed_value = 1)
  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("Evolution de la qualite mode STO nb_iter=1", {
  arbres <- as.data.frame(readRDS(test_path("fixtures/evol_qualite", "ex_qualite_evol_sto.rds"))) %>% filter(iter==1)
  expect_no_error(evol_qualite(arbres, "STO", nb_iter = 1, seed_value = 1))

})

