test_that("Evolution de la qualite", {
  expected <- as.data.frame(readRDS(test_path("fixtures/evol_qualite", "ex_qualite_evol_sortie.rds")))

  actual <- as.data.frame(readRDS(test_path("fixtures/evol_qualite", "ex_qualite_evol.rds"))) %>%
    rename(sdom_bio = sdom) %>%
    rename(st_ha_cumul_gt = sum_st_ha_gt)

  # actual <- readRDS(test_path("fixtures/attrib_qualite", "entreeQualiteLourd_avec_ass_ess.rds"))

  actual <- evol_qualite(actual, "DET", nb_iter = 5)

  expect_equal(actual, expected, ignore_attr = TRUE)
})
