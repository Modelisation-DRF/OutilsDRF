test_that("Attribution de qualite en mode deterministe donne les bons resultats - leger", {
  expected <- as.data.frame(readRDS(test_path("fixtures", "resultatAttendu_attributionQualite_DET.rds")))
  actual <- attrib_qualite(ex_qualite, "DET", nb_iter = 2, seed_value = 0)

  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("Attribution de qualite en mode deterministe donne les bons resultats - lourd", {
  actual <-
    as.data.frame(readRDS(test_path("fixtures", "entreeQualiteLourd.rds"))) %>%
    attrib_qualite("DET", nb_iter = 2, seed_value = 0) %>%
    lazy_dt() %>%
    arrange(id_pe, no_arbre) %>%
    as.data.frame()

  expected <- as.data.frame(readRDS(test_path("fixtures", "resultatQualiteLourd.rds"))) %>%
    lazy_dt() %>%
    arrange(id_pe, no_arbre) %>%
    as.data.frame()

  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("Attribution de qualite en mode stochastique donne les bons resultats - lourd", {
  set.seed(1234)
  actual <- attrib_qualite(entree, "STO", nb_iter = 2, seed_value = 0) %>%
    lazy_dt() %>%
    arrange(id_pe, no_arbre, iter) %>%
    as.data.frame()

  expected <- as.data.frame(readRDS(test_path("fixtures", "resultatAttributionSTO.rds")))

  expect_equal(actual, expected)
})
