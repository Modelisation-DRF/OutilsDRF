#library("testthat")
#devtools::test_file("tests/testthat/test-prob_coupe.R")

#On assume que tous les dhs sont à 0.15

test_that("prob_coupe retourne une data.table vide si l'entrée du fichier est vide", {
  # Créer un data.frame avec les colonnes requises mais aucune ligne
  donnees_vide <- data.frame(
    essence = character(0),
    id_pe = integer(0),
    no_arbre = integer(0),
    dhpcm = numeric(0),
    nbTi_ha = numeric(0),
    st_ha = numeric(0),
    stringsAsFactors = FALSE)

  resultat <- prob_coupe(donnees_vide, 10, "DET")
  expect_s3_class(resultat, "data.table")
  expect_equal(nrow(resultat), 0)
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 0", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
    id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
    no_arbre = seq(1, 12, 1),
    dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
    nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
    st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
      6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
    stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 0)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt0.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les NA en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 1", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 1)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt1.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les NA en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 2", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 2)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt2.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les NA en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 3", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 3)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt3.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les NA en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 3", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 3)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt3.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 4", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 4)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt4.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 5", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 5)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt5.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 6", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 6)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt6.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 7", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 7)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt7.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 8", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 8)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt8.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 9", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 9)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt9.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 10", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 10)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt10.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 11", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 11)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt11.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 12", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 12)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt12.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 13", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 13)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt13.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 14", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 14)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt14.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 15", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 15)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt15.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 16", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 16)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt16.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 17", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 17)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt17.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("prob_coupe retourne une data.table avec les bonnes valeurs pour le trt 18", {
  data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                          id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                          no_arbre = seq(1, 12, 1),
                          dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                          nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                          st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                    6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                          stringsAsFactors = FALSE)

  resultat <- prob_coupe(data_tree, 18)
  resultat_attendu <- read.csv(
    test_path("fixtures", "prob_trt18.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "prob_coupe") {
        # Utiliser une tolérance relative de 0.01%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.0001)
      } else {
        # On convertit les colonnes en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})


