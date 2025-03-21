#devtools::test_file("tests/testthat/test-calcul_vol_bille.R")

test_that("calcul_vol_bille retourne une data.table vide si l'entrée est vide", {
  # Créer un data.frame avec les colonnes requises mais aucune ligne
  donnees_vide <- data.frame(
    essence = character(0),
    id_pe = integer(0),
    no_arbre = integer(0),
    HAUTEUR_M = numeric(0),
    sdom_bio = character(0),
    cl_drai = character(0),
    veg_pot = character(0),
    DHP_Ae = numeric(0),
    nbTi_ha = numeric(0),
    st_ha = numeric(0),
    ALTITUDE = numeric(0),
    stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(donnees_vide)
  expect_s3_class(resultat, "data.table")
  expect_equal(nrow(resultat), 0)
})

test_that("calcul_vol_bille retourne une data.table de valeur NA si l'entrée ne contient que des essences qui ne sont pas dans defil_liste_ess", {
  data_billes <- data.frame(essence = c('ABC', "DEF", "GHI"),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage long",
                               long_grade1 = 12,
                               diam_grade1 = 12,
                               nom_grade2 = "sciage court",
                               long_grade2 = 8,
                               diam_grade2 = 10,
                               nom_grade3 = "pate",
                               long_grade3 = 4,
                               diam_grade3 = 10)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Aucune_ess_valide.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.015)
      } else {
        # On convertit les NA en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne une data.table de NA si l'entrée ne contient que long_grade plus grand que les hauteurs", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage long",
                               long_grade1 = 100,
                               diam_grade1 = 12,
                               nom_grade2 = "sciage court",
                               long_grade2 = 100,
                               diam_grade2 = 10,
                               nom_grade3 = "pate",
                               long_grade3 = 100,
                               diam_grade3 = 10)

  resultat_attendu <- read.csv(
    #Même fichier résultat
    test_path("fixtures", "Aucune_ess_valide.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.015)
      } else {
        # On convertit les NA en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne une data.table de NA si l'entrée ne contient que diam_grade plus grand que les diamètres prédits", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage long",
                               long_grade1 = 12,
                               diam_grade1 = 50,
                               nom_grade2 = "sciage court",
                               long_grade2 = 8,
                               diam_grade2 = 50,
                               nom_grade3 = "pate",
                               long_grade3 = NA,
                               diam_grade3 = 90)

  resultat_attendu <- read.csv(
    #Même fichier résultat
    test_path("fixtures", "Aucune_ess_valide.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(as.numeric(resultat[[col]]), as.numeric(resultat_attendu[[col]]), tolerance = 0.015)
      } else {
        # On convertit les NA en char pour les 2 tables, puisque le type ne dérange pas pour les tests(vecteur de NA dans R donne un vecteur logical...)
        expect_equal(as.character(resultat[[col]]), as.character(resultat_attendu[[col]]))
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne les bons volumes et le bon nombre de billes lorsque tous les types de billes sont présents et complets", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
             id_pe = rep(1, 3),
             no_arbre = 1:3,
             sdom_bio = rep(c("3OUEST"), 3),
             cl_drai = rep(NA, 3),
             veg_pot = rep('MS2', 3),
             DHP_Ae = c(120, 150, 300),
             HT_REELLE_M = rep(0, 3),
             HAUTEUR_M = c(13, 20, 28),
             nbTi_ha = NA,
             st_ha = NA,
             ALTITUDE = NA,
             stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage long",
                               long_grade1 = 12,
                               diam_grade1 = 12,
                               nom_grade2 = "sciage court",
                               long_grade2 = 8,
                               diam_grade2 = 10,
                               nom_grade3 = "pate",
                               long_grade3 = 4,
                               diam_grade3 = 10)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_12_8_4pieds.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne les bons volumes et le bon nombre de billes lorsque tous les types de billes sont présents, mais le 3eme est imcomplet", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                           id_pe = rep(1, 3),
                           no_arbre = 1:3,
                           sdom_bio = rep(c("3OUEST"), 3),
                           cl_drai = rep(NA, 3),
                           veg_pot = rep('MS2', 3),
                           DHP_Ae = c(120, 150, 300),
                           HT_REELLE_M = rep(0, 3),
                           HAUTEUR_M = c(13, 20, 28),
                           nbTi_ha = NA,
                           st_ha = NA,
                           ALTITUDE = NA,
                           stringsAsFactors = FALSE)

 resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage long",
                              long_grade1 = 12,
                              diam_grade1 = 12,
                              nom_grade2 = "sciage court",
                              long_grade2 = 8,
                              diam_grade2 = 10,
                              nom_grade3 = "pate",
                              long_grade3 = NA,
                              diam_grade3 = 6)
 resultat_attendu <- read.csv(
   test_path("fixtures", "Test_billes_12_8_indefinie.csv"),
   sep = ";",
   stringsAsFactors = FALSE
 )

 setDT(resultat_attendu)

 cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
 for (col in cols_identification) {
   if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
     if (col == "vol_bille_dm3") {
       # Utiliser une tolérance relative de 1.5%
       expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
     } else {
       # Pour les autres colonnes, on peut garder une comparaison stricte
       expect_equal(resultat[[col]], resultat_attendu[[col]])
     }
   }
 }
 expect_equal(nrow(resultat), nrow(resultat_attendu))
 })

test_that("calcul_vol_bille retourne les bons volumes et le bon nombre de billes lorsque deux types de billes sont présents et complets", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage court",
                               long_grade1 = 8,
                               diam_grade1 = 20,
                               nom_grade2 = "pate",
                               long_grade2 = 4,
                               diam_grade2 = 10)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_8_4pieds.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne les bons volumes et le bon nombre de billes lorsque deux types de billes sont présents, mais le second est incomplet", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage court",
                               long_grade1 = 8,
                               diam_grade1 = 12,
                               nom_grade2 = "pate",
                               long_grade2 = NA,
                               diam_grade2 = 9)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_8_indefinie.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne les bons volumes et le bon nombre de billes lorsqu'un seul type de bille est présent et complet", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "pate",
                               long_grade1 = 4,
                               diam_grade1 = 10)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_4pieds.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne les bons volumes et le bon nombre de billes lorsqu'un seul type de bille est présent, mais incomplet", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "vmb",
                               long_grade1 = NA,
                               diam_grade1 = 9)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_indefinie.csv"),
    sep = ";",
    #À la base, vecteur de NA -> vecteur Logical.
    colClasses = c(long_bille_pied = "numeric"),
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne les bons volumes et le bon nombre de billes lorsque tout les types n'ont pas de valeur de longueur définie", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage long",
                               long_grade1 = NA,
                               diam_grade1 = 10,
                               nom_grade2 = "sciage court",
                               long_grade2 = NA,
                               diam_grade2 = 8,
                               nom_grade3 = "pate",
                               long_grade3 = NA,
                               diam_grade3 = 6)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_all_indefinie.csv"),
    sep = ";",
    #À la base, vecteur de NA -> vecteur Logical.
    colClasses = c(long_bille_pied = "numeric"),
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne les bons volumes et le bon nombre de billes lorsque le premier type est complet et
          les deux autres n'ont pas de valeur de longueur définie", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage long",
                               long_grade1 = 8,
                               diam_grade1 = 12,
                               nom_grade2 = "sciage court",
                               long_grade2 = NA,
                               diam_grade2 = 9,
                               nom_grade3 = "pate",
                               long_grade3 = NA,
                               diam_grade3 = 5)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_8_indefinie_indefinie.csv"),
    sep = ";",
    #À la base, vecteur de NA -> vecteur Logical.
    colClasses = c(long_bille_pied = "numeric"),
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne les bons volumes et le bon nombre de billes lorsque 2 types de billes ont une longueur indéfinie", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA,
                            stringsAsFactors = FALSE)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage long",
                               long_grade1 = NA,
                               diam_grade1 = 14,
                               nom_grade2 = "sciage court",
                               long_grade2 = NA,
                               diam_grade2 = 7)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_indefinie_indefinie.csv"),
    sep = ";",
    #À la base, vecteur de NA -> vecteur Logical.
    colClasses = c(long_bille_pied = "numeric"),
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

#########################################################################################
#Test des autres fichiers, même si tous les cas généraux sont traités

test_that("calcul_vol_bille retourne les bons volumes de ce fichier (8pieds)", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage court",
                               long_grade1 = 8,
                               diam_grade1 = 20)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_8pieds.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )
  setDT(resultat_attendu)
  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})

test_that("calcul_vol_bille retourne les bons volumes de ce fichier (12 pieds)", {
  data_billes <- data.frame(essence = rep(c('BOP'), 3),
                            id_pe = rep(1, 3),
                            no_arbre = 1:3,
                            sdom_bio = rep(c("3OUEST"), 3),
                            cl_drai = rep(NA, 3),
                            veg_pot = rep('MS2', 3),
                            DHP_Ae = c(120, 150, 300),
                            HT_REELLE_M = rep(0, 3),
                            HAUTEUR_M = c(13, 20, 28),
                            nbTi_ha = NA,
                            st_ha = NA,
                            ALTITUDE = NA)

  resultat <- calcul_vol_bille(data_billes, nom_grade1 = "sciage long", long_grade1 = 12, diam_grade1 = 12)
  resultat_attendu <- read.csv(
    test_path("fixtures", "Test_billes_12pieds.csv"),
    sep = ";",
    stringsAsFactors = FALSE
  )

  setDT(resultat_attendu)

  cols_identification <- c("id_pe", "no_arbre", "dhpcm", "ht", "vol_bille_dm3", "grade_bille", "diam_fb_cm", "long_bille_pied")
  for (col in cols_identification) {
    if (col %in% names(resultat) && col %in% names(resultat_attendu)) {
      if (col == "vol_bille_dm3") {
        # Utiliser une tolérance relative de 1.5%
        expect_equal(resultat[[col]], resultat_attendu[[col]], tolerance = 0.015)
      } else {
        # Pour les autres colonnes, on peut garder une comparaison stricte
        expect_equal(resultat[[col]], resultat_attendu[[col]])
      }
    }
  }
  expect_equal(nrow(resultat), nrow(resultat_attendu))
})
