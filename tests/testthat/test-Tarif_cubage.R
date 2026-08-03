test_that("cubage() avec mode déterministe et fichier param fourni estime les bons volumes", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_vol.rds"))
  data_arbre_attendu <- readRDS(test_path("fixtures", "data_arbre_vol_attendu.rds"))
  DataVol <- cubage(fic_arbres = data_arbre, mode_simul = "DET")

  verif_obtenu <- DataVol %>%
    rename(vol_obtenu = vol_dm3) %>%
    select(id_pe, no_arbre, essence, vol_obtenu)
  verif_attendu <- data_arbre_attendu %>%
    rename(vol_attendu = vol_dm3) %>%
    select(id_pe, no_arbre, essence, vol_attendu)
  compare <- inner_join(verif_attendu, verif_obtenu, by = c("id_pe", "no_arbre", "essence")) %>% mutate(diff = round(vol_attendu - vol_obtenu, 2))

  expect_equal(compare$vol_attendu, compare$vol_obtenu)
})

test_that("cubage() avec mode stochastique estime les bons volumes", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_attendu_sto_2.rds"))
  # data_arbre_attendu_sto <- readRDS(test_path("fixtures", "data_arbre_vol_attendu_sto_2.rds"))
  data_arbre_attendu_sto <- readRDS(test_path("fixtures", "data_arbre_vol_attendu_sto_2a.rds"))
  DataVol <- cubage(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed_value = 20)

  verif_obtenu <- DataVol %>%
    rename(vol_obtenu = vol_dm3) %>%
    select(id_pe, no_arbre, essence, iter, step, vol_obtenu)
  verif_attendu <- data_arbre_attendu_sto %>%
    rename(vol_attendu = vol_dm3) %>%
    select(id_pe, no_arbre, essence, iter, step, vol_attendu)
  compare <- inner_join(verif_attendu, verif_obtenu, by = c("id_pe", "no_arbre", "essence", "step", "iter")) %>% mutate(diff = round(vol_attendu - vol_obtenu, 2))

  expect_equal(round(compare$vol_attendu, 1), round(compare$vol_obtenu, 1))
})

test_that("cubage() avec mode_simiul=STO sans les variables iter et step", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds")) %>% dplyr::select(-iter, -step)
  expect_error(cubage(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 2, nb_step = 1), "les colonnes iter et step doivent etre dans fic_arbres avec mode_simul=STO")
})


test_that("cubage() avec un fichier de samare estime les bons volumes", {
  data_simul_samare <- readRDS(test_path("fixtures", "data_simul_samare.rds"))
  # data_simul_samare_attendu_2 <- readRDS(test_path("fixtures", "data_simul_samare_attendu_2.rds"))
  data_simul_samare_attendu_2 <- readRDS(test_path("fixtures", "data_simul_samare_attendu_2a.rds"))
  nb_iter <- max(data_simul_samare$iter)
  nb_step <- max(data_simul_samare$step)
  ht <- relation_h_d(fic_arbres = data_simul_samare, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, reg_eco = T, dt = 5, seed_value = 20)
  data_simul_samare_obtenu <- cubage(fic_arbres = ht, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, seed_value = 20)

  verif_obtenu <- data_simul_samare_obtenu %>%
    rename(vol_obtenu = vol_dm3) %>%
    select(id_pe, no_arbre, essence, iter, step, vol_obtenu)
  verif_attendu <- data_simul_samare_attendu_2 %>%
    rename(vol_attendu = vol_dm3) %>%
    select(id_pe, no_arbre, essence, iter, step, vol_attendu)
  compare <- inner_join(verif_attendu, verif_obtenu, by = c("id_pe", "no_arbre", "essence", "step", "iter")) %>% mutate(diff = round(vol_attendu - vol_obtenu, 2))

  expect_equal(round(compare$vol_attendu, 0), round(compare$vol_obtenu, 0))
})


test_that("relation_h_d() avec mode stochastique retourne le bon nombre de lignes avec nb_step>9", {
  data <- readRDS(test_path("fixtures", "data_simul_samare.rds")) # une placette, 7 steps, 2 iters

  # ajouter une 8e step
  step <- data %>%
    filter(step == 7) %>%
    mutate(step = 8)
  data2 <- bind_rows(data, step)

  # ajouter une 9e step
  step <- data %>%
    filter(step == 7) %>%
    mutate(step = 9)
  data3 <- bind_rows(data2, step)

  # ajouter une 10e step
  step <- data %>%
    filter(step == 7) %>%
    mutate(step = 10)
  data4 <- bind_rows(data3, step)
  nb_rows_soumis <- nrow(data4)

  nb_iter <- max(data4$iter)
  nb_step <- max(data4$step)

  data5 <- relation_h_d(fic_arbres = data4, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, reg_eco = T, dt = 5)

  data_obtenu <- cubage(fic_arbres = data5, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step)


  nb_rows_obtenu <- nrow(data_obtenu)
  nb_row_attendu <- nrow(data4)

  max_step_obtenu <- max(data_obtenu$step)


  expect_equal(nb_rows_obtenu, nb_row_attendu)
  expect_equal(max_step_obtenu, nb_step)
})


# ajouter test d'un fichier avec les essences qui ont une essence associée

test_that("cubage() fonctionne comme attendu avec les essences qui ont des essences associées mode DET", {
  data_ess_ass <- readRDS(test_path("fixtures", "data_ess_ass_det.rds"))
  data_ess_ass <- data_ess_ass %>% mutate(hauteur_pred = 10)
  data_obt <- cubage(fic_arbres = data_ess_ass, mode_simul = "DET")
  data_obt_na <- data_obt %>% filter(is.na(vol_dm3))

  expect_equal(nrow(data_ess_ass), nrow(data_obt))
  expect_equal(5, nrow(data_obt_na))
})

test_that("cubage() fonctionne comme attendu avec les essences qui ont des essences associées mode STO", {
  data_ess_ass <- readRDS(test_path("fixtures", "data_ess_ass_sto.rds"))
  data_ess_ass <- data_ess_ass %>% mutate(hauteur_pred = 10)
  nb_iter <- max(data_ess_ass$iter)
  nb_step <- max(data_ess_ass$step)
  data_obt <- cubage(fic_arbres = data_ess_ass, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step)
  data_obt_na <- data_obt %>% filter(is.na(vol_dm3))

  expect_equal(nrow(data_ess_ass), nrow(data_obt))
  expect_equal(5 * nb_iter, nrow(data_obt_na))
})

test_that("cubage() fonctionne comme attendu avec use_ess_ass=F en mode DET", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_vol.rds"))
  data_obt <- cubage(fic_arbres = data_arbre, mode_simul = "DET", use_ass_ess = F)
  data_obt_na <- data_obt %>% filter(is.na(vol_dm3))

  expect_equal(nrow(data_arbre), nrow(data_obt))

  expect_equal(39, nrow(data_obt_na))
})

test_that("cubage() fonctionne comme attendu avec use_ess_ass=F en mode STO", {
  data_ess_ass <- readRDS(test_path("fixtures", "data_ess_ass_sto.rds"))
  data_ess_ass <- data_ess_ass %>% mutate(hauteur_pred = 10)
  nb_iter <- max(data_ess_ass$iter)
  nb_step <- max(data_ess_ass$step)
  data_obt <- cubage(fic_arbres = data_ess_ass, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, use_ass_ess = F)
  data_obt_na <- data_obt %>% filter(is.na(vol_dm3))

  expect_equal(nrow(data_ess_ass), nrow(data_obt))

  expect_equal(1950, nrow(data_obt_na))
})




test_that("cubage() avec mode déterministe estime les bons volumes utilisables", {

  data_arbre_attendu <- readRDS(test_path("fixtures/volume_utilisable", "data_vmu_attendu.rds")) %>% filter(dhpcm %in% c(9.1, 12, 18, 25) | essence %in% c('CHB','PED'))

  #table(compare$dhpcm)

  data_arbre <- data_arbre_attendu %>% dplyr::select(-vol_dm3)

  data_arbre_obtenu <- cubage(fic_arbres = data_arbre, mode_simul = "DET", type='UTIL') %>% rename(vol_dm3_obtenu = vol_dm3)

  compare <- left_join(data_arbre_attendu, data_arbre_obtenu, by = join_by(id_pe, no_arbre, essence, dhpcm, hauteur_pred))

  expect_equal(round(compare$vol_dm3,0), round(compare$vol_dm3_obtenu,0)) # le fichier des paramètres que la DIF m'a envoyé ne contient que 4 décimales, et son fichier de volume a été calculé avec toutes les décimales
})


test_that("cubage() avec mode_simiul=STO et type=UTIL", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds")) %>% dplyr::select(-iter, -step)
  expect_error(cubage(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 2, nb_step = 1, type='UTIL'), "Le mode stochastique ne peut pas etre utilise avec type=UTIL")
})


# test_that("cubage() avec mode stochastique fonctionne avec un gros fichier", {
#
#   data_arbre3 <- readRDS(test_path("fixtures", "data_arbre_sto_gros.rds"))
#
#   tic()
#   DataHt <- relation_h_d(fic_arbres=data_arbre3, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed=20)
#   toc() # 132 sec
#
#   tic()
#   DataVol <- cubage(fic_arbres=DataHt, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed=20)
#   toc() # 130 sec
#
# })
