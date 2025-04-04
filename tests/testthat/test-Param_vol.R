test_that("param_vol() avec mode déterministe retourne les bons paramètres", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_vol.rds"))

  # parametre_vol_attendu <- readRDS(test_path("fixtures", "param_vol_attendu.rds"))
  # parametre_vol_attendu <- parametre_vol_attendu[[1]]$effet_fixe %>% dplyr::select(-iter, -essence_volume)
  parametre_vol_attendu <- tarif_param_fixe %>%
    separate_wider_delim(col = beta_ess, names = c("parm", "essence"), delim = "_", too_few = "align_start") %>%
    filter(!is.na(essence)) %>%
    group_by(essence) %>%
    pivot_wider(names_from = parm, values_from = Estimate) %>%
    mutate(b1 = tarif_param_fixe[tarif_param_fixe$beta_ess == "b1", 2]) %>%
    ungroup() %>%
    dplyr::select(-essence)

  parametre_vol <- param_vol(fic_arbres = data_arbre, mode_simul = "DET")
  parametre_vol <- parametre_vol$effet_fixe %>% dplyr::select(-essence, -random_plot, -resid)
  expect_equal(parametre_vol, parametre_vol_attendu)
})

test_that("param_vol() avec mode stochastique avec seed=20 retourne les bons paramètres des effets fixes", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))


  # la moyenne des sto doit donner le deterministe
  param_tarif_tr <- tarif_param_fixe %>%
    filter(beta_ess != "b1") %>%
    mutate(
      essence_volume = substr(beta_ess, 4, 7),
      beta = substr(beta_ess, 1, 2)
    ) %>%
    dplyr::select(-beta_ess) %>%
    group_by(essence_volume) %>%
    pivot_wider(names_from = beta, values_from = Estimate) %>%
    ungroup()
  param_tarif_tr$b1 <- tarif_param_fixe[tarif_param_fixe$beta_ess == "b1", "Estimate"]
  param_fixe <- round(as.data.frame(param_tarif_tr %>% dplyr::select(-essence_volume)), 4)

  param_sto <- param_vol(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 200, nb_step = 5)
  param_sto <- param_sto$effet_fixe

  param_fixe_sto <- round(as.data.frame(param_sto %>% group_by(essence) %>% dplyr::select(-iter) %>%
    summarise_all(mean) %>% arrange(essence) %>% dplyr::select(-essence)), 4)

  expect_true(all.equal(param_fixe, param_fixe_sto))
})


test_that("param_vol() avec mode stochastique (seed=20) retourne les bons effets aléatoires", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))

  param_random <- round(data.frame("random" = tarif_param_random[1, 4]), 4)


  param_sto <- param_vol(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed_value = 20)
  param_sto <- param_sto$random

  param_mrandom_sto <- round(as.data.frame(param_sto %>% summarise(moy = mean(random_plot))), 4)

  param_vrandom_sto <- round(as.data.frame(param_sto %>% summarise(random = var(random_plot))), 4)

  expect_true(all.equal(data.frame("moy" = 0), param_mrandom_sto))
  expect_true(all.equal(param_random, param_vrandom_sto))
})

test_that("param_vol() avec mode stochastique (seed=20) retourne les bonnes erreurs résiduelles", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))
  # max(data_arbre$iter) # 200
  # max(data_arbre$step) # 5
  param_random <- round(data.frame("random" = tarif_param_random[7:32, 4]), 4)

  param_sto <- param_vol(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed_value = 20)
  param_sto <- param_sto$random

  param_mrandom_sto <- round(as.data.frame(param_sto %>% dplyr::select(-iter, -id_pe, -step, -no_arbre, -random_plot, -essence) %>% summarise_all(mean)), 4)

  param_vrandom_sto <- round(as.data.frame(param_sto %>% dplyr::select(-iter, -id_pe, -step, -no_arbre, -random_plot, -essence) %>% summarise_all(var) %>%
    pivot_longer(names_to = "essence", values_to = "random", cols = BOG:TIL) %>% dplyr::select(-essence)), 4)


  expect_equal(sum(param_mrandom_sto), 0)
  expect_true(all.equal(param_random, param_vrandom_sto))
})


test_that("param_vol() avec mode stochastique avec nb_iter=1 retourne un message d'erreur", {
  expect_error(param_vol(fic_arbres = fic_artemis_sto, mode_simul = "STO", nb_iter = 1), "Le nombre d'iterations doit etre plus grand que 1 en mode stochastique")
})



test_that("param_vol() avec mode stochastique retourne le bon nombre de lignes dans random avec nb_step>9", {
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

  param_obtenu <- param_vol(fic_arbres = data4, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step)
  param_random_obtenu <- param_obtenu$random

  nb_rows_obtenu <- nrow(param_random_obtenu)
  # nb_row_attendu <- nrow(data4 %>% dplyr::select(id_pe,no_arbre) %>% unique)*nb_iter*nb_step
  nb_row_attendu <- nrow(data4)

  max_step_obtenu <- max(param_random_obtenu$step)

  expect_equal(nb_rows_obtenu, nb_row_attendu) # ok

  expect_equal(max_step_obtenu, nb_step) # ok
})


#
# test_that("param_vol() avec mode stochastique (seed=20) vitesse execution", {
#
#   # 2000 placettes de 27 arbres, 5 steps, 200 iter, 54 000 000 obs
# data_arbre3 <- readRDS(test_path("fixtures", "data_arbre_sto_gros.rds"))
#
# tic()
# param_sto_vol <- param_vol(fic_arbres=data_arbre3, mode_simul='STO', nb_iter=200, nb_step=5)
# toc()
#   # avant modifications: après 1 heure, ce n'est pas encore fini
#   # après modifications: 105 sec
#
#
# })
