test_that("param_ht() avec mode déterministe retourne les bons paramètres", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  parametre_ht <- param_ht(fic_arbres = data_arbre, mode_simul = "DET")
  parametre_ht2 <- parametre_ht %>% dplyr::select(-essence, -random_ldhp2, -res_arbre)

  # parametre_ht_attendu <- readRDS(test_path("fixtures", "parametre_ht_attendu.rds"))
  # liste_ess <- names(parametre_ht_attendu)
  # parametre_ht_attendu2 <- bind_rows(lapply(liste_ess, function(x) parametre_ht_attendu[[x]]$effet_fixe))

  parametre_ht_attendu <- ht_param_fixe # fichier interne
  liste_ess <- names(parametre_ht_attendu)
  parametre_ht_attendu2 <- bind_rows(lapply(liste_ess, function(x) parametre_ht_attendu[[x]]))

  # remplacer tous les NA par des 0
  parametre_ht_attendu3 <- parametre_ht_attendu2 %>%
    replace(is.na(.), 0) %>%
    dplyr::select(-essence)

  expect_equal(parametre_ht2, parametre_ht_attendu3)
  expect_equal(unique(parametre_ht$random_ldhp2), 0)
  expect_equal(unique(parametre_ht$res_arbre), 0)
})

test_that("param_ht() avec mode stochastique (seed=20) nb_step=1 retourne les bons paramètres des effets fixes", {
  data_arbre3 <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))

  param_sto <- param_ht(fic_arbres = data_arbre3, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed_value = 20) # il faut bcp d'iteration pour que la moyenne des resid=0, je ne sais pas pourquoi. Pour les effets fixes etr l'effet aléatoire, 200 iter suffit


  # aller chercher les paramètres déterministe dans la liste ht_param_fixe
  param_fixe <- round(as.data.frame(bind_rows(
    lapply(1:length(ht_param_fixe), function(x) {
      ht_param_fixe[[x]]
    })
  ) %>% arrange(essence) %>% dplyr::select(-essence)), 4)
  param_fixe <- param_fixe %>% replace(is.na(.), 0)
  new_order <- names(param_fixe)
  liste_ess <- names(ht_param_fixe)


  # moyenne des effets fixes
  param_sto_mean <- as.data.frame(round(param_sto %>%
    group_by(essence) %>%
    dplyr::select(-iter, -id_pe, -no_arbre, -step, -res_arbre, -random_ldhp2) %>%
    summarise_all(mean) %>% arrange(essence) %>% ungroup() %>% dplyr::select(-essence) %>%
    dplyr::select(all_of(new_order)), 4))

  # la moyenne des sto des effets fixes doit donner le deterministe
  expect_true(all.equal(param_fixe, param_sto_mean)) # ok
})


test_that("param_ht() avec mode stochastique (seed=20) retourne les bons effets aléatoires", {
  data_arbre3 <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))

  param_sto <- param_ht(fic_arbres = data_arbre3, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed_value = 20) # il faut bcp d'iteration pour que la moyenne des resid=0, je ne sais pas pourquoi. Pour les effets fixes etr l'effet aléatoire, 200 iter suffit



  # effets aléatoire du modèle
  liste <- unique(ht_param_random$essence)
  param_random <- round(as.data.frame(
    bind_rows(
      lapply(liste, function(x) {
        ht_param_random[ht_param_random$essence == x & ht_param_random$CovParm == "ldhp2", ]
      })
    ) %>% arrange(essence) %>% dplyr::select(estimate) # le arrange est nécessaire car les essences ne sont pas en ordre alphabétique
  ), 4)

  # moyenne et variance de l'effet aléatoire
  rand <- as.data.frame(round(
    param_sto %>%
      group_by(essence, step) %>% # il faut faire la variance par step, car les effets aléatoires sont les mêmes pour toutes les steps d'une placette, ça l'affect donc la variance si on prend toutes les steps en même temps
      summarise(
        mrandom_ldhp2 = mean(random_ldhp2),
        vrandom_ldhp2 = var(random_ldhp2)
      ) %>%
      ungroup() %>%
      group_by(essence) %>%
      summarise(
        mrandom_ldhp2 = mean(mrandom_ldhp2), # on a juste à faire la moyenne des steps par essence, les valeurs sont identiques
        vrandom_ldhp2 = mean(vrandom_ldhp2)
      ) %>%
      ungroup() %>%
      dplyr::select(-essence),
    4
  ))

  vrand <- rand$vrandom_ldhp2[-25] # enlever le SAB car pas d'effet aléatoire
  mrand <- rand$mrandom_ldhp2[-25] # enlever le SAB car pas d'effet aléatoire
  vrand_attendu <- param_random$estimate

  expect_equal(mrand, rep(0, 26)) # ok
  expect_equal(as.matrix(vrand), as.matrix(vrand_attendu)) # ok
})

test_that("param_ht() avec mode stochastique (seed=20) retourne les bonnes erreurs résiduelles", {
  data_arbre3 <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))


  param_sto <- param_ht(fic_arbres = data_arbre3, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed_value = 20) # il faut bcp d'iteration pour que la moyenne des resid=0, je ne sais pas pourquoi. Pour les effets fixes etr l'effet aléatoire, 200 iter suffit


  # erreurs résiduelles du modèle
  liste <- unique(ht_param_random$essence)
  param_resid <- round(as.data.frame(bind_rows(
    lapply(liste, function(x) {
      ht_param_random[ht_param_random$essence == x & ht_param_random$CovParm == "Residual", ]
    })
  ) %>% arrange(essence) %>% dplyr::select(estimate)), 4)

  res <- as.data.frame(round(
    param_sto %>%
      group_by(essence, step) %>%
      summarise(
        mres_arbre = mean(res_arbre),
        vres_arbre = var(res_arbre)
      ) %>%
      ungroup() %>%
      group_by(essence) %>%
      summarise(
        mres_arbre = mean(mres_arbre),
        vres_arbre = mean(vres_arbre)
      ) %>%
      ungroup() %>%
      dplyr::select(-essence),
    4
  ))

  vres <- res$vres_arbre
  mres <- res$mres_arbre
  vres_attendu <- param_resid$estimate

  expect_equal(mres, rep(0, 27)) # ok
  expect_equal(as.matrix(vres), as.matrix(vres_attendu)) # ok
})


test_that("param_ht() avec mode stochastique avec nb_iter=1 retourne un message d'erreur", {
  expect_error(param_ht(fic_arbres = fic_artemis_sto, mode_simul = "STO", nb_iter = 1), "Le nombre d'iterations doit etre plus grand que 1 en mode stochastique")
})




test_that("param_ht() avec mode stochastique retourne le bon nombre de lignes nb_step>9", {
  data <- readRDS(test_path("fixtures", "data_simul_samare.rds")) # une placette, 7 steps, 2 iters
  # 1202 lignes
  table(data$id_pe)
  table(data$step)

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

  param_obtenu <- param_ht(fic_arbres = data4, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, dt = 5)
  max(param_obtenu$iter)
  max(param_obtenu$step)
  nb_rows_obtenu <- nrow(param_obtenu)

  expect_equal(nb_rows_obtenu, nb_rows_soumis) # ok
})
##################################################################
##################################################################


# # tester le temps d'exécution
#
#   # essayer une vrai placette de samare de 2500m2
#   data_samare <- read_delim(file="E:\\MrnMicro\\Applic\\_Modeles-DRF\\Capsis-3ModelesDRF-20230418\\data\\samare2014\\inv_samare_cf2023.csv", delim = ';')
#   length(unique(data_samare$IDREGRO)) #2 placettes
#   data_samare1 <- data_samare %>% filter(IDREGRO=="SAMARE_CP2500")
#   names(data_samare1) <- tolower(names(data_samare1))
#
#   data_samare2 <- data_samare1 %>%
#     rename(id_pe=idregro, no_arbre=id_arbre, t_ma=temperature_moy, p_tot=precipitations_tot, dhpcm=dhp) %>%
#     mutate(milieu = substr(type_eco,4,4),
#            veg_pot = substr(type_eco,1,3),
#            sdom_bio="3O",
#            nb_tige = 0.16) %>%
#     dplyr::select(id_pe,veg_pot,milieu,sdom_bio,altitude,t_ma,p_tot,no_arbre,essence,nb_tige,dhpcm)
#
#   data_samare2_plot <- NULL
#   for (plot in 1:100){
#     temp <- data_samare2
#     temp$id_pe <- plot
#     if (plot==1) {data_samare2_plot <- temp}
#     if (plot>1) {data_samare2_plot <- bind_rows(data_samare2_plot, temp)}
#   }
#  # 100 placettes de 97 arbres chacun = 9700 arbres
#
#   data_samare2_plot2 <- NULL
#   for (iter in 1:30){
#     temp <- data_samare2_plot
#     temp$iter <- iter
#     if (iter==1) {data_samare2_plot2 <- temp}
#     if (iter>1) {data_samare2_plot2 <- bind_rows(data_samare2_plot2, temp)}
#   }
#   data_samare2_plot3 <- NULL
#   for (step in 1:5){
#     temp <- data_samare2_plot2
#     temp$step <- step
#     if (step==1) {data_samare2_plot3 <- temp}
#     if (step>1) {data_samare2_plot3 <- bind_rows(data_samare2_plot3, temp)}
#   }
#   # 1 455 000 obs = 9700 arbres (en 100 placettes) * 30 iter * 5 steps
#
#   tic()
#   param_sto <- param_ht(fic_arbres=data_samare2_plot3, mode_simul='STO', nb_iter=30, nb_step=5)
#   toc()
#   # 66.28 sec = 1min6 sec, beaucoup plus d'arbre que l'autre test mais avec bcp moins d'iteration, presque la moitié plus vite
#   # 8.4 sec avec ma modif
#
#   tic()
#   param_sto <- relation_h_d(fic_arbres=data_samare2_plot3, mode_simul='STO', nb_iter=30, nb_step=5)
#   toc()
#   # 11 sec au lieu de 2 min
#


# test_that("param_ht() avec mode stochastique (seed=20) vitesse execution", {
#
#   # 2000 placettes de 27 arbres, 5 steps, 200 iter, 54 000 000 obs
# data_arbre3 <- readRDS(test_path("fixtures", "data_arbre_sto_gros.rds"))
#
# tic()
# param_sto_ht <- param_ht(fic_arbres=data_arbre3, mode_simul='STO', nb_iter=200, nb_step=5, seed_value=20)
# toc()
# avant modifications: 175.27 sec elapsed = 2.9 min
# après modifications: 45 sec
#
#
# })
