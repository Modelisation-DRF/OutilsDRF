test_that("relation_h_d() avec param par defaut estime les bonnes hauteurs", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  data_arbre_attendu <- readRDS(test_path("fixtures", "data_arbre_attendu.rds"))
  DataHt <- relation_h_d(fic_arbres = data_arbre)

  verif_obtenu <- DataHt %>%
    rename(haut_obtenu = hauteur_pred) %>%
    select(id_pe, no_arbre, essence, haut_obtenu)
  verif_attendu <- data_arbre_attendu %>%
    rename(haut_attendu = hauteur_pred) %>%
    select(id_pe, no_arbre, essence, haut_attendu)
  compare <- inner_join(verif_attendu, verif_obtenu, by = c("id_pe", "no_arbre", "essence")) %>% mutate(diff = round(haut_attendu - haut_obtenu, 2))
  # verif <- compare %>% filter(diff != 0)
  # table(verif$essence)

  # expect_equal(DataHt$hauteur_pred, data_arbre_attendu$hauteur_pred) # les lignes ne sont plus dans le même ordre
  expect_equal(compare$haut_attendu, compare$haut_obtenu)
})


test_that("relation_h_d() avec param déterministe et utilisation du grouping_vars estime les bonnes hauteurs", {
  data_arbre1 <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.1, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 10, no_arbre = 1, step = 1)
  data_arbre2 <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.1, altitude = 200, essence = "ERS", dhpcm = 11, nb_tige = 10, no_arbre = 1, step = 2)
  data_arbre3 <- data.frame(id_pe = 2, veg_pot = "MS2", sdom_bio = "2E", milieu = "0", p_tot = 1000, t_ma = 0.1, altitude = 200, essence = "SAB", dhpcm = 12, nb_tige = 10, no_arbre = 1, step = 1)
  data_arbre4 <- data.frame(id_pe = 2, veg_pot = "MS2", sdom_bio = "2E", milieu = "0", p_tot = 1000, t_ma = 0.1, altitude = 200, essence = "SAB", dhpcm = 13, nb_tige = 10, no_arbre = 1, step = 2)
  data_arbre <- bind_rows(data_arbre1, data_arbre2, data_arbre3, data_arbre4)
  DataHt <- relation_h_d(fic_arbres = data_arbre, grouping_vars = "step")
  expect_equal(DataHt$hauteur_pred, c(8.9989156, 9.5695115, 9.0633389, 9.7025151))
})


# tester le stochastique quand il y plusieurs mesures par arbre
test_that("relation_h_d() avec mode stochastique (seed=20) et nb_step=4 pour le meme arbre retourne la bonne hauteur pour la mesure 2", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))
  data_arbre_attendu_sto_2 <- readRDS(test_path("fixtures", "data_arbre_attendu_sto_2.rds"))

  DataHt <- relation_h_d(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed = 20)
  # MERGER LES FICHIERS POUR FAIRE LA COMPARAISON
  verif_obtenu <- DataHt %>%
    rename(haut_obtenu = hauteur_pred) %>%
    select(id_pe, no_arbre, essence, step, iter, haut_obtenu)
  verif_attendu <- data_arbre_attendu_sto_2 %>%
    rename(haut_attendu = hauteur_pred) %>%
    select(id_pe, no_arbre, essence, step, iter, haut_attendu)
  compare <- inner_join(verif_attendu, verif_obtenu, by = c("id_pe", "no_arbre", "essence", "step", "iter")) %>% mutate(diff = round(haut_attendu - haut_obtenu, 2))

  # expect_equal(DataHt, data_arbre_attendu_sto_2) # les lignes ne sont plus dans le même ordre
  expect_equal(compare$haut_attendu, compare$haut_obtenu)
})



# tester le stochastique quand il n'y qu'une seule mesure par arbre
test_that("relation_h_d() avec mode stochastique avec seed=20 estime les bonnes hauteurs", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))
  data_arbre <- data_arbre %>% filter(step == 1)
  data_arbre_attendu_sto1_2 <- readRDS(test_path("fixtures", "data_arbre_attendu_sto1_2.rds"))
  DataHt <- relation_h_d(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 200, nb_step = 1, seed_value = 20)
  # MERGER LES FICHIERS POUR FAIRE LA COMPARAISON
  verif_obtenu <- DataHt %>%
    rename(haut_obtenu = hauteur_pred) %>%
    select(id_pe, no_arbre, essence, step, iter, haut_obtenu)
  verif_attendu <- data_arbre_attendu_sto1_2 %>%
    rename(haut_attendu = hauteur_pred) %>%
    select(id_pe, no_arbre, essence, step, iter, haut_attendu)
  compare <- inner_join(verif_attendu, verif_obtenu, by = c("id_pe", "no_arbre", "essence", "step", "iter")) %>% mutate(diff = round(haut_attendu - haut_obtenu, 2))

  # expect_equal(DataHt, data_arbre_attendu_sto1_2)  # les lignes ne sont plus dans le même ordre
  expect_equal(compare$haut_attendu, compare$haut_obtenu)
})



test_that("relation_h_d() avec mode_simiul=STO mais grouping_vars non nul retourne un message d'erreur", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))
  expect_error(relation_h_d(fic_arbres = data_arbre, grouping_vars = "step", mode_simul = "STO"), "grouping_vars ne peut pas etre utilise avec mode_simul=STO")
})

test_that("relation_h_d() avec mode_simiul=STO sans les variables iter et step", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds")) %>% dplyr::select(-iter, -step)
  expect_error(relation_h_d(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 2, nb_step = 1), "les colonnes iter et step doivent etre dans fic_arbres avec mode_simul=STO")
})


test_that("relation_h_d() avec essence non traitée retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.1, altitude = 200, essence = "SAQ", dhpcm = 10, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec veg_pot non traitée retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE9", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.1, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec milieu manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = NA, p_tot = 1000, t_ma = 0.1, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec sdom_bio non traité retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "7E", milieu = "0", p_tot = 1000, t_ma = 0.1, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec p_tot manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = NA, t_ma = 0.1, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec t_ma manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = NA, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec altitude manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = NA, essence = "ERS", dhpcm = 10, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec essence manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = 200, essence = "", dhpcm = 10, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec dhpcm manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = 200, essence = "ERS", dhpcm = NA, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec dhpcm <9 retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = 200, essence = "ERS", dhpcm = 8, nb_tige = 1, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec nb_tige manquant retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = NA, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec un seul arbre et nb_tige=0 retourne une hauteur NA", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 0, no_arbre = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  # avec une seul arbre dans la placette et nb_tige=0, la densité du peuplement est donc 0, donc le DQ moyenne ne se calcule pas (division par 0), donc dhpcm/dq_moy est à NaN, donc hauteur_pred est à NaN
  expect_true(is.na(DataHt$hauteur_pred))
})

test_that("relation_h_d() avec 2 arbres dont un avec nb_tige=0 retourne les bonnes hauteurs", {
  data_arbre1 <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 0, no_arbre = 1)
  data_arbre2 <- data.frame(id_pe = 1, veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 1, no_arbre = 2)
  data_arbre <- bind_rows(data_arbre1, data_arbre2)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  # s'il y a au moins un arbre avec nb_tige>0, le DQ moyen va se calculer avec ceux >0, et puisque nb_tige n'entre pas directement dans la prédiction de la haut, une hauteur va se calculer pour tous les arbres
  expect_equal(DataHt$hauteur_pred, c(8.990205, 8.990205))
})

test_that("relation_h_d() avec id_pe manquant retourne les bonnes hauteurs", {
  data_arbre1 <- data.frame(id_pe = "", veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 1, no_arbre = 1)
  data_arbre2 <- data.frame(id_pe = "1", veg_pot = "FE3", sdom_bio = "1", milieu = "0", p_tot = 1000, t_ma = 0.2, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 1, no_arbre = 2)
  data_arbre <- bind_rows(data_arbre1, data_arbre2)
  DataHt <- relation_h_d(fic_arbres = data_arbre)
  # si l'identifiant de placette est manquant pour certaines lignes, c'est arbres seront considérés comme faisant partis d'une meme placette dont l'identifiant est ''
  expect_equal(DataHt$hauteur_pred, c(8.990205, 8.990205))
})

test_that("relation_h_d() avec reg_eco fonctionne", {
  data_arbre <- data.frame(id_pe = 1, veg_pot = "FE3", reg_eco = "5e", milieu = "0", p_tot = 1000, t_ma = 0.1, altitude = 200, essence = "ERS", dhpcm = 10, nb_tige = 10, no_arbre = 1, step = 1)
  DataHt <- relation_h_d(fic_arbres = data_arbre, mode_simul = "DET", reg_eco = T)
  expect_equal(round(DataHt$hauteur_pred, 2), round(9.237575, 2))
})



test_that("relation_h_d() avec un fichier de samare estime les bonnes hauteurs", {
  data_simul_samare <- readRDS(test_path("fixtures", "data_simul_samare.rds")) # une placette
  data_simul_samare_attendu_2 <- readRDS(test_path("fixtures", "data_simul_samare_attendu_2.rds"))
  nb_iter <- max(data_simul_samare$iter)
  nb_step <- max(data_simul_samare$step)
  data_simul_samare_obtenu <- relation_h_d(fic_arbres = data_simul_samare, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, reg_eco = T, dt = 5, seed_value = 20)

  verif_obtenu <- data_simul_samare_obtenu %>%
    rename(haut_obtenu = hauteur_pred) %>%
    select(id_pe, no_arbre, essence, iter, step, haut_obtenu)
  verif_attendu <- data_simul_samare_attendu_2 %>%
    rename(haut_attendu = hauteur_pred) %>%
    select(id_pe, no_arbre, essence, iter, step, haut_attendu)
  compare <- inner_join(verif_attendu, verif_obtenu, by = c("id_pe", "no_arbre", "essence", "step", "iter")) %>% mutate(diff = round(haut_attendu - haut_obtenu, 2))

  # expect_equal(data_simul_samare_obtenu$hauteur_pred, data_simul_samare_attendu_2$hauteur_pred)
  expect_equal(round(compare$haut_attendu, 1), round(compare$haut_obtenu, 1))
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

  data_obtenu <- relation_h_d(fic_arbres = data4, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, reg_eco = T, dt = 5)
  nb_rows_obtenu <- nrow(data_obtenu)
  nb_row_attendu <- nrow(data4)

  max_step_obtenu <- max(data_obtenu$step)


  expect_equal(nb_rows_obtenu, nb_row_attendu)
  expect_equal(max_step_obtenu, nb_step)
})


# ajouter test d'un fichier avec les essences qui ont une essence associée

test_that("relation_h_d() fonctionne comme attendu avec les essences qui ont des essences associées avec use_ass_ess=T mode DET", {
  data_ess_ass <- readRDS(test_path("fixtures", "data_ess_ass_det.rds")) # une placette
  data_obt <- relation_h_d(fic_arbres = data_ess_ass, mode_simul = "DET")
  data_obt_na <- data_obt %>% filter(is.na(hauteur_pred))

  expect_equal(nrow(data_ess_ass), nrow(data_obt))
  expect_equal(0, nrow(data_obt_na))
})

test_that("relation_h_d() fonctionne comme attendu avec les essences qui ont des essences associées avec use_ass_ess=T mode STO", {
  data_ess_ass <- readRDS(test_path("fixtures", "data_ess_ass_sto.rds")) # une placette
  nb_iter <- max(data_ess_ass$iter)
  nb_step <- max(data_ess_ass$step)
  data_obt <- relation_h_d(fic_arbres = data_ess_ass, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step)

  data_obt_na <- data_obt %>% filter(is.na(hauteur_pred))

  expect_equal(nrow(data_ess_ass), nrow(data_obt))
  expect_equal(0, nrow(data_obt_na))
})

test_that("relation_h_d() fonctionne comme attendu avec use_ess_ass=F en mode DET", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  data_obt <- relation_h_d(fic_arbres = data_arbre, mode_simul = "DET", use_ass_ess = F)
  data_obt_na <- data_obt %>% filter(!is.na(hauteur_pred))

  expect_equal(nrow(data_arbre), nrow(data_obt))
  expect_equal(918, nrow(data_obt_na))
})


test_that("relation_h_d() fonctionne comme attendu avec use_ess_ass=F en mode DET mode STO", {
  data_ess_ass <- readRDS(test_path("fixtures", "data_ess_ass_sto.rds")) # une placette
  nb_iter <- max(data_ess_ass$iter)
  nb_step <- max(data_ess_ass$step)
  data_obt <- relation_h_d(fic_arbres = data_ess_ass, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, use_ass_ess = F)

  data_obt_na <- data_obt %>% filter(!is.na(hauteur_pred))

  expect_equal(nrow(data_ess_ass), nrow(data_obt))
  expect_equal(50, nrow(data_obt_na))
})


test_that("relation_h_d() fonctionne avec sum_st_ha et dhp_moy dans le fichier mode DET", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  data_arbre$sum_st_ha <- 20
  data_arbre$dhp_moy <- 15
  DataHt <- relation_h_d(fic_arbres = data_arbre)

  DataHt_na <- DataHt %>% filter(is.na(hauteur_pred))

  expect_equal(0, nrow(DataHt_na)) # pas de données manquantes

  # et les colonnes sum_st_ha et dhp_moy sont encore dans le fichier
  expect_true("sum_st_ha" %in% names(DataHt))
  expect_true("dhp_moy" %in% names(DataHt))
})
test_that("relation_h_d() fonctionne avec sum_st_ha dans le fichier mode DET", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  data_arbre$sum_st_ha <- 20
  DataHt <- relation_h_d(fic_arbres = data_arbre)

  DataHt_na <- DataHt %>% filter(is.na(hauteur_pred))

  expect_equal(0, nrow(DataHt_na)) # pas de données manquantes
  # et la colonne sum_st_ha n'est plus là car elle a été calculée et onsuite on l'enleve du fichier si calculée
  expect_false("sum_st_ha" %in% names(DataHt))
})

test_that("relation_h_d() fonctionne avec dhp_moy dans le fichier mode DET", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre.rds"))
  data_arbre$dhp_moy <- 20
  DataHt <- relation_h_d(fic_arbres = data_arbre)

  DataHt_na <- DataHt %>% filter(is.na(hauteur_pred))

  expect_equal(0, nrow(DataHt_na)) # pas de données manquantes
  # et la colonne sum_st_ha n'est plus là car elle a été calculée et onsuite on l'enleve du fichier si calculée
  expect_false("dhp_moy" %in% names(DataHt))
})


test_that("relation_h_d() fonctionne avec sum_st_ha et dhp_moy dans le fichier mode STO", {
  data_arbre <- readRDS(test_path("fixtures", "data_arbre_sto.rds"))
  data_arbre$sum_st_ha <- 20
  data_arbre$dhp_moy <- 15
  DataHt <- relation_h_d(fic_arbres = data_arbre, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed = 20)

  DataHt_na <- DataHt %>% filter(is.na(hauteur_pred))

  expect_equal(0, nrow(DataHt_na)) # pas de données manquantes

  # et les colonnes sum_st_ha et dhp_moy sont encore dans le fichier
  expect_true("sum_st_ha" %in% names(DataHt))
  expect_true("dhp_moy" %in% names(DataHt))
})



# tester le stochastique d'un gros fichier
# test_that("relation_h_d() avec mode stochastique (seed=20) et nb_step=4 pour le meme arbre retourne la bonne hauteur pour la mesure 2", {
#   data_arbre3 <- readRDS(test_path("fixtures", "data_arbre_sto_gros.rds"))
#
#   tic()
#   DataHt <- relation_h_d(fic_arbres=data_arbre3, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed=20)
#   toc() # 110 sec
#
#
# })




# tester un fichier avec quelques arbres avec une ht fournie et d'autres à estimer: NON, la fonction va toujours estimer la hauteur de tous les arbres du fichier, ça sera à l'utilisateur ensuite de remplacer les ht_pred par les hauteur mesurées au besoin

# tester si le nombre d'itérations est le même dans le fichier des arbres que dans le fichier des paramètres:
# pas nécessaire, car en mode stochastique, on applique la fonction relation_h_d à une itération à la fois. Voir dans les exemples.
# Je n'ai pas voulu appliquer la relation_h_d à un fichier d'arbres qui contient toutes les itérations et toutes les step, avec un gros merge par iter/step,
# car si le fichier est gros, ça prends du temps.
# En plus, dans Natura, j'ai le fichier des arbres une seule itération à la fois. Je'aurais pu générer le fichiersdes arbres à la step 0 en multipliant
# les placettes par le nonmbre d'itérations, mais j'ai peur que ça ralentisse bcp et prenne bcp de mémoire d'avoir à travailler sur un gros fichier
# échelle arbre si plusieurs milliers de placettes



# la fct devrait être plus décortiquée
# il devrait y avoir une fct de base qui ne fait qu'appliquer l'équation à un seul arbre pour lequel on fournit les covariables nécessaires: ça serait bcp plus facile à tester
# puisqu'il y a 2 covariables qui dépendent des caractéristiques dendrométriques du peuplement, il pourrait aussi y avoir des fonctions qui applique la fct de base à chaque arbre du peuplement en calculant les caractéristiques
# mais je ne sais pas trop si c'est optimal quand on veut appliquer ça à tous les arbres de plusieurs placettes, car la prep se fait à l'échelle de la placette
