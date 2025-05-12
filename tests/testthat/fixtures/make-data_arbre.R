# Fichier test, une placette par veg_pot traitée (34)
# un leur associe un des 11 sdom et 10 milieu traités
# on met un arbre de chacune des 63 essences traitées dans chacune des 34 placettes

# fichier association des essences pour le modèle de hauteur-dhp
unique(ht_ass_ess$essence) # 63
unique(ht_ass_ess$essence_hauteur) # 27
view(ht_ass_ess)
# fichier d'associasions des sdom, pert, etc. pour les modèles ht-dhp, un fichier par essence
ht_liste_ess # 27

unique(ht_ass_vp$veg_pot) # 34
unique(ht_ass_vp$vp) # 31

unique(ht_ass_sd$sdom_bio) # 11
unique(ht_ass_sd$sdom) # 11

unique(ht_ass_mil$type_eco4) # 10
unique(ht_ass_mil$milieu) # 11

# créer les 34 placettes
id1 <- c("FC1", "FE1", "FE2")
id2 <- c("FE3", "FE4", "FE5")
id3 <- c("FE6", "FO1", "ME1")
id4 <- c("MF1", "MJ1", "MJ2")
id5 <- c("MS2", "MS4", "MS6")
id6 <- c("MS7", "RP1", "RT1")
id7 <- c("RC3", "MS1")
id8 <- c("RE2", "RE3", "RB1", "RB2", "RB5")
id9 <- c("RE4", "RE7", "RE1")
id10 <- c("RS1", "RS2", "RS3")
id11 <- c("RS4", "RS5", "RS7")

id1 <- data.frame(veg_pot = id1, sdom_bio = "1", milieu = "0")
id2 <- data.frame(veg_pot = id2, sdom_bio = "2E", milieu = "1")
id3 <- data.frame(veg_pot = id3, sdom_bio = "2O", milieu = "2")
id4 <- data.frame(veg_pot = id4, sdom_bio = "3E", milieu = "3")
id5 <- data.frame(veg_pot = id5, sdom_bio = "3O", milieu = "4")
id6 <- data.frame(veg_pot = id6, sdom_bio = "4E", milieu = "5")
id7 <- data.frame(veg_pot = id7, sdom_bio = "4O", milieu = "6")
id8 <- data.frame(veg_pot = id8, sdom_bio = "5E", milieu = "7")
id9 <- data.frame(veg_pot = id9, sdom_bio = "5O", milieu = "8")
id10 <- data.frame(veg_pot = id10, sdom_bio = "6E", milieu = "9")
id11 <- data.frame(veg_pot = id11, sdom_bio = "6O", milieu = "0")

id <- bind_rows(id1, id2, id3, id4, id5, id6, id7, id8, id9, id10, id11) %>%
  mutate(
    id_pe = row_number(),
    p_tot = 1000,
    t_ma = 0.1,
    altitude = 200
  )

# créer la liste des 63 arbres
# liste de toutes les essences traitées
ess <- unique(ht_ass_ess$essence) # 63
ess <- data.frame(essence = ess, dhpcm = 10, nb_tige = 1) %>%
  mutate(no_arbre = row_number())

# mettre les 63 arbres dans chacune des 34 placettes: 2142 lignes
data_arbre <- as.data.frame(unclass(expand_grid(id, ess))) %>%
  dplyr::select(id_pe, veg_pot, milieu, sdom_bio, altitude, t_ma, p_tot, no_arbre, essence, nb_tige, dhpcm)

saveRDS(data_arbre, "tests/testthat/fixtures/data_arbre.rds")



# fichier de ht attendu en mode déterministe
# je devrais faire ce fichier dans SAS pour m'assurer que R donne la meme chose
parametre_ht_attendu <- param_ht(fic_arbres = data_arbre)
data_arbre_attendu <- relation_h_d(fic_arbres = data_arbre)
saveRDS(data_arbre_attendu, "tests/testthat/fixtures/data_arbre_attendu.rds")
saveRDS(parametre_ht_attendu, "tests/testthat/fixtures/parametre_ht_attendu.rds")

# fichier attendu pour le volume deterministe: une seule ligne par essence est suffisant
data_arbre_vol <- data_arbre_attendu %>%
  group_by(essence) %>%
  slice(1) %>%
  ungroup()
saveRDS(data_arbre_vol, "tests/testthat/fixtures/data_arbre_vol.rds")
param_vol_attendu <- param_vol(fic_arbres = data_arbre_vol)
saveRDS(param_vol_attendu, "tests/testthat/fixtures/param_vol_attendu.rds")
data_arbre_vol_attendu <- cubage(fic_arbres = data_arbre_vol, mode_simul = "DET")
saveRDS(data_arbre_vol_attendu, "tests/testthat/fixtures/data_arbre_vol_attendu.rds")


# fichier arbres pour tests stochastiques
data_arbre_a <- data_arbre %>%
  filter(id_pe == 1, essence %in% ht_liste_ess) %>%
  mutate(step = 1, no_arbre = row_number()) # ht_liste_ess au lieu de liste_ess
data_arbre_b <- data_arbre %>%
  filter(id_pe == 1, essence %in% ht_liste_ess) %>%
  mutate(step = 2, no_arbre = row_number())
data_arbre_c <- data_arbre %>%
  filter(id_pe == 1, essence %in% ht_liste_ess) %>%
  mutate(step = 3, no_arbre = row_number())
data_arbre_d <- data_arbre %>%
  filter(id_pe == 1, essence %in% ht_liste_ess) %>%
  mutate(step = 4, no_arbre = row_number())
data_arbre_e <- data_arbre %>%
  filter(id_pe == 1, essence %in% ht_liste_ess) %>%
  mutate(step = 5, no_arbre = row_number())
data_arbre0 <- bind_rows(data_arbre_a, data_arbre_b, data_arbre_c, data_arbre_d, data_arbre_e)

data_arbre2 <- do.call(rbind, replicate(200, data_arbre0, simplify = FALSE))
data_arbre3 <- data_arbre2 %>%
  group_by(id_pe, no_arbre, step) %>%
  mutate(iter = row_number()) %>%
  ungroup()

data_arbre_sto_2 <- data_arbre3
saveRDS(data_arbre_sto_2, "tests/testthat/fixtures/data_arbre_sto_2.rds")

# fichier de ht attendu en mode stochastique, en fixant le seed à 20
data_arbre_attendu_sto_2 <- relation_h_d(fic_arbres = data_arbre_sto_2, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed_value = 20)
saveRDS(data_arbre_attendu_sto_2, "tests/testthat/fixtures/data_arbre_attendu_sto_2.rds")


# fichier de ht attendu en mode stochastique avec une seule step, en fixant le seed à 20
data_arbre_attendu_sto1_2 <- relation_h_d(fic_arbres = data_arbre_sto_2[data_arbre_sto_2$step == 1, ], mode_simul = "STO", nb_iter = 200, nb_step = 1, seed_value = 20)
saveRDS(data_arbre_attendu_sto1_2, "tests/testthat/fixtures/data_arbre_attendu_sto1_2.rds")



# fichier attendu pour le volume en mode stochastique
data_arbre_vol_attendu_sto_2 <- cubage(fic_arbres = data_arbre_attendu_sto_2, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed_value = 20)
saveRDS(data_arbre_vol_attendu_sto_2, "tests/testthat/fixtures/data_arbre_vol_attendu_sto_2.rds")
saveRDS(data_arbre_vol_attendu_sto_2, "tests/testthat/fixtures/data_arbre_vol_attendu_sto_2a.rds") # fichier attendu avec la nouvelle version 2024-12-20 (quelques décimales de changées à cause du sto)


# fichier de samare avec des données qui ont souvant fait planté le code R
data_simul_samare <- read_delim("tests/testthat/fixtures/SimulHtVol.csv", delim = ";") %>% mutate(milieu = substr(milieu, 1, 1))
setDT(data_simul_samare)
liste_arbre2 <- unique(fic_arbres[, .(id_pe, no_arbre, essence)]) %>%
  mutate(fic = 1) %>%
  group_by(id_pe, no_arbre) %>%
  pivot_wider(names_from = essence, values_from = fic)
# il y a des arbres avec le même numéro d'arbres mais pas la même essence
# est-ce que c'est dû aux itérations dans samare? une recue avec le même numéro d'arbre mais pas la même essence générée?
saveRDS(data_simul_samare, "tests/testthat/fixtures/data_simul_samare.rds")
nb_iter <- max(data_simul_samare$iter)
nb_step <- max(data_simul_samare$step)
ht <- relation_h_d(fic_arbres = data_simul_samare, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, reg_eco = T, dt = 5, seed_value = 20)
data_simul_samare_attendu_2 <- cubage(fic_arbres = ht, mode_simul = "STO", nb_iter = nb_iter, nb_step = nb_step, seed_value = 20)
# il y a des ht et vol à NA seulement pour les morts, car n'ont pas de dhp
saveRDS(data_simul_samare_attendu_2, "tests/testthat/fixtures/data_simul_samare_attendu_2.rds")
saveRDS(data_simul_samare_attendu_2, "tests/testthat/fixtures/data_simul_samare_attendu_2a.rds") # fichier attendu avec la nouvelle version 2024-12-20 (quelques décimales de changées à cause du sto)



# ajouter test d'un fichier avec les essences qui ont une essence associée
# FEU FIN FRP MAS MEJ MEU NOC ORR ORT PED PEH PEU PID PIN PIS PRP RES SAL SOA SOD
ess_ass_ht <- ht_ass_ess %>%
  filter(essence != essence_hauteur) %>%
  select(essence)
ess_ass_vol <- tarif_ass_ess %>%
  filter(essence != essence_volume) %>%
  select(essence)
ess_ass <- bind_rows(ess_ass_ht, ess_ass_vol) %>%
  unique() %>%
  arrange(essence)
ess_ass2 <- ess_ass %>%
  mutate(
    id_pe = 1,
    no_arbre = row_number(),
    dhpcm = 10,
    nb_tige = 1,
    veg_pot = "MS2",
    sdom_bio = "4O",
    milieu = "2",
    p_tot = 1000,
    t_ma = 0.1,
    altitude = 200
  ) %>%
  filter(!essence %in% c("JUV", "MEO"))
saveRDS(ess_ass2, "tests/testthat/fixtures/data_ess_ass_det.rds")
ess_ass_attendu <- relation_h_d(fic_arbres = ess_ass2)
ess_ass_attendu <- cubage(fic_arbres = ess_ass_attendu)

# fichier pour test stochastique
ess_ass2 <- ess_ass2 %>% mutate(step = 1)
ess_ass_sto <- NULL
for (i in 1:50) {
  temp <- ess_ass2 %>% mutate(iter = i)
  ess_ass_sto <- bind_rows(ess_ass_sto, temp)
}
saveRDS(ess_ass_sto, "tests/testthat/fixtures/data_ess_ass_sto.rds")
ess_ass_attendu_sto <- relation_h_d(fic_arbres = ess_ass_sto)
ess_ass_attendu_sto <- cubage(fic_arbres = ess_ass_attendu_sto)



##############################################################
##############################################################
##############################################################

# Vérification si mode déterministe et stochastique produisent les memes ht et vol que Capsis

# pour Capsis
data_arbre2 <- data_arbre %>% mutate(type_eco = paste0(veg_pot, milieu), etat = "10", latitude = 48, longitude = -78, strate = 1, cl_drai = "21")
regeco_ass_sdom2 <- regeco_ass_sdom %>%
  group_by(sdom_bio) %>%
  slice(1)
data_arbre2 <- left_join(data_arbre2, regeco_ass_sdom2[, c("reg_eco", "sdom_bio")], by = "sdom_bio")
data_arbre2 <- data_arbre2 %>%
  filter(
    !(essence %in% c("AUT", "CHX", "EPX", "F_0", "F_1", "F0R", "FEN", "FEU", "FIN", "PEU", "PIN", "RES", "MEJ", "MEU", "CAR", "CEO", "ERG", "ERP", "MAS", "PRP", "SAL", "SOA", "SOD")),
    !(veg_pot %in% c("MS4", "MS7", "RE4", "RE7", "RS4", "RS7", "RB2"))
  ) %>%
  mutate(
    vigueur = ifelse(essence %in% c("EPB", "EPN", "EPO", "EPR", "MEL", "PIB", "PID", "PIG", "PIR", "PIS", "PRU", "SAB", "THO"), 5, 1),
    residuel = 0, sup_pe = 0.04
  )

write_delim(data_arbre2, file = "tests\\testthat\\fixtures\\data_arbre_test.csv", delim = ";")
# pour natura (mode déterministe)
data_arbre_etude <- data_arbre2 %>%
  group_by(id_pe) %>%
  slice(1) %>%
  dplyr::select(strate, id_pe, essence, dhpcm) %>%
  mutate(
    age = 50,
    hauteur = 120,
    etage = "D"
  )
write_delim(data_arbre_etude, file = "tests\\testthat\\fixtures\\data_arbre_etude_test.csv", delim = ";")

# fichier obtenu de Capsis-Natura2014
data_arbre_test_attendu <- read_delim(file = "tests\\testthat\\fixtures\\data_arbre_test_resCapsis.csv", delim = ";")
data_arbre_test_attendu <- data_arbre_test_attendu %>%
  dplyr::select(PlacetteID, Espece, Hautm, Vol_dm3) %>%
  rename(id_pe = PlacetteID, essence = Espece, Hautm_capsis = Hautm, Vol_dm3_capsis = Vol_dm3)

# fichier obtenu de Capsis-Artemis2014 en stochastique 1000 iter, 1 pas de simul
data_arbre_test_attendu_stoA <- read_delim(file = "E:\\MrnMicro\\Applic\\_Modeles-DRF\\Capsis-ModelesDRF-20240222\\data\\artemis2014\\resArt.csv", delim = ";")
data_arbre_test_attendu_stoA <- data_arbre_test_attendu_stoA %>%
  rename(id_pe = PlacetteID, essence = Espece, Hautm_capsis = Hautm, Vol_dm3_capsis = Vol_dm3, no_arbre = origTreeID) %>%
  arrange(IterMC, id_pe, no_arbre, essence, Annee) %>%
  filter(Etat == "vivant") %>%
  filter(!is.na(no_arbre))

# Passer le data_arbre2 dans les fct R en mode déterministe
data_arbre_test_obtenu <- relation_h_d(fic_arbres = data_arbre2)
data_arbre_test_obtenu <- cubage(fic_arbres = data_arbre_test_obtenu)

# comparaison déterministe R vs Capsis-Natura
data_arbre_test_compare <- inner_join(data_arbre_test_obtenu, data_arbre_test_attendu) %>%
  mutate(
    diff_ht = Hautm_capsis - hauteur_pred,
    diff_vol = Vol_dm3_capsis - vol_dm3
  )
summary(data_arbre_test_compare$diff_ht)
summary(data_arbre_test_compare$diff_vol)
# tous des différence à 0 pour la ht
verif <- data_arbre_test_compare %>% arrange(diff_vol)
# différence de -1.896 pour PIS, probablement pas la même association d'essence, pas important


# resultat de R stochastisque du fichier simulé dans capsis-artemis, 1000 iter et 1 pas de simul
# ne pas regarder les recrues car elles change de numero par itération
# ne pas regarder les morts
# passer le fichier de sortie de Artemis dans les fonctions R
data_arbre_var <- data_arbre2 %>%
  dplyr::select(id_pe, p_tot, t_ma, altitude, veg_pot, sdom_bio, milieu) %>%
  unique()
data_arbre_test_attendu_stoA <- data_arbre_test_attendu_stoA %>%
  mutate(
    step = (Annee - 2024) / 10 + 1, iter = IterMC + 1,
    dhpcm = DHPcm, nb_tige = Nombre,
    # essence = ifelse(essence=='???',GrEspece,essence),
    essence_original = essence
  ) %>%
  select(step, essence, essence_original, id_pe, no_arbre, iter, dhpcm, contains("haut"), contains("vol"), nb_tige)
data_arbre_test_attendu_stoA2 <- inner_join(data_arbre_var, data_arbre_test_attendu_stoA)

# les recrues ont toutes des numéros d'arbres différents par iteration, c'est pour ça que c'est long le tarifqc
# pour ce test, je vais enlever les recrues
data_arbre_test_obtenu_stoA <- relation_h_d(fic_arbres = data_arbre_test_attendu_stoA2, mode_simul = "STO", nb_iter = 1000, nb_step = 2, dt = 10, seed_value = 20)
data_arbre_test_obtenu_stoA <- cubage(fic_arbres = data_arbre_test_obtenu_stoA, mode_simul = "STO", nb_iter = 1000, nb_step = 2, seed_value = 20)

data_arbre_test_obtenu_stoA <- data_arbre_test_obtenu_stoA %>%
  arrange(step, essence, id_pe, no_arbre, iter) %>%
  select(step, essence, essence_original, id_pe, no_arbre, iter, dhpcm, contains("haut"), contains("vol")) %>%
  mutate(
    diff_ht = Hautm_capsis - hauteur_pred,
    diff_vol = Vol_dm3_capsis - vol_dm3
  )
# capsis retourne des ht et des vol pour les morts
# moyenne des itérations par arbre
data_arbre_test_obtenu_sto_moyA <- data_arbre_test_obtenu_stoA %>%
  group_by(id_pe, step, no_arbre, essence_original, essence) %>%
  summarise(
    n = n(),
    ht_moy_capsis = round(mean(Hautm_capsis), 1),
    ht_moy_r = round(mean(hauteur_pred), 1),
    vol_moy_capsis = round(mean(Vol_dm3_capsis), 1),
    vol_moy_r = round(mean(vol_dm3), 1)
  ) %>%
  mutate(
    diff_ht = ht_moy_capsis - ht_moy_r,
    diff_vol = vol_moy_capsis - vol_moy_r
  )
summary(data_arbre_test_obtenu_sto_moyA$diff_ht)
summary(data_arbre_test_obtenu_sto_moyA$ht_moy_capsis)
summary(data_arbre_test_obtenu_sto_moyA$diff_vol)
summary(data_arbre_test_obtenu_sto_moyA$vol_moy_capsis)
# essence original de la relation h-d
verif_hd <- data_arbre_test_obtenu_sto_moyA %>%
  filter(essence %in% c("BOJ", "BOG", "BOP", "CET", "CHR", "EPB", "EPN", "EPR", "ERA", "ERS", "ERR", "ORA", "PET", "FRA", "FRN", "HEG", "MEL", "OSV", "PEB", "PEG", "PIB", "PIG", "PIR", "PRU", "SAB", "THO", "TIL"))
summary(verif_hd$diff_ht) # différence entre -0.2 et 0.2 m ok
# essence original du tarif de cubage
verif_vol <- data_arbre_test_obtenu_sto_moyA %>%
  filter(essence %in% c("BOJ", "BOG", "BOP", "ORA", "CET", "CHR", "EPB", "EPN", "EPR", "ERS", "ERR", "PET", "FRA", "FRN", "HEG", "THO", "MEL", "OSV", "PEB", "PEG", "PIB", "PIR", "PIG", "PRU", "SAB", "TIL"))
summary(verif_vol$diff_vol) # différence entre -0.3 et 0.7 dm3

# donc pas de problème avec les 2 équations en mode stochastique



#####################################################################
# créer un gros fichier d'arbres stochastique pour vérifier la vitesse d'exécution

# fichier arbres pour tests stochastiques
# générer 2000 placettes avec 5 steps
data_tous <- NULL
for (i in 1:2000) {
  data_arbre_a <- data_arbre %>%
    filter(id_pe == 1, essence %in% ht_liste_ess) %>%
    mutate(step = 1, no_arbre = row_number()) # ht_liste_ess au lieu de liste_ess
  data_arbre_b <- data_arbre %>%
    filter(id_pe == 1, essence %in% ht_liste_ess) %>%
    mutate(step = 2, no_arbre = row_number())
  data_arbre_c <- data_arbre %>%
    filter(id_pe == 1, essence %in% ht_liste_ess) %>%
    mutate(step = 3, no_arbre = row_number())
  data_arbre_d <- data_arbre %>%
    filter(id_pe == 1, essence %in% ht_liste_ess) %>%
    mutate(step = 4, no_arbre = row_number())
  data_arbre_e <- data_arbre %>%
    filter(id_pe == 1, essence %in% ht_liste_ess) %>%
    mutate(step = 5, no_arbre = row_number())
  data_arbre0 <- bind_rows(data_arbre_a, data_arbre_b, data_arbre_c, data_arbre_d, data_arbre_e) %>% mutate(id_pe = i)
  data_tous <- bind_rows(data_tous, data_arbre0)
}
# 270 000 lignes

# générer 200 iter
data_tous2 <- do.call(rbind, replicate(200, data_tous, simplify = FALSE)) # 54 000 000
data_tous3 <- data_tous2 %>%
  group_by(id_pe, no_arbre, step) %>%
  mutate(iter = row_number()) %>%
  ungroup()

data_arbre_sto_gros <- data_tous3
saveRDS(data_arbre_sto_gros, "tests/testthat/fixtures/data_arbre_sto_gros.rds")

# fichier des paramètres ht et vol pour le gros fichier
param_sto_ht_gros <- param_ht(fic_arbres = data_tous3, mode_simul = "STO", nb_iter = 200, nb_step = 5, seed_value = 20)
param_sto_vol_gros <- param_vol(fic_arbres = data_arbre3, mode_simul = "STO", nb_iter = 200, nb_step = 5)

saveRDS(param_sto_ht_gros, "tests/testthat/fixtures/param_sto_ht_gros.rds")
saveRDS(param_sto_vol_gros, "tests/testthat/fixtures/param_sto_vol_gros.rds")



#####################################################################
# créer un fichier test pour équation de coupe en mode stochastique
data_tree <- data.frame(essence= c("BOJ", "BOP", "EPN", "ERR", "PET", "PIB", "PIG", "SAB", "THO", "CHR", "ERS", "HEG"),#
                        id_pe = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
                        no_arbre = seq(1, 12, 1),
                        dhpcm = c(12, 12, 12, 12, 24, 24, 24, 24, 24, 12, 12, 24),
                        nbTi_ha = c(225, 225, 225, 225, 225, 225, 225, 225, 225, 75, 75, 75),
                        st_ha = c(6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132, 6.785840132,
                                  6.785840132, 6.785840132, 1.696460033, 1.696460033, 1.696460033),
                        stringsAsFactors = FALSE)
set.seed(NULL)
resultat <- prob_coupe(data_tree=data_tree, trt_coupe=0, mode_simul="STO", seed_value=3)
saveRDS(data_tree, test_path("fixtures", "data_tree_coupe.rds"))
saveRDS(resultat, test_path("fixtures", "resultat_coupe_STO.rds"))
