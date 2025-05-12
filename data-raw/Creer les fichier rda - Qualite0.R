# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw


#### Équations d'attribution de qualité ####


########################################################

# Lecture des fichiers csv nécessaires pour appliquer les équations d'attribution de qualité

# un dossier par essence, 8 essences

chemin <- "data-raw/Parametre_qualite0/"
liste_ess <- c("boj", "bop", "chx", "err", "ers", "fen", "heg", "peu")

# Fichier des paramètres bi des équations
liste_param <- list()
liste_cov <- list()
liste_sd <- list()
for (i in 1:length(liste_ess)) {
  # nom des fichiers
  fic <- paste0("PredParameters", toupper(liste_ess[i]), ".csv")
  covar <- paste0("PredOmega", toupper(liste_ess[i]), ".csv")
  sd <- paste0("listeSdom", toupper(liste_ess[i]), ".csv")

  # lecture fichier des bi
  param <- read_delim(paste0(chemin, liste_ess[i], "/", fic), delim = ";")
  param <- param %>% mutate(Variable = stringr::str_replace(Variable, pattern = fixed("*"), replacement = "_x_"))
  param <- param %>% mutate(
    Variable = tolower(Variable),
    Variable = ifelse(Variable == "prio_recol", "priorecol", Variable), # mettre le même nom de variable partout
    essence = toupper(liste_ess[i])
  )
  liste_param[[i]] <- param

  # lecture du fuchier des associations des sdom
  sdom <- read_delim(paste0(chemin, liste_ess[i], "/", sd), delim = ";")
  names(sdom) <- tolower(names(sdom))
  liste_sd[[i]] <- sdom

  # lecture du fichier de la matrice covb
  liste_cov[[i]] <- read_delim(paste0(chemin, liste_ess[i], "/", covar), delim = ";")
}

liste_param <- bind_rows(liste_param)
liste_param <- liste_param %>%
  mutate(
    # renommber les intercept pour plus de clareté
    Intercept = ifelse(Equation == 1 & Intercept == "A", "C",
      ifelse(Equation == 2 & Intercept == "A", "B",
        ifelse(Equation == 2 & Intercept == "B", "C",
          Intercept
        )
      )
    ),
    # concaténer la variable et son niveau
    var_i = ifelse(Variable %in% c("intercept", "dhpcm_x_intercept"), paste(Variable, Intercept, sep = "_"),
      ifelse(Variable %in% c("priorecol", "dhpcm_x_priorecol", "sum_st_ha_x_priorecol"), paste(Variable, PrioRecolte, sep = "_"),
        ifelse(Variable == "sdom", paste(Variable, Sdom, sep = "_"),
          Variable
        )
      )
    )
  ) %>%
  select(-vp) # jamais utilisé


# il faut séparer les matrices des 3 équations car pas le meme nombre de colonnes dans chacune et les derniere non utilisées sont a 0
# le nombre de colonnes a garder est le nombre de lignes
# faire une liste de liste, soit une liste par essence, et pour chaque essence, une liste avec chacune des matrices des 3 equations
liste_covb <- list()
liste_eq <- list()
for (i in 1:length(liste_ess)) {
  for (j in 1:3) {
    cov_i <- liste_cov[[i]] %>% filter(Equation == j)
    cov_i <- cov_i[, 8:(8 + nrow(cov_i) - 1)]
    liste_eq[[j]] <- cov_i
  }
  liste_covb[[i]] <- liste_eq
}

# regarder si les associations de sdom change vraiment par equation
# liste_sd1 = liste_sd[[3]]
# verif = liste_sd1 %>%
#   group_by(Sdom_Bio, Sdom) %>%
#   summarise(nb = n())
# oui ca change par equation...

# transposer séparément par équation
# eq1 <- liste_param %>% filter(Equation==1) %>% select(essence, Estimate, effet) %>% group_by(essence) %>%  pivot_wider(names_from = 'effet', values_from = 'Estimate')
# eq2 <- liste_param %>% filter(Equation==2) %>% select(essence, Estimate, effet) %>% group_by(essence) %>%  pivot_wider(names_from = 'effet', values_from = 'Estimate')
# eq3 <- liste_param %>% filter(Equation==3) %>% select(essence, Estimate, effet) %>% group_by(essence) %>%  pivot_wider(names_from = 'effet', values_from = 'Estimate')
#
# [1] "essence"           "intercept_A"       "priorecol_C"       "priorecol_R"       "priorecol_S"       "dhpcm"
# [7] "sdom_1"            "sdom_2EST"         "sdom_2OUEST"       "sdom_3EST"         "sdom_3OUEST"       "sdom_4EST"
# [13] "sdom_4OUEST"       "sdom_5EST"         "sum_st_ha"         "sdom_5OUEST"       "sdom_6EST"         "tmoy"
# [19] "dhpcm_x_priorecol_C" "dhpcm_x_priorecol_R" "dhpcm_x_priorecol_S" "coupe"             "dhpcm_x_sum_st_ha"
# # eq1 une simple régression logistique
#
#
# [1] "essence"               "intercept_A"           "intercept_B"           "priorecol_C"           "priorecol_R"
# [6] "priorecol_S"           "dhpcm"                 "sdom_2EST"             "sdom_2OUEST"           "sdom_3EST"
# [11] "sdom_3OUEST"           "sdom_4EST"             "sdom_4OUEST"           "sdom_5EST"             "sum_st_ha"
# [16] "tmoy"                  "sum_st_ha_x_priorecol_C" "sum_st_ha_x_priorecol_R" "sum_st_ha_x_priorecol_S" "sdom_1"
# [21] "coupe"
# # eq 2: multinomiale à 3 niveaux, donc 2 équations, seuls les intercepts changent, les paramètres des effets fixes sont les mêmes
#
# [1] "essence"               "intercept_A"           "intercept_B"           "intercept_C"           "priorecol_C"
# [6] "priorecol_R"           "priorecol_S"           "sdom_2EST"             "sdom_2OUEST"           "sdom_3EST"
# [11] "sdom_3OUEST"           "sdom_4EST"             "sdom_4OUEST"           "sdom_5EST"             "sum_st_ha"
# [16] "ptot"                  "sum_st_ha_x_priorecol_C" "sum_st_ha_x_priorecol_R" "sum_st_ha_x_priorecol_S" "dhpcm"
# [21] "dhpcm_x_priorecol_C"     "dhpcm_x_priorecol_R"     "dhpcm_x_priorecol_S"     "dhpcm_x_intercept_A"     "dhpcm_x_intercept_B"
# [26] "dhpcm_x_intercept_C"     "tmoy"                  "sdom_1"
# # eq 3: multinomiale à 4 niveaux, donc 3 équations, les intercepts changent, les paramètres des effets fixes sont les mêmes, sauf pour dhpcm

####################################################################

# fichier pour faire des tests
# essences traitées et non traitées
# dhp < 23, 23-33, 33-39, et 39+
# quelques sous-domaines, donc plusieurs placettes
# tous les M-S-C-R

ex_qualite <- read_delim(paste0(chemin, "ex_qualite.csv"), delim = ";")

write_delim(ex_qualite, "P:\\F1272\\CPF\\_Simulateurs\\QualiteArbres\\AttributionQualite\\ex_qualite.csv", delim=';') # pour faire les calcul à la main
usethis::use_data(ex_qualite,
  internal = FALSE, overwrite = TRUE
)

####################################################################


# renommer les objets
qualite0_param_covb <- liste_covb
qualite0_param <- liste_param %>%
  select(essence, Equation, var_i, Estimate) %>%
  rename(b_i = Estimate)
qualite0_ass_sdom <- liste_sd
qualite0_ess <- toupper(c("boj", "bop", "chx", "err", "ers", "fen", "heg", "peu"))

write_delim(qualite0_param, "P:\\F1272\\CPF\\_Simulateurs\\QualiteArbres\\AttributionQualite\\qualite0_param.csv", delim=';') # pour faire les calcul à la main
# fichiers généras dans param_qualite0()
#write_delim(bi_eq1, "P:\\F1272\\CPF\\_Simulateurs\\QualiteArbres\\AttributionQualite\\bi_eq1.csv", delim=';') # pour faire les calcul à la main
#write_delim(bi_eq2, "P:\\F1272\\CPF\\_Simulateurs\\QualiteArbres\\AttributionQualite\\bi_eq2.csv", delim=';') # pour faire les calcul à la main
#write_delim(bi_eq3, "P:\\F1272\\CPF\\_Simulateurs\\QualiteArbres\\AttributionQualite\\bi_eq3.csv", delim=';') # pour faire les calcul à la main

############################################

# ajouter les nouveaux fichiers au sysdata existant
# Créer un environnement temporaire
temp_env <- new.env()
# Charger le fichier sysdata.rda dans cet environnement
load("R/sysdata.rda", envir = temp_env)
# Vérifier les objets actuellement dans sysdata.rda
ls(envir = temp_env)
# Ajouter les nouveaux objets à l'environnement temporaire

temp_env$qualite0_param_covb <- qualite0_param_covb
temp_env$qualite0_param <- qualite0_param
temp_env$qualite0_ass_sdom <- qualite0_ass_sdom
temp_env$qualite0_ess <- qualite0_ess

# Sauvegarder tous les objets présents dans l'environnement temporaire
save(list = ls(envir = temp_env), file = "R/sysdata.rda", envir = temp_env)

rm(temp_env)


# load("R/sysdata.rda")
# ls()

