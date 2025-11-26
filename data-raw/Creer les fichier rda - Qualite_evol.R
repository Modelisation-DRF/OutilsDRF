# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw


#### Équations d'évolution de la qualité ####


########################################################

# Lecture des fichiers csv nécessaires pour appliquer les équations d'attribution de qualité

# un dossier par essence, 8 essences

chemin <- "data-raw/Parametre_qualite_evol/"
liste_ess <- c("boj", "bop", "chx", "err", "ers", "fen", "heg", "peu")

# Fichier des paramètres bi des équations
liste_param <- list()
liste_cov <- list()
liste_sd <- list()
for (i in 1:length(liste_ess)) {
  # nom des fichiers
  fic <- paste0("parameters", toupper(liste_ess[i]), ".csv")
  covar <- paste0("omega", toupper(liste_ess[i]), ".csv")
  sd <- paste0("listeSdom", toupper(liste_ess[i]), ".csv")

  # lecture fichier des bi
  param <- read_delim(paste0(chemin, liste_ess[i], "/", fic), delim = ";")
  param <- param %>% mutate(
    Effect = tolower(Effect),
    essence = toupper(liste_ess[i])
  )
  liste_param[[i]] <- param

  # lecture du fichier des associations des sdom
  sdom <- read_delim(paste0(chemin, liste_ess[i], "/", sd), delim = ";")
  names(sdom) <- tolower(names(sdom))
  liste_sd[[i]] <- sdom %>% mutate(essence = toupper(liste_ess[i]))

  # lecture du fichier de la matrice covb
  liste_cov[[i]] <- read_delim(paste0(chemin, liste_ess[i], "/", covar), delim = ";")
}
liste_param <- bind_rows(liste_param)
liste_param <- liste_param %>%
  mutate(
    # renommer les intercept pour plus de clareté
    ClasseIntercept = ifelse(Equation == 1 & ClasseIntercept == "A", "C",
      ifelse(Equation == 2 & ClasseIntercept == "A", "B",
        ifelse(Equation == 2 & ClasseIntercept == "B", "C",
          ClasseIntercept
        )
      )
    ),
    # concaténer la variable et son niveau
    var_i = ifelse(Effect == "sdom", paste(Effect, Sdom, sep = "_"),
      ifelse(Effect == "qualite", paste(Effect, Qualite, sep = "_"),
        ifelse(Effect == "intercept", paste(Effect, ClasseIntercept, sep = "_"),
          Effect
        )
      )
    )
  ) %>%
  select(-vp, -Drainage) # jamais utilisé


# il faut séparer les matrices des 3 équations car pas le meme nombre de colonnes dans chacune et les derniere non utilisées sont a 0
# le nombre de colonnes a garder est le nombre de lignes
# faire une liste de liste, soit une liste par essence, et pour chaque essence, une liste avec chacune des matrices des 3 equations
liste_covb <- list()
liste_eq <- list()
for (i in 1:length(liste_ess)) {
  for (j in 1:3) {
    cov_i <- liste_cov[[i]] %>% filter(Equation == j)
    cov_i <- cov_i[, 9:(9 + nrow(cov_i) - 1)]
    liste_eq[[j]] <- cov_i
  }
  liste_covb[[i]] <- liste_eq
}

# créer un seul dataframe à partir de la liste liste_sd
liste_sd2 <- do.call(rbind, liste_sd) %>% rename(Equation = equation)


####################################################################

# fichier pour faire des tests
# essences traitées et non traitées
# dhp < 23, 23-33, 33-39, et 39+
# quelques sous-domaines, donc plusieurs placettes
# toutes les qualite de départ

plot <- data.frame(id_pe = c(1, 2, 3), sdom = c("2EST", "3OUEST", "4EST"), tmoy = c(2.1, 0.1, 1.2), ptot = c(828, 700, 1022), sum_st_ha = c(29, 22, 25), coupe = c(0, 0, 1))

arbre1 <- data.frame(id_pe = rep(1, 3), no_arbre = seq(1, 3, 1), dhpcm = c(24.0, 33.5, 40.2), essence = c("ERS", "BOJ", "CHX"), qualite = c("C", "B", "A"), st_ha_cumul_gt = c(23, 15, 2))
arbre2 <- data.frame(id_pe = rep(2, 4), no_arbre = seq(1, 4, 1), dhpcm = c(16.1, 26.0, 35.5, 41.3), essence = c("BOP", "PEU", "ERR", "EPN"), qualite = c(NA, "D", "B", NA), st_ha_cumul_gt = c(20, 15, 8, 1))
arbre3 <- data.frame(id_pe = rep(3, 4), no_arbre = seq(1, 4, 1), dhpcm = c(26.1, 27.0, 38.5, 39.2), essence = c("FEN", "HEG", "ERR", "ERS"), qualite = c("C", "D", "B", "A"), st_ha_cumul_gt = c(20, 19, 5, 4))

arbres <- bind_rows(arbre1, arbre2, arbre3)

ex_qualite_evol <- left_join(plot, arbres)

ex_qualite_evol$ddhpcm <- 0.12
ex_qualite_evol <- ex_qualite_evol %>%
  rename(dhpcm1 = dhpcm) %>%
  mutate(dhpcm = dhpcm1 - ddhpcm * 10) %>%
  select(-ddhpcm)


#write_delim(ex_qualite_evol, "P:\\F1272\\CPF\\_Simulateurs\\QualiteArbres\\EvolutionQualite\\ex_qualite_evol.csv", delim = ";") # pour faire les calcul à la main
write_delim(ex_qualite_evol, test_path("fixtures/evol_qualite", "ex_qualite_evol.csv"), delim = ";")
usethis::use_data(ex_qualite_evol,
  internal = FALSE, overwrite = TRUE
)

####################################################################





# renommer les objets
qualite_evol_param_covb <- liste_covb
qualite_evol_param <- liste_param %>%
  select(essence, Equation, var_i, Estimate) %>%
  rename(b_i = Estimate) # %>% mutate(var_i = gsub("\\*", "_x_", var_i))
qualite_evol_ass_sdom <- liste_sd2


write_delim(qualite_evol_param, test_path("fixtures/evol_qualite", "qualite_evol_param"), delim = ";") # pour faire les calcul à la main


############################################

# ajouter les nouveaux fichiers au sysdata existant
# Créer un environnement temporaire
temp_env <- new.env()
# Charger le fichier sysdata.rda dans cet environnement
load("R/sysdata.rda", envir = temp_env)
# Vérifier les objets actuellement dans sysdata.rda
ls(envir = temp_env)
# Ajouter les nouveaux objets à l'environnement temporaire

temp_env$qualite_evol_param_covb <- qualite_evol_param_covb
temp_env$qualite_evol_param <- qualite_evol_param
temp_env$qualite_evol_ass_sdom <- qualite_evol_ass_sdom

# Sauvegarder tous les objets présents dans l'environnement temporaire
save(list = ls(envir = temp_env), file = "R/sysdata.rda", envir = temp_env)

rm(temp_env)


# load("R/sysdata.rda")
# ls()

