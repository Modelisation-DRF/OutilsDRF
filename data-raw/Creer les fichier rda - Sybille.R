# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw

# library(xlsx)


#### Fichiers interne pour Sybille ####

defil_liste_ess <- c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIB", "PIG", "SAB", "THO")

# objets contenant les équation du modèle complet
load("data-raw\\Parametre_eq_defil\\Modele_complet\\standAndTree.bop")
load("data-raw\\Parametre_eq_defil\\Modele_complet\\standAndTree.epb")
load("data-raw\\Parametre_eq_defil\\Modele_complet\\standAndTree.epn")
load("data-raw\\Parametre_eq_defil\\Modele_complet\\standAndTree.epr")
load("data-raw\\Parametre_eq_defil\\Modele_complet\\standAndTree.peg")
load("data-raw\\Parametre_eq_defil\\Modele_complet\\standAndTree.pet")
load("data-raw\\Parametre_eq_defil\\Modele_complet\\standAndTree.pig")
load("data-raw\\Parametre_eq_defil\\Modele_complet\\standAndTree.sab")
load("data-raw\\Parametre_eq_defil\\Modele_complet\\standAndTree.tho")
# objets contenant les équation du modèle arbre
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.bop")
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.epb")
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.epn")
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.epr")
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.peg")
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.pet")
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.pib")
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.pig")
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.sab")
load("data-raw\\Parametre_eq_defil\\Modele_arbre\\treeOnly.tho")
# 19

defil_standAndTree.bop <- standAndTree.bop
defil_standAndTree.epb <- standAndTree.epb
defil_standAndTree.epn <- standAndTree.epn
defil_standAndTree.epr <- standAndTree.epr
defil_standAndTree.peg <- standAndTree.peg
defil_standAndTree.pet <- standAndTree.pet
defil_standAndTree.pig <- standAndTree.pig
defil_standAndTree.sab <- standAndTree.sab
defil_standAndTree.tho <- standAndTree.tho

defil_treeOnly.bop <- treeOnly.bop
defil_treeOnly.epb <- treeOnly.epb
defil_treeOnly.epn <- treeOnly.epn
defil_treeOnly.epr <- treeOnly.epr
defil_treeOnly.peg <- treeOnly.peg
defil_treeOnly.pet <- treeOnly.pet
defil_treeOnly.pib <- treeOnly.pib
defil_treeOnly.pig <- treeOnly.pig
defil_treeOnly.sab <- treeOnly.sab
defil_treeOnly.tho <- treeOnly.tho


# fichiers des groupes de variables
defil_group_vp <- read.xlsx(file = "data-raw\\Parametre_eq_defil\\groupement_variables.xlsx", sheetName = "gr_veg_pot")
defil_group_dr <- read.xlsx(file = "data-raw\\Parametre_eq_defil\\groupement_variables.xlsx", sheetName = "gr_drainage")
defil_group_sd <- read.xlsx(file = "data-raw\\Parametre_eq_defil\\groupement_variables.xlsx", sheetName = "gr_sdom")
# 3
# chacune des catégories de la variable végétation potentielle est dans la colonne veg_pot
# chacune des catégories de la variable classe de drainage est dans la colonne cl_drai
# chacune des catégories de la variable sous-domaine bioclimatique est dans la colonne sdom_bio
# les groupes de catégories pour chaque essence sont dans la colonne de l'essence
# si la colonne d'une essence est complètement à NA, alors la variable n'est pas dans le modèle de cette essence


# fichiers des effets aléatoires et des paramètres

# fichier des effets alétoires du modèle complet
# liste des essences avec un modèle complet
liste_ess_complet <- c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIG", "SAB", "THO")
for (ess in liste_ess_complet) {
  # ess <- 'bop'
  random_arbre <- read_delim(paste0("data-raw/Parametre_eq_defil/Modele_complet/", ess, "/treeRandomEffects", ess, ".csv"), delim = ";")
  random_plot <- read_delim(paste0("data-raw/Parametre_eq_defil/Modele_complet/", ess, "/plotRandomEffects", ess, ".csv"), delim = ";")
  alpha <- read_delim(paste0("data-raw/Parametre_eq_defil/Modele_complet/", ess, "/parameters", ess, ".csv"), delim = ";") %>% slice(1)
  nom1 <- paste0("defil_random_arbre_complet_", ess)
  nom2 <- paste0("defil_random_plot_complet_", ess)
  assign(nom1, random_arbre)
  assign(nom2, random_plot)
  nom3 <- paste0("defil_alpha_complet_", ess)
  assign(nom3, alpha)
}

# fichier des effets alétoires du modèle arbre
# liste des essences avec un modèle arbre
liste_ess_arbre <- c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIB", "PIG", "SAB", "THO")
for (ess in liste_ess_arbre) {
  # ess <- 'bop'
  random_arbre <- read_delim(paste0("data-raw/Parametre_eq_defil/Modele_arbre/", ess, "/treeRandomEffects", ess, ".csv"), delim = ";")
  random_plot <- read_delim(paste0("data-raw/Parametre_eq_defil/Modele_arbre/", ess, "/plotRandomEffects", ess, ".csv"), delim = ";")
  alpha <- read_delim(paste0("data-raw/Parametre_eq_defil/Modele_arbre/", ess, "/parameters", ess, ".csv"), delim = ";") %>%
    slice(1) %>%
    select(x)
  nom1 <- paste0("defil_random_arbre_arbre_", ess)
  nom2 <- paste0("defil_random_plot_arbre_", ess)
  assign(nom1, random_arbre)
  assign(nom2, random_plot)
  nom3 <- paste0("defil_alpha_arbre_", ess)
  assign(nom3, alpha)
}

# nouveaux objets à mettre dans le fichier sysdata.rda
# obj_list <- list(
#   defil_random_arbre_arbre_BOP,
#   defil_random_arbre_arbre_EPB,
#   defil_random_arbre_arbre_EPN,
#   defil_random_arbre_arbre_EPR,
#   defil_random_arbre_arbre_PEG,
#   defil_random_arbre_arbre_PET,
#   defil_random_arbre_arbre_PIB,
#   defil_random_arbre_arbre_PIG,
#   defil_random_arbre_arbre_SAB,
#   defil_random_arbre_arbre_THO,
#
#   defil_random_plot_arbre_BOP,
#   defil_random_plot_arbre_EPB,
#   defil_random_plot_arbre_EPN,
#   defil_random_plot_arbre_EPR,
#   defil_random_plot_arbre_PEG,
#   defil_random_plot_arbre_PET,
#   defil_random_plot_arbre_PIB,
#   defil_random_plot_arbre_PIG,
#   defil_random_plot_arbre_SAB,
#   defil_random_plot_arbre_THO,
#
#   defil_alpha_arbre_BOP,
#   defil_alpha_arbre_EPB,
#   defil_alpha_arbre_EPN,
#   defil_alpha_arbre_EPR,
#   defil_alpha_arbre_PEG,
#   defil_alpha_arbre_PET,
#   defil_alpha_arbre_PIB,
#   defil_alpha_arbre_PIG,
#   defil_alpha_arbre_SAB,
#   defil_alpha_arbre_THO,
#
#   defil_random_arbre_complet_BOP,
#   defil_random_arbre_complet_EPB,
#   defil_random_arbre_complet_EPN,
#   defil_random_arbre_complet_EPR,
#   defil_random_arbre_complet_PEG,
#   defil_random_arbre_complet_PET,
#   defil_random_arbre_complet_PIG,
#   defil_random_arbre_complet_SAB,
#   defil_random_arbre_complet_THO,
#
#   defil_random_plot_complet_BOP,
#   defil_random_plot_complet_EPB,
#   defil_random_plot_complet_EPN,
#   defil_random_plot_complet_EPR,
#   defil_random_plot_complet_PEG,
#   defil_random_plot_complet_PET,
#   defil_random_plot_complet_PIG,
#   defil_random_plot_complet_SAB,
#   defil_random_plot_complet_THO,
#
#   defil_alpha_complet_BOP,
#   defil_alpha_complet_EPB,
#   defil_alpha_complet_EPN,
#   defil_alpha_complet_EPR,
#   defil_alpha_complet_PEG,
#   defil_alpha_complet_PET,
#   defil_alpha_complet_PIG,
#   defil_alpha_complet_SAB,
#   defil_alpha_complet_THO,
#
#   defil_group_vp,
#   defil_group_dr,
#   defil_group_sd,
#
#   defil_standAndTree.bop,
#   defil_standAndTree.epb,
#   defil_standAndTree.epn,
#   defil_standAndTree.epr,
#   defil_standAndTree.peg,
#   defil_standAndTree.pet,
#   defil_standAndTree.pig,
#   defil_standAndTree.sab,
#   defil_standAndTree.tho,
#
#   defil_treeOnly.bop,
#   defil_treeOnly.epb,
#   defil_treeOnly.epn,
#   defil_treeOnly.epr,
#   defil_treeOnly.peg,
#   defil_treeOnly.pet,
#   defil_treeOnly.pib,
#   defil_treeOnly.pig,
#   defil_treeOnly.sab,
#   defil_treeOnly.tho
# )


obj_name <- c(
  "defil_random_arbre_arbre_BOP",
  "defil_random_arbre_arbre_EPB",
  "defil_random_arbre_arbre_EPN",
  "defil_random_arbre_arbre_EPR",
  "defil_random_arbre_arbre_PEG",
  "defil_random_arbre_arbre_PET",
  "defil_random_arbre_arbre_PIB",
  "defil_random_arbre_arbre_PIG",
  "defil_random_arbre_arbre_SAB",
  "defil_random_arbre_arbre_THO",
  "defil_random_plot_arbre_BOP",
  "defil_random_plot_arbre_EPB",
  "defil_random_plot_arbre_EPN",
  "defil_random_plot_arbre_EPR",
  "defil_random_plot_arbre_PEG",
  "defil_random_plot_arbre_PET",
  "defil_random_plot_arbre_PIB",
  "defil_random_plot_arbre_PIG",
  "defil_random_plot_arbre_SAB",
  "defil_random_plot_arbre_THO",
  "defil_alpha_arbre_BOP",
  "defil_alpha_arbre_EPB",
  "defil_alpha_arbre_EPN",
  "defil_alpha_arbre_EPR",
  "defil_alpha_arbre_PEG",
  "defil_alpha_arbre_PET",
  "defil_alpha_arbre_PIB",
  "defil_alpha_arbre_PIG",
  "defil_alpha_arbre_SAB",
  "defil_alpha_arbre_THO",
  "defil_random_arbre_complet_BOP",
  "defil_random_arbre_complet_EPB",
  "defil_random_arbre_complet_EPN",
  "defil_random_arbre_complet_EPR",
  "defil_random_arbre_complet_PEG",
  "defil_random_arbre_complet_PET",
  "defil_random_arbre_complet_PIG",
  "defil_random_arbre_complet_SAB",
  "defil_random_arbre_complet_THO",
  "defil_random_plot_complet_BOP",
  "defil_random_plot_complet_EPB",
  "defil_random_plot_complet_EPN",
  "defil_random_plot_complet_EPR",
  "defil_random_plot_complet_PEG",
  "defil_random_plot_complet_PET",
  "defil_random_plot_complet_PIG",
  "defil_random_plot_complet_SAB",
  "defil_random_plot_complet_THO",
  "defil_alpha_complet_BOP",
  "defil_alpha_complet_EPB",
  "defil_alpha_complet_EPN",
  "defil_alpha_complet_EPR",
  "defil_alpha_complet_PEG",
  "defil_alpha_complet_PET",
  "defil_alpha_complet_PIG",
  "defil_alpha_complet_SAB",
  "defil_alpha_complet_THO",
  "defil_group_vp",
  "defil_group_dr",
  "defil_group_sd",
  "defil_standAndTree.bop",
  "defil_standAndTree.epb",
  "defil_standAndTree.epn",
  "defil_standAndTree.epr",
  "defil_standAndTree.peg",
  "defil_standAndTree.pet",
  "defil_standAndTree.pig",
  "defil_standAndTree.sab",
  "defil_standAndTree.tho",
  "defil_treeOnly.bop",
  "defil_treeOnly.epb",
  "defil_treeOnly.epn",
  "defil_treeOnly.epr",
  "defil_treeOnly.peg",
  "defil_treeOnly.pet",
  "defil_treeOnly.pib",
  "defil_treeOnly.pig",
  "defil_treeOnly.sab",
  "defil_treeOnly.tho"
)


##################################################

# Code pour ajouter les nouveaux objets à ceux déjà dans le fichier sysdata.rda

# Créer un environnement temporaire
temp_env <- new.env()
# Charger le fichier sysdata.rda dans cet environnement
load("R/sysdata.rda", envir = temp_env)
# Vérifier les objets actuellement dans sysdata.rda
ls(envir = temp_env)

# Ajouter les nouveaux objets à l'environnement temporaire
# list2env(obj_list, "R/sysdata.rda", envir = temp_env)

# Ajouter les objets dynamiquement dans l'environnement
for (name in obj_name) {
  assign(name, get(name), envir = temp_env)
}

ls(temp_env)

# Sauvegarder tous les objets présents dans l'environnement temporaire dans le fichier sysdata.rda
save(list = ls(envir = temp_env), file = "R/sysdata.rda", envir = temp_env)

# supprimer l'environnement temporaire
# rm(temp_env)


##################################################
##################################################


# construction des fichiers pour essayer la fonction get_diam()

# fichier avec les 10 essences
data_diam1 <- data.frame(
  essence = c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIB", "PIG", "SAB", "THO", "ERS"),
  sdom_bio = c("2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5EST", "5OUEST", "6EST", "6OUEST", "1"),
  cl_drai = c("2", "4", "1", "3", "5", "6", "0", "1", "2", "3", "4"),
  veg_pot = c("MS2", "RE3", "MJ2", "FE2", "RE2", "RE1", "MJ1", "RB2", "MS1", "RS2", "FE2"),
  DHP_Ae = rep(150, 11),
  HT_REELLE_M = rep(10, 11),
  HAUTEUR_M = rep(5, 11),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200
)



# plusieurs placettes, tous des arbres de la même essence, mais certaines placette pourront utiliser le modele complet, d'autre non
# BOP utilise seulement le sdom, mais n'a pas de groupe pour 1 et 2OUEST
data_diam2 <- data.frame(
  essence = rep("BOP", 11),
  id_pe = seq(1, 11, 1),
  no_arbre = rep(1, 11),
  sdom_bio = c("1", "2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5EST", "5OUEST", "6EST", "6OUEST"),
  cl_drai = rep("2", 11),
  veg_pot = rep("MS2", 11),
  DHP_Ae = rep(150, 11),
  HT_REELLE_M = rep(10, 11),
  HAUTEUR_M = rep(5, 11),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200
)

data_diam3 <- data.frame(
  essence = rep("BOP", 9000), #
  id_pe = seq(1, 9000, 1), #
  no_arbre = rep(1, 9000), #
  sdom_bio = rep(c("2EST", "3EST", "3OUEST", "4EST", "4OUEST", "5EST", "5OUEST", "6EST", "6OUEST"), 1000),
  cl_drai = rep("2", 9000),
  veg_pot = rep("MS2", 9000),
  DHP_Ae = rep(150, 9000), ##
  HT_REELLE_M = rep(3, 9000),
  HAUTEUR_M = rep(10, 9000),
  nbTi_ha = 2000, # /// N
  st_ha = 15, # /// BA
  ALTITUDE = 200,
  nom_grade1 = "sciage court",
  long_grade1 = 8,
  diam_grade1 = 10,
  nom_grade2 = "pate",
  long_grade2 = 4,
  diam_grade2 = 5,
  nom_grade3 = "autre",
  long_grade3 = 2,
  diam_grade3 = 3
)

data_diam4 <- data.frame(
  essence = rep(c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIG", "SAB", "THO"), 100000),
  id_pe = seq(1, 900000, 1),
  no_arbre = rep(1, 900000, 1),
  sdom_bio = rep(c("2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5OUEST", "6EST", "6OUEST"), 100000),
  cl_drai = rep(c("2", "4", "1", "3", "5", "6", "1", "2", "3"), 100000),
  veg_pot = rep(c("MS2", "RE3", "MJ2", "FE2", "RE2", "RE1", "RB2", "MS1", "RS2"), 100000),
  DHP_Ae = rep(150, 900000),
  HT_REELLE_M = rep(2:4, 300000),
  HAUTEUR_M = rep(10:12, 300000),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200,
  nom_grade1 = "sciage court",
  long_grade1 = 8,
  diam_grade1 = 10,
  nom_grade2 = "pate",
  long_grade2 = 4,
  diam_grade2 = 5,
  nom_grade3 = "autre",
  long_grade3 = 2,
  diam_grade3 = 3
)


data_diam5 <- data.frame(
  essence = rep(c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIB", "PIG", "SAB", "THO", "ERS"), 4000),
  id_pe = seq(1, 44000, 1),
  no_arbre = seq(1, 44000, 1),
  sdom_bio = rep(c("2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5EST", "5OUEST", "6EST", "6OUEST", "1"), 4000),
  cl_drai = rep(c("2", "4", "1", "3", "5", "6", "0", "1", "2", "3", "4"), 4000),
  veg_pot = rep(c("MS2", "RE3", "MJ2", "FE2", "RE2", "RE1", "MJ1", "RB2", "MS1", "RS2", "FE2"), 4000),
  DHP_Ae = rep(150, 44000),
  HT_REELLE_M = rep(5, 44000),
  HAUTEUR_M = rep(10, 44000),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200,
  nom_grade1 = "sciage court",
  long_grade1 = 8,
  diam_grade1 = 10,
  nom_grade2 = "pate",
  long_grade2 = 4,
  diam_grade2 = 5,
  nom_grade3 = "autre",
  long_grade3 = 2,
  diam_grade3 = 3
)

data_diam6 <- data.frame(
  essence = rep(c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIB", "PIG", "SAB", "THO", "ERS"), 50),
  id_pe = seq(1, 550, 1),
  no_arbre = seq(1, 550, 1),
  sdom_bio = rep(c("2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5EST", "5OUEST", "6EST", "6OUEST", "1"), 50),
  cl_drai = rep(c("2", "4", "1", "3", "5", "6", "0", "1", "2", "3", "4"), 50),
  veg_pot = rep(c("MS2", "RE3", "MJ2", "FE2", "RE2", "RE1", "MJ1", "RB2", "MS1", "RS2", "FE2"), 50),
  DHP_Ae = rep(150, 550),
  HT_REELLE_M = rep(1, 550),
  HAUTEUR_M = rep(10, 550),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200,
  nom_grade1 = "sciage court",
  long_grade1 = 8,
  diam_grade1 = 10,
  nom_grade2 = "pate",
  long_grade2 = 4,
  diam_grade2 = 5,
  nom_grade3 = "autre",
  long_grade3 = 2,
  diam_grade3 = 3
)


data_diam7 <- data.frame(
  essence = rep(c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIB", "PIG", "SAB", "THO", "ERS"), 50000),
  id_pe = seq(1, 550000, 1),
  no_arbre = seq(1, 550000, 1),
  sdom_bio = rep(c("2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5EST", "5OUEST", "6EST", "6OUEST", "1"), 50000),
  cl_drai = rep(c("2", "4", "1", "3", "5", "6", "0", "1", "2", "3", "4"), 50000),
  veg_pot = rep(c("MS2", "RE3", "MJ2", "FE2", "RE2", "RE1", "MJ1", "RB2", "MS1", "RS2", "FE2"), 50000),
  DHP_Ae = rep(150, 550000),
  HT_REELLE_M = rep(1, 550000),
  HAUTEUR_M = rep(10, 550000),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200
)

data_diam8 <- data.frame(
  essence = rep(c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIG", "SAB", "THO"), 100),
  id_pe = seq(1, 900, 1),
  no_arbre = rep(1, 900, 1),
  sdom_bio = rep(c("2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5OUEST", "6EST", "6OUEST"), 100),
  cl_drai = rep(c("2", "4", "1", "3", "5", "6", "1", "2", "3"), 100),
  veg_pot = rep(c("MS2", "RE3", "MJ2", "FE2", "RE2", "RE1", "RB2", "MS1", "RS2"), 100),
  DHP_Ae = rep(150, 900),
  HT_REELLE_M = rep(2:4, 300),
  HAUTEUR_M = rep(10:12, 300),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200,
  nom_grade1 = "sciage court",
  long_grade1 = 8,
  diam_grade1 = 10,
  nom_grade2 = "pate",
  long_grade2 = 4,
  diam_grade2 = 5,
  nom_grade3 = "autre",
  long_grade3 = 2,
  diam_grade3 = 3
)

data_diam9 <- data.frame(
  essence = rep(c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIB", "PIG", "SAB", "THO", "ERS"), 50),
  id_pe = seq(1, 550, 1),
  no_arbre = seq(1, 550, 1),
  sdom_bio = rep(c("2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5EST", "5OUEST", "6EST", "6OUEST", "1"), 50),
  cl_drai = rep(c("2", "4", "1", "3", "5", "6", "0", "1", "2", "3", "4"), 50),
  veg_pot = rep(c("MS2", "RE3", "MJ2", "FE2", "RE2", "RE1", "MJ1", "RB2", "MS1", "RS2", "FE2"), 50),
  DHP_Ae = rep(150, 550),
  HT_REELLE_M = rep(1, 550),
  HAUTEUR_M = rep(10, 550),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200,
  nom_grade1 = "sciage court",
  long_grade1 = 8,
  diam_grade1 = 10,
  nom_grade2 = "pate",
  long_grade2 = 6,
  diam_grade2 = 5
)

data_diam10 <- data.frame(
  essence = rep(c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIB", "PIG", "SAB", "THO", "ERS"), 50),
  id_pe = seq(1, 550, 1),
  no_arbre = seq(1, 550, 1),
  sdom_bio = rep(c("2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5EST", "5OUEST", "6EST", "6OUEST", "1"), 50),
  cl_drai = rep(c("2", "4", "1", "3", "5", "6", "0", "1", "2", "3", "4"), 50),
  veg_pot = rep(c("MS2", "RE3", "MJ2", "FE2", "RE2", "RE1", "MJ1", "RB2", "MS1", "RS2", "FE2"), 50),
  DHP_Ae = rep(150, 550),
  HT_REELLE_M = rep(1, 550),
  HAUTEUR_M = rep(10, 550),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200,
  nom_grade1 = "sciage court",
  long_grade1 = NA,
  diam_grade1 = 10
)

data_diam11 <- data.frame(
  essence = rep(c("BOP", "EPB", "EPN", "EPR", "PEG", "PET", "PIG", "SAB", "THO"), 100),
  id_pe = seq(1, 900, 1),
  no_arbre = rep(1, 900, 1),
  sdom_bio = rep(c("2EST", "2OUEST", "3EST", "3OUEST", "4EST", "4OUEST", "5OUEST", "6EST", "6OUEST"), 100),
  cl_drai = rep(c("2", "4", "1", "3", "5", "6", "1", "2", "3"), 100),
  veg_pot = rep(c("MS2", "RE3", "MJ2", "FE2", "RE2", "RE1", "RB2", "MS1", "RS2"), 100),
  DHP_Ae = rep(150, 900),
  HT_REELLE_M = rep(2:4, 300),
  HAUTEUR_M = rep(10:12, 300),
  nbTi_ha = 2000,
  st_ha = 15,
  ALTITUDE = 200,
  nom_grade1 = "sciage court",
  long_grade1 = 12,
  diam_grade1 = 12,
  nom_grade2 = "pate",
  long_grade2 = 4,
  diam_grade2 = 5,
  nom_grade3 = "autre",
  long_grade3 = NA,
  diam_grade3 = 3
)

data_diam12 <- data.frame(
  essence = rep(c("BOP"), 3),
  id_pe = rep(1, 3),
  no_arbre = 1:3,
  sdom_bio = rep(c("3OUEST"), 3),
  cl_drai = rep(NA, 3),
  veg_pot = rep("MS2", 3),
  DHP_Ae = c(120, 150, 300),
  HT_REELLE_M = rep(0, 3),
  HAUTEUR_M = c(13, 20, 28),
  nbTi_ha = NA,
  st_ha = NA,
  ALTITUDE = NA,
  nom_grade1 = "sciage long",
  long_grade1 = NA,
  diam_grade1 = 10,
  nom_grade2 = "sciage court",
  long_grade2 = NA,
  diam_grade2 = 8,
  nom_grade3 = "pate",
  long_grade3 = NA,
  diam_grade3 = 6
)

data_diam13 <- data.frame(
  essence = c("BOP", "BOP", "BOP"),
  id_pe = rep(1, 3),
  no_arbre = 1:3,
  sdom_bio = rep(c("3OUEST"), 3),
  cl_drai = rep(NA, 3),
  veg_pot = rep("MS2", 3),
  DHP_Ae = c(120, 150, 300),
  HT_REELLE_M = rep(0, 3),
  HAUTEUR_M = c(13, 20, 28),
  nbTi_ha = NA,
  st_ha = NA,
  ALTITUDE = NA
)


# usethis::use_data(data_diam4, data_diam5, data_diam6, data_diam7, data_diam8, data_diam9, data_diam10, data_diam11, data_diam12,
#                  defil_liste_ess,
#                  overwrite = TRUE,
#                  internal = FALSE)

# ces fichiers seront sous data/ et toujours accessible quand le package est loadé
