# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw


#### Équations de coupe ####




########################################################


# Fichier des paramètres du modèle de coupe
param   <- read_delim("data-raw/Parametre_coupe/parmsNew.csv", delim = ';')
# table(param$effect_capsis, param$effect) # ok


# lecture des matrices de cov des effets fixe
covparam   <- read_delim("data-raw/Parametre_coupe/covbNew.csv", delim = ';')
# par trt, utiliser les valeurs de la colonne pour remplir une matrice symétrique
list_trt <- unique(covparam$num_trt)
list_covb <- list()
for (i in 1:length(list_trt)) {
  covb <- covparam %>% filter(num_trt==list_trt[i])
  # Taille de la matrice
  n <- (-1 + sqrt(1 + 8 * nrow(covb))) / 2
  n <- as.integer(n)

  # Créer et remplir la matrice
  mat <- matrix(0, n, n)
  mat[upper.tri(mat, diag = TRUE)] <- covb$estimate
  mat <- mat + t(mat) - diag(diag(mat)) # remplir la partie inférieure
  list_covb[[i]] <- mat
}

# est-ce qu'il y a toujours un seul paramètre à 0 par trt?
verif <- param %>% filter(estimate==0) %>% group_by(num_trt) %>% summarise(nb=n()) # il y a 2 trt sans 0, les 12 et 14
# num_trt=18 à 2 0?? oui, le traiter à part, c'est le dernier

# enlever le 18 de la liste des trt
list_trt <- list_trt[1:18]

# il faut ajouter la ligne/colonne de 0 associée au paramètre=0
list_covb_modif <- list()
for (i in 1:length(list_trt)) {
  #i=2
  list_covb_i <- list_covb[[i]]
  param_i <- param %>% filter(num_trt==list_trt[i])
  # numéro de la ligne où est le 0
  num0 <- which(param_i$estimate == 0)
  nb_param <- nrow(param_i)

  if (length(num0)==0) {
    list_covb_modif[[i]] <- list_covb_i} else {

    # remplir une matrice qui a une colonne et une ligne de plus
    mat <- matrix(0, nb_param, nb_param)
    # prendre les éléments des lignes avant num0 et les colonnes avant num0
    list_covb_0_a <- list_covb_i[1:(num0-1), 1:(num0-1)]
    mat[1:(num0-1),1:(num0-1)] <- list_covb_0_a

    # prendre les éléments des lignes avant num0 et les colonnes après num0
    list_covb_0_b <- list_covb_i[1:(num0-1), num0:(nb_param-1)]
    mat[1:(num0-1), (num0+1):nb_param] <- list_covb_0_b

    # prendre les éléments des lignes après num0 et les colonnes avant num0
    list_covb_0_c <- list_covb_i[num0:(nb_param-1), 1:(num0-1)]
    mat[(num0+1):nb_param, 1:(num0-1)] <- list_covb_0_c

    # prendre les éléments des lignes après num0 et les colonne après num0
    list_covb_0_d <- list_covb_i[num0:(nb_param-1), num0:(nb_param-1)]
    mat[(num0+1):nb_param, (num0+1):nb_param] <- list_covb_0_d

    list_covb_modif[[i]] <- mat
  }

}

# traiter le cas où il y a 2 lignes/colonnes de 0 à ajouter, le trt=18, le 19e
list_covb_i <- list_covb[[19]] # 45
param_i <- param %>% filter(num_trt==18) # 47
# numéro des lignes où sont les 0
num0_tous <- which(param_i$estimate == 0) # 12 et 46
nb_param_tous <- nrow(param_i)

# faire la 1e ligne/colonne de 0
num0 <- num0_tous[1]
nb_param <- nb_param_tous-1
# remplir une matrice qui a une colonne et une ligne de plus
mat <- matrix(0, nb_param, nb_param)


# prendre les éléments des lignes avant num0 et les colonnes avant num0
list_covb_0_a <- list_covb_i[1:(num0-1), 1:(num0-1)]
mat[1:(num0-1),1:(num0-1)] <- list_covb_0_a

# prendre les éléments des lignes avant num0 et les colonnes après num0
list_covb_0_b <- list_covb_i[1:(num0-1), num0:(nb_param-1)]
mat[1:(num0-1), (num0+1):nb_param] <- list_covb_0_b

# prendre les éléments des lignes après num0 et les colonnes avant num0
list_covb_0_c <- list_covb_i[num0:(nb_param-1), 1:(num0-1)]
mat[(num0+1):nb_param, 1:(num0-1)] <- list_covb_0_c

# prendre les éléments des lignes après num0 et les colonne après num0
list_covb_0_d <- list_covb_i[num0:(nb_param-1), num0:(nb_param-1)]
mat[(num0+1):nb_param, (num0+1):nb_param] <- list_covb_0_d

# faire la 2e ligne/colonne de 0
num0 <- num0_tous[2]
nb_param <- nb_param_tous
# remplir une matrice qui a une colonne et une ligne de plus
mat2 <- matrix(0, nb_param, nb_param)
# prendre les éléments des lignes avant num0 et les colonnes avant num0
list_covb_0_a <- mat[1:(num0-1), 1:(num0-1)]
mat2[1:(num0-1),1:(num0-1)] <- list_covb_0_a

# prendre les éléments des lignes avant num0 et les colonnes après num0
list_covb_0_b <- mat[1:(num0-1), num0:(nb_param-1)]
mat2[1:(num0-1), (num0+1):nb_param] <- list_covb_0_b

# prendre les éléments des lignes après num0 et les colonnes avant num0
list_covb_0_c <- mat[num0:(nb_param-1), 1:(num0-1)]
mat2[(num0+1):nb_param, 1:(num0-1)] <- list_covb_0_c

# prendre les éléments des lignes après num0 et les colonne après num0
list_covb_0_d <- mat[num0:(nb_param-1), num0:(nb_param-1)]
mat2[(num0+1):nb_param, (num0+1):nb_param] <- list_covb_0_d

list_covb_modif[[19]] <- mat2

####################################################################

# fichier association des essences pour le modèle coupe
# coupe_ass_ess <- read_delim("data-raw/Parametre_coupe/species.csv", delim = ';')
# # dans la liste des essences, il faut ajouter les essences individuelles des essences groupées d'artemis
# epx <- data.frame(essence=c('EPB','EPR','EPN','EPO'), essence_coupe='EPX')
# chx <- data.frame(essence=c('CHB','CHG','CHR','CHE'), essence_coupe='CHX')
# pin <- data.frame(essence=c('PIB','PIR'), essence_coupe='PIN')
# peu <- data.frame(essence=c('PEB','PED','PEG','PET'), essence_coupe='PEU')
# autres groupes d'artémis: AUT, RES, FEU, F_0, F_1, F0R
# groupe de samare

coupe_ess <- read_delim("data-raw/Parametre_coupe/Association-EspeceTot 20250113.csv", delim = ';')
# une colonne par traitement, transposer le fichier pour avoir une colonne traitement
coupe_ess2 <- coupe_ess %>% group_by(SpeciesName) %>% pivot_longer(cols = CA:CPI_RL_CIMOTF, names_to = 'code_trt', values_to = 'essence_coupe') %>% rename(essence=SpeciesName)
# ajouter le numérode du trt
list_trt <- param %>% select(num_trt, code_trt) %>% unique()
coupe_ess3 <- left_join(coupe_ess2, list_trt) %>% select(num_trt, code_trt, essence, essence_coupe)


############################################

# renommer les objets
coupe_param_covb <- list_covb_modif
coupe_param <- param
coupe_ass_ess <- coupe_ess3


############################################

# ajouter les nouveaux fichiers au sysdata existant
# Créer un environnement temporaire
temp_env <- new.env()
# Charger le fichier sysdata.rda dans cet environnement
load("R/sysdata.rda", envir = temp_env)
# Vérifier les objets actuellement dans sysdata.rda
ls(envir = temp_env)
# Ajouter les nouveaux objets à l'environnement temporaire

#temp_env$coupe_param_covb <- coupe_param_covb
#temp_env$coupe_param <- coupe_param
temp_env$coupe_ass_ess <- coupe_ass_ess
view(temp_env$coupe_ass_ess)

# Sauvegarder tous les objets présents dans l'environnement temporaire
save(list = ls(envir = temp_env), file = "R/sysdata.rda", envir = temp_env)

rm(temp_env)

data_tree1 <- data.frame(essence=rep(c("AUR", "BOP", "EPB", "CAR", "CHX"), 1),#
                         id_pe = seq(1,5,1),#
                         no_arbre = rep(1,5),#
                         DHP_Ae = rep(35, 5),  ##
                         nbTi_ha = 2000, #/// N
                         st_ha = 15) #/// BA


#load("R/sysdata.rda")
#ls()


################################################
## ensuite, dans le code qui génèrera les paramètres, après avoir générer les paramètres aléatoires, il faudra faire ceci pour le trt sélectionné:
## effets qui ne dépendent pas de l'essence et les transposer
#ess_non <- param_0 %>% filter(is.na(essence))
#ess_non_tr <- ess_non %>% select(code_trt, num_trt, effect, estimate) %>% group_by(code_trt, num_trt) %>% pivot_wider(names_from = effect, values_from = estimate)
#
## transposer les effets qui dépendent de l'essence
#param_ess <- param_0 %>% filter(!is.na(essence)) %>% select(code_trt, num_trt, essence, effect, estimate) %>%
#  group_by(code_trt, num_trt, essence) %>%
#  pivot_wider(names_from = effect, values_from = estimate)
#
## ajouter les effet qui ne dépendent pas de l'essence
#param_ess2 <- left_join(param_ess, ess_non_tr, by=c('code_trt', 'num_trt'))
#
## ajouter les paramètres qui ne sont pas présents dans ce trt
#list_effets <- c("b1_s", "b2_s", "b4_s", "b3_s", "b5_s", "b6_s", "b7_s", "b0", "b4", "b5", "b6", "b3", "b2")
#effet_presents <- param_ess2 %>% ungroup() %>% select(-code_trt, -num_trt, -essence) %>% names()
#effets_manquants <- setdiff(list_effets, effet_presents)
#for (var in effets_manquants) {
#  param_ess2[[var]] <- NA
#}
#
## copier les parmètres qui ne dépendent pas de l'essence dans les colonnes des effets des essences
#param_ess3 <- param_ess2 %>%
#  mutate(b2_s = ifelse(!is.na(b2), b2, b2_s),
#         b3_s = ifelse(!is.na(b3), b3, b3_s),
#         b4_s = ifelse(!is.na(b4), b4, b4_s),
#         b5_s = ifelse(!is.na(b5), b5, b5_s),
#         b6_s = ifelse(!is.na(b6), b6, b6_s)) %>%
#  select(-c(b2,b3,b4,b5,b6))
## mettre des 0 au lieu des NA
#param_ess3 <-  param_ess3 %>% replace(is.na(.), 0)
#
## on merge les paramètres aux arbre par essence, en sélectionnant le trt désiré
## l'équation s'écrira comme ceci:
## y = b0 + b1_s + (b2_s + b3_s * m)*d + (b4_s + b5_s*m)*d^2 + b6_s*log(N+1) + b7_s*BA
## où d = dhp-23
## où m = 1 si dhp>23
##    m = 0 si dhp<=23






