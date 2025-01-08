# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw

# param_tarif = read.sas7bdat("c:/Mes docs/ data/ beta_volume.sas7bdat")
# param_ht = read.sas7bdat("c:/Mes docs/ data/ beta_ht.sas7bdat")
# Puis utiliser la ligne de code suivant (toujours dans le projet du package)
# usethis::use_data(param_tarif, param_ht, internal=TRUE): ça fonctionne seulement si le projet est un package

#library(readxl)
#library(sas7bdat)


#### RELATION HT-DHP ####

# fichier association des essences pour le modèle de hauteur-dhp
library(readxl)
library(sas7bdat)
ht_ass_ess <- read_excel("data-raw/Parametre_ht-dhp/index_essence.xls")

# fichier d'associasions des sdom, pert, etc. pour les modèles ht-dhp, un fichier par essence
liste_ess_ht <- unique(ht_ass_ess$essence_hauteur)
ht_ass_pert <- c(); ht_ass_mil <- c(); ht_ass_sd <- c(); ht_ass_vp <- c();
for (ess in liste_ess_ht) {
  suppressMessages(
    ass_p <- read_delim(paste("data-raw/Parametre_ht-dhp/Association_var_cat_capsis/",ess,"/listePerturb",ess,".csv", sep=''), delim = ';') %>%
      dplyr::select(-absent) %>%
      mutate(essence=ess))
  suppressMessages(
    ass_m  <- read_delim(paste("data-raw/Parametre_ht-dhp/Association_var_cat_capsis/",ess,"/listeTypeEco4",ess,".csv", sep=''), delim = ';') %>%
      dplyr::select(-absent) %>%
      mutate(essence=ess))
  suppressMessages(
    ass_s   <- read_delim(paste("data-raw/Parametre_ht-dhp/Association_var_cat_capsis/",ess,"/listeSdom",ess,".csv", sep=''), delim = ';') %>%
      dplyr::select(-absent) %>%
      mutate(essence=ess))
  suppressMessages(
    ass_v   <- read_delim(paste("data-raw/Parametre_ht-dhp/Association_var_cat_capsis/",ess,"/listeVp2",ess,".csv", sep=''), delim = ';') %>%
      dplyr::select(-absent) %>%
      mutate(essence=ess))
  ht_ass_pert <- bind_rows(ht_ass_pert, ass_p)
  ht_ass_mil <- bind_rows(ht_ass_mil, ass_m)
  ht_ass_sd <- bind_rows(ht_ass_sd, ass_s)
  ht_ass_vp <- bind_rows(ht_ass_vp, ass_v)
}
ht_ass_sd <- ht_ass_sd %>% rename(sdom_bio=SDOM_BIO)
ht_ass_mil <- ht_ass_mil %>% mutate(type_eco4 = as.character(type_eco4),
                                    milieu = as.character(milieu))


# Fichiers des paramètres pour la relation ht-dhp, un par essence, pour la fonction param_ht_stoch()
liste_ess_ht <- unique(ht_ass_ess$essence_hauteur)
ht_param_fixe <- list()
ht_param_cov <- list()
ht_param_random <- c()
for (ess in liste_ess_ht) {

# lecture des effets fixes
  suppressMessages(
    param   <- read_delim(paste("data-raw/Parametre_ht-dhp/Association_var_cat_capsis/",ess,"/Parameters",ess,".csv", sep=''), delim = ';')
  )
  param2 <- param %>%
    dplyr::select(-StdErr, -DF, -tValue, -Probt) %>%
    #filter(estimate != 0) %>%
    mutate(effet = ifelse(effect=="ldhp*milieu", paste("mil", milieu, sep='_'),
                          ifelse(effect=="ldhp*alt", "ef_alt",
                                 ifelse(effect=="ldhp*pert", paste("pert", pert, sep='_'),
                                        ifelse(effect=="ldhp*ptot", "ef_ptot",
                                               ifelse(effect=="ldhp*rdhp", "ef_rdhp",
                                                      ifelse(effect=="ldhp*sdom", paste("sd", sdom, sep='_'),
                                                             ifelse(effect=="ldhp*st", "ef_st",
                                                                    ifelse(effect=="ldhp*tmoy", "ef_tmoy",
                                                                           ifelse(effect=="ldhp*vp", paste("vp", vp, sep='_'),
                                                                                  ifelse(effect=="ldhp", "ef_ldhp", "ef_ldhp2")))))))))))
  param2_tr <- param2 %>%
    dplyr::select(effet, estimate) %>%
    pivot_wider(names_from = effet, values_from = estimate)
  param2_tr$essence <- ess
  #ht_param_fixe <- bind_rows(ht_param_fixe, param2_tr)
  ht_param_fixe <- append(ht_param_fixe, list(param2_tr))
  # lecture des matrices de cov des effets fixe
  suppressMessages(
    covparam   <- read_delim(paste("data-raw/Parametre_ht-dhp/Association_var_cat_capsis/",ess,"/omega",ess,".csv", sep=''), delim = ';') %>%
      dplyr::select(contains("Col"))
  )
  covparam$essence <- ess
  #ht_param_cov <- bind_rows(ht_param_cov, covparam)
  ht_param_cov <- append(ht_param_cov, list(covparam))
  # lecture des effets aléatoires
  suppressMessages(
    rand_ht   <- read_delim(paste("data-raw/Parametre_ht-dhp/Association_var_cat_capsis/",ess,"/randomEffects",ess,".csv", sep=''), delim = ';')
  )
  rand_ht$essence <- ess
  ht_param_random <- bind_rows(ht_param_random, rand_ht)
}
# il faut les effets fixes et les matrices de cov dans des listes, sinon les colonnes sont mélangées
names(ht_param_fixe) <- liste_ess_ht
names(ht_param_cov) <- liste_ess_ht
ht_liste_ess <- liste_ess_ht


#### TARIF DE CUBAGE ######
# fichier des paramètres du tarif de cubage pour la fonction param_vol_stoch()
tarif_param_fixe <- read.sas7bdat("data-raw/Parametre_vol/beta_volume.sas7bdat") %>%
  mutate(beta = ifelse(Effect=="ht_dhp",'b1',
                       ifelse(Effect=="cylindre*ESSENCE","b2","b3")),
         beta_ess = ifelse(ESSENCE != '', paste(beta,ESSENCE, sep = '_'), beta)) %>%
  dplyr::select(beta_ess, Estimate)


tarif_param_cov <- read.sas7bdat("data-raw/Parametre_vol/beta_covb.sas7bdat") %>%
  dplyr::select(contains("Col"))


tarif_param_random <- read.sas7bdat("data-raw/Parametre_vol/covparms.sas7bdat")


# fichier association des essences aux essences du tarif de cubage
tarif_ass_ess <- read_delim("data-raw/Parametre_vol/Essences_Volume.csv", delim=";")

# fichier d'association des reg_eco aux sdom_bio
regeco_ass_sdom <- read_delim("data-raw/Ass_regeco_sdombio.csv", delim=";")
regeco_ass_sdom <- regeco_ass_sdom %>% mutate(dom_bio = as.character(dom_bio))

# tous les fichier à mettre dans le rda
usethis::use_data(ht_ass_ess, ht_liste_ess, ht_ass_pert, ht_ass_mil, ht_ass_sd, ht_ass_vp, ht_param_fixe, ht_param_cov, ht_param_random,
                  tarif_param_fixe, tarif_param_cov, tarif_param_random, tarif_ass_ess,
                  regeco_ass_sdom,
                  internal=TRUE, overwrite = TRUE)



#tar('TarifQC.tar.gz', compression = 'gzip', tar="tar")
