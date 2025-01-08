# exemple d'utilisation
# fichier data/fic_arbres_test.rda

# loader tous les fichier rda
#file_list <- list.files("data/", full.names = TRUE)
#walk(file_list, ~ load(.x, .GlobalEnv))
#devtools::load_all()


# fichier des PET4, 2 placettes de tiges
file_arbre="U:\\Projets\\IsabelleAuger\\Natura-2020\\PET\\PET_arbres.csv"
fichier_arbres <- read_delim(file=file_arbre, delim = ';')
names(fichier_arbres) <- tolower(names(fichier_arbres))
# fichier d'arbres regroupés en placette
fic_arbres_test_temp <- fichier_arbres %>%
   filter(id_pe %in% c('0300600202','0504212702')) %>%
   mutate(veg_pot = substr(type_eco,1,3),
          milieu = as.character(substr(type_eco,4,4)),
          nb_tige = tige_ha/10000*400,
          no_arbre = row_number()) %>%
   dplyr::select(id_pe, sdom_bio, reg_eco, type_eco, veg_pot, milieu, altitude, t_ma, p_tot, tige_ha, nb_tige, no_arbre, essence, dhpcm)

# sauvegarder le fichier sans reg_eco en rda sous /data
fic_arbres_test <- fic_arbres_test_temp %>% dplyr::select(-reg_eco)
usethis::use_data(fic_arbres_test, overwrite = TRUE)

# sauvegarder le fichier sans sdom_bio en rda sous /data
fic_arbres_reg_eco <- fic_arbres_test_temp %>% dplyr::select(-sdom_bio)
usethis::use_data(fic_arbres_reg_eco, overwrite = TRUE)



# Reproduire un fichier résultant d'Artémis en mode stochastique et 5 pas de simulation, à l'échelle de l'arbre
nb_iter=10
nb_step=5
fic <- as.data.frame(unclass(expand_grid(iter = 1:nb_iter, fic_arbres_test)))
fic$dhpcm <- rnorm(nb_iter*length(fic_arbres_test$id_pe), mean=fic_arbres_test$dhpcm, sd=1)
fic <- fic %>% mutate(dhpcm = ifelse(dhpcm<9.1, 9.1, dhpcm))
fic2 <- as.data.frame(unclass(expand_grid(step = 1:nb_step, fic)))
fic2$dhpcm <- rnorm(nb_step*length(fic$id_pe), mean=fic$dhpcm, sd=0.5)
fic_artemis_sto <- fic2 %>% mutate(dhpcm=dhpcm+step,
                                   annee = 2023 + 10*(step-1))
  #dplyr::select(-step)
usethis::use_data(fic_artemis_sto, overwrite = TRUE)




# Simuler un fichier résultant d'Artémis en mode déterministe avec 5 pas de simulation, à l'échelle de l'arbre
fic_artemis_det <- fic_artemis_sto %>% filter(iter==1) %>% dplyr::select(-iter)
usethis::use_data(fic_artemis_det, overwrite = TRUE)

