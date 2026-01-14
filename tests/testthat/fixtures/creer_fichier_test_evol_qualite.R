# utiliser le fichier interne d'exemple ex_qualite_evol pour faire un test
write_delim(ex_qualite_evol, test_path("fixtures/evol_qualite", "ex_qualite_evol.csv"), delim = ";")
saveRDS(ex_qualite_evol, test_path("fixtures/evol_qualite", "ex_qualite_evol.rds"))

# utiliser le fichier interne d'exemple ex_qualite_evol_sto pour faire un test en mode STO
saveRDS(ex_qualite_evol_sto, test_path("fixtures/evol_qualite", "ex_qualite_evol_sto.rds"))


# utiliser le fichier interne des paramètres pour créer un csv pour aller calculer les prob à la main
write_delim(qualite_evol_param, test_path("fixtures/evol_qualite", "qualite_evol_param.csv"), delim = ";") # pour faire les calcul à la main


# pas de trace de comment ce fichier a été créé
fic <- read_delim(test_path("fixtures/evol_qualite", "ex_qualite_evol_sortie.csv"), delim = ";")
saveRDS(fic, test_path("fixtures/evol_qualite", "ex_qualite_evol_sortie.rds"))



# fichier de resultats pour le mode STO
attendu_sto <- evol_qualite(ex_qualite_evol_sto, mode_simul = "STO", nb_iter = 5, seed_value = 1)
saveRDS(attendu_sto, test_path("fixtures/evol_qualite", "attendu_sto.rds"))

# fichier test pour le stochastique
liste_arbre <- readRDS(test_path("fixtures/evol_qualite", "xxx.rds")) %>% select(-sdom)
set.seed(1234)
resultatAttributionSTO <- attrib_qualite(liste_arbre, "STO", nb_iter = 2, seed_value = 1234)
saveRDS(resultatAttributionSTO, test_path("fixtures/evol_qualite", "resultatAttributionSTO_avec_ass_ess.rds"))
