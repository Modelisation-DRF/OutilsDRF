# ce fichier csv a été créé à partir du fichier ex_qualite_calculs_v2.xlsx, dans lequel on a appliqué les équations de qualité à la main dans Excel
fic <- read_delim(test_path("fixtures/attrib_qualite", "resultatAttendu_attributionQualite_DET.csv"), delim = ";")
saveRDS(fic, test_path("fixtures/attrib_qualite", "resultatAttendu_attributionQualite_DET.rds"))



# fichier avec plus de lignes: toutes les combinaisons ess/sdom/gr_dhp/mscr
# 8 x 10 x 3 x 3  = 720, il y a moins de cas pour 5O et 6E
fic <- read_delim(test_path("fixtures/attrib_qualite", "entreeQualiteLourd.csv"), delim = ";")
saveRDS(fic, test_path("fixtures/attrib_qualite", "entreeQualiteLourd.rds"))


# fichier des résultats attendus pour toutes les combinaisons, calculé dans Excel dans le fichier CalculQualiteLourd.xlsx
fic <- read_delim(test_path("fixtures/attrib_qualite", "resultatQualiteLourd.csv"), delim = ";")
saveRDS(fic, test_path("fixtures/attrib_qualite", "resultatQualiteLourd.rds"))



# faire un fichier de toutes les combinaisons possibles de ess/sdom/mscr/eq pour les tests
liste_arbre <- read.xlsx(test_path("fixtures/attrib_qualite", "CalculQualiteLourd.xlsx"), sheetName = "Depart")
liste_arbre <- liste_arbre %>%
  rename(sdom_bio = sdom) %>%
  mutate(Equation = ifelse(dhpcm > 23 & dhpcm <= 33, 1, ifelse(dhpcm > 33 & dhpcm <= 39, 2, ifelse(dhpcm > 39, 3, NA)))) %>%
  left_join(qualite0_ass_sdom)
write_delim(liste_arbre, test_path("fixtures/attrib_qualite", "CalculQualiteLourd_ass_sdom.csv"), delim = ";")

liste_arbre_eq1 <- liste_arbre %>%
  filter(dhpcm > 23 & dhpcm <= 33) %>%
  mutate(Equation = 1) %>%
  left_join(qualite0_ass_sdom)
liste_arbre_eq2 <- liste_arbre %>%
  filter(dhpcm > 33 & dhpcm <= 39) %>%
  mutate(Equation = 2) %>%
  left_join(qualite0_ass_sdom)
liste_arbre_eq3 <- liste_arbre %>%
  filter(dhpcm > 39) %>%
  mutate(Equation = 3) %>%
  left_join(qualite0_ass_sdom)
write_delim(liste_arbre_eq1, test_path("fixtures/attrib_qualite", "CalculQualiteLourd_eq1.csv"), delim = ";")
write_delim(liste_arbre_eq2, test_path("fixtures/attrib_qualite", "CalculQualiteLourd_eq2.csv"), delim = ";")
write_delim(liste_arbre_eq3, test_path("fixtures/attrib_qualite", "CalculQualiteLourd_eq3.csv"), delim = ";")

resultatQualiteLourdEq1 <- read.xlsx(test_path("fixtures/attrib_qualite", "CalculQualiteLourd_v2.xlsx"), sheetName = "eq1_resultat")
resultatQualiteLourdEq2 <- read.xlsx(test_path("fixtures/attrib_qualite", "CalculQualiteLourd_v2.xlsx"), sheetName = "eq2_resultat")
resultatQualiteLourdEq3 <- read.xlsx(test_path("fixtures/attrib_qualite", "CalculQualiteLourd_v2.xlsx"), sheetName = "eq3_resultat")

resultatQualiteLourd_tous <- rbind(resultatQualiteLourdEq1, resultatQualiteLourdEq2, resultatQualiteLourdEq3) %>%
  mutate(
    prop_a = as.numeric(prop_a),
    prop_b = as.numeric(prop_b),
    prop_c = as.numeric(prop_c),
    prop_d = as.numeric(prop_d)
  )

saveRDS(resultatQualiteLourd_tous, test_path("fixtures/attrib_qualite", "resultatQualiteLourd_avec_ass_ess.rds"))
saveRDS(liste_arbre, test_path("fixtures/attrib_qualite", "entreeQualiteLourd_avec_ass_ess.rds"))


# fichier test pour le stochastique
liste_arbre <- readRDS(test_path("fixtures/attrib_qualite", "entreeQualiteLourd_avec_ass_ess.rds")) %>% select(-sdom)
liste_arbre1 <- liste_arbre %>% mutate(iter=1)
liste_arbre2 <- liste_arbre %>% mutate(iter=2)
liste_arbre <- bind_rows(liste_arbre1, liste_arbre2)
set.seed(1234)
resultatAttributionSTO <- attrib_qualite(liste_arbre, "STO", nb_iter = 2, seed_value = 1234)
saveRDS(resultatAttributionSTO, test_path("fixtures/attrib_qualite", "resultatAttributionSTO_avec_ass_ess.rds"))
