#Fichier contenant sortie_sybille pour tester avec un fichier artémis
#Result <- fread("C:/Users/boini5/OneDrive - BuroVirtuel/Bureau/MRNF Projects/OutilsDRF/data-raw/Result.csv")
#PropEPX <- fread("C:/Users/boini5/OneDrive - BuroVirtuel/Bureau/MRNF Projects/OutilsDRF/data-raw/PropEPX.csv")
#artemis_test <- SortieSybille(Result, nom_grade1 = "pate", long_grade1 = 4, diam_grade1 = 8) -> Ceci calcule le fichier résultat voulu

SortieSybille <- function(Data, dhs = 0.15, nom_grade1 = NA, long_grade1 = NA, diam_grade1 = NA, nom_grade2 = NA, long_grade2 = NA, diam_grade2 = NA,
                          nom_grade3 = NA, long_grade3 = NA, diam_grade3 = NA) {

  # Préparer toutes les colonnes pour utilisation de Sybille
  setDT(Data)

  # Calculer nbTi_ha et st_ha et inclure dans Data
  Data[Etat != 'mort', `:=`(
    nbTi_ha = sum(Nombre/0.04),
    st_ha = sum(pi*(DHPcm/200)^2 * Nombre/0.04)
  ), by = .(PlacetteID, Annee)]

  # Faire une copie de la table PropEPX
  PropEPX_copy <- copy(PropEPX)
  # Renommer VEG_POT -> Veg_Pot
  setnames(PropEPX_copy, "VEG_POT", "Veg_Pot")

  # Faire le merge sur Veg_Pot
  Data <- merge(Data, PropEPX_copy, by = "Veg_Pot", all.x = TRUE)

  Data[Espece == "" | trimws(Espece) == "", Espece := NA]
  # Appliquer la logique pour les différents cas
  Data_temp <- Data[, EssenceFinale := ifelse(
    #
    !is.na(Espece) & Espece != "", Espece,
    ifelse(is.na(Espece) & !(GrEspece %in% c("EPX", "PIN", "PEU")), GrEspece,
           ifelse(is.na(Espece) & GrEspece == "PIN", "PIB",
                  ifelse(is.na(Espece) & GrEspece == "PEU", "PET",
                         ifelse(is.na(Espece) & GrEspece == "EPX" & PropEPB >= 0.5, "EPB",
                                ifelse(is.na(Espece) & GrEspece == "EPX" & PropEPB < 0.5, "EPN",
                                       NA_character_))))))
  ]

  # Renommer les colonnes pour préparer le Data dans Sybille
  setnames(Data_temp, c("Veg_Pot", "PlacetteID", "DHPcm", "Altitude", "hauteur_pred", "origTreeID", "EssenceFinale", "Cl_Drai"),
             c("veg_pot", "id_pe", "DHP_Ae", "ALTITUDE", "HAUTEUR_M", "no_arbre", "essence", "cl_drai"))

  # Ajouter les colonnes manquantes et effectuer les traitements de préparation de données
  Data_temp[, HT_REELLE_M := 0]
  # Multiplier par 10 pour satisfaire le calcul avec DHP_Ae
  Data_temp[, DHP_Ae := DHP_Ae * 10]
  # Prendre que le premier caractère de cl_drai
  Data_temp[, cl_drai := str_sub(cl_drai, 1, 1)]
  # Tranformation du caractère E ou O en Est/Ouest pour sdom_bio si besoin, sinon on ne fait rien
  Data_temp[, cl_drai := as.character(cl_drai)]
  Data_temp[, sdom_bio := ifelse(
    substr(sdom_bio, 2, 2) == "E",
    paste0(substr(sdom_bio, 1, 1), "EST"),
    ifelse(
      substr(sdom_bio, 2, 2) == "O",
      paste0(substr(sdom_bio, 1, 1), "OUEST"),
      sdom_bio
    )
  )]

  # Garder que les colonnes nécessaires pour utiliser Sybille
  Data_treated <- Data_temp[, .(essence, id_pe, no_arbre, sdom_bio, cl_drai, veg_pot, DHP_Ae, HT_REELLE_M, HAUTEUR_M, nbTi_ha, st_ha, ALTITUDE, Annee)]

  # Application de Sybille sur les données
  Data_calculated <- calcul_vol_bille(Data_treated, dhs, nom_grade1, long_grade1, diam_grade1, nom_grade2, long_grade2, diam_grade2,
                                                 nom_grade3, long_grade3, diam_grade3)

  # Probablement remettre toutes les autres colonnes pour le traitement fusion
  # Rechanger les noms de colonnes de la table de base, car on a changé selon la référence
  setnames(Data, c("id_pe", "no_arbre"),
           c("PlacetteID", "origTreeID"))

  setnames(Data_calculated, c("id_pe", "no_arbre"),
           c("PlacetteID", "origTreeID"))

  merged_data <- merge(Data, Data_calculated,
                       by = c("PlacetteID", "origTreeID", "Annee"),
                       all = TRUE)

  merged_data[, c("PropEPB", "HT_REELLE_M", "DHP_Ae", "essence", "ht", "st_ha", "nbTi_ha") := NULL]

  return(merged_data)

}
