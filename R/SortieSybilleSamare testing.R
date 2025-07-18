SortieSybille2 <- function(Data, dhs = 0.15, nom_grade1 = NA, long_grade1 = NA, diam_grade1 = NA, nom_grade2 = NA, long_grade2 = NA, diam_grade2 = NA,
                          nom_grade3 = NA, long_grade3 = NA, diam_grade3 = NA) {

  setDT(Data)

  # Calculer nbTi_ha et st_ha et inclure dans Data
  Data[Etat != 'mort', `:=`(
    nbTi_ha = sum(Nombre/0.04),
    st_ha = sum(pi*(DHPcm/200)^2 * Nombre/0.04)
  ), by = .(PlacetteID, Annee)]

  ## Renommer les colonnes pour préparer le Data dans Sybille
  setnames(Data, c("PlacetteID", "DHPcm", "Hautm", "ArbreID", "Espece"),
           c("id_pe", "DHP_Ae", "HAUTEUR_M", "no_arbre", "essence"))

  # Ajouter les colonnes manquantes et effectuer les traitements de préparation de données
  # Puisque SaMARE n'utilise pas ces colonnes, on les initialise à NA pour Sybille
  Data[, `:=`(
    cl_drai = NA_character_,
    veg_pot = NA_character_,
    sdom_bio = NA_character_,
    ALTITUDE = NA_real_
  )]

  # temporaire pour résultats
  Data[, essence := "BOP"]

  # Keep
  Data[, HT_REELLE_M := 0]

  # Multiplier par 10 pour satisfaire le calcul avec DHP_Ae
  Data[, DHP_Ae := DHP_Ae * 10]

  # On prend que les lignes où hauteur existe
  Data <- Data[is.finite(HAUTEUR_M)]

  # Garder que les colonnes nécessaires pour utiliser Sybille
  Data_treated <- Data[, .(essence, id_pe, no_arbre, sdom_bio, cl_drai, veg_pot, DHP_Ae, HT_REELLE_M, HAUTEUR_M, nbTi_ha, st_ha, ALTITUDE, Annee)]

  # Application de Sybille sur les données
  Data_calculated <- calcul_vol_bille(Data_treated, dhs, nom_grade1, long_grade1, diam_grade1, nom_grade2, long_grade2, diam_grade2,
                                                 nom_grade3, long_grade3, diam_grade3)

  #On renomme les colonnes pour matcher avec SaMARE
  setnames(Data_calculated, c("id_pe", "no_arbre"),
           c("PlacetteID", "ArbreID"))

  #On garde que les colonnes nécessaires
  Data_calculated <- Data_calculated[, .(PlacetteID, ArbreID, Annee, grade_bille, vol_bille_dm3)]

  return(Data_calculated)

}

###
#result11 <- read.csv("C:/Users/boini5/OneDrive - BuroVirtuel/Bureau/MRNF Projects/OutilsDRF/data-raw/testSamare.csv")
#result22 <- SortieSybille2(result11, dhs = 0.15, nom_grade1 = "sciage long", long_grade1 = 4, diam_grade1 = 8)
