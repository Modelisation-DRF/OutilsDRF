# fonction pour préparer les variables pour la correction de biais d'une prediction d'un diam avec un des 2 types de modèles
# type_modele = 'arbre' ou 'complet'
# fic = data avec les arbres et contenant la prédiction pred_mm2
# essence = code d'essence en majuscule
correction_biais <- function(type_modele, fic, essence) {
  # type_modele = 'arbre'; fic=data_ess_na; essence='PIG'

  # aller chercher les paramètres du modèle demandé
  random_arbre <- get(paste("defil_random_arbre", type_modele, essence, sep = "_"))
  random_plot <- get(paste("defil_random_plot", type_modele, essence, sep = "_"))
  alpha <- as.numeric(get(paste("defil_alpha", type_modele, essence, sep = "_")))

  sigma_arbre1 <- as.numeric(sqrt(random_arbre[1, 1]))
  sigma_arbre2 <- as.numeric(sqrt(random_arbre[2, 2]))
  corr_arbre <- as.numeric(random_arbre[2, 1] / (sigma_arbre1 * sigma_arbre2))
  if (is.na(corr_arbre)) {
    corr_arbre <- 0
  }
  sigma_plot1 <- as.numeric(sqrt(random_plot[1, 1]))
  sigma_plot2 <- as.numeric(sqrt(random_plot[2, 2]))
  corr_plot <- as.numeric(random_plot[2, 1] / (sigma_plot1 * sigma_plot2))
  if (is.na(corr_plot)) {
    corr_plot <- 0
  }


  fic_biais <- fic %>%
    mutate(
      a12 = -pred_mm2 / alpha * log(HAUTEUR_M / 1.3),
      a21 = a12,
      a22 = pred_mm2 * (log(HAUTEUR_M / 1.3))^2,
      correction = 0.5 * (a12 * corr_plot * sigma_plot1 * sigma_plot2 + a21 * corr_plot * sigma_plot1 * sigma_plot2 + a22 * sigma_plot2^2)
        + 0.5 * (a12 * corr_arbre * sigma_arbre1 * sigma_arbre2 + a21 * corr_arbre * sigma_arbre1 * sigma_arbre2 + a22 * sigma_arbre2^2),
      pred_mm2_corr = pred_mm2 + correction
    ) %>%
    select(-a12, -a21, -a22)

  return(fic_biais)
}
# correction_biais(type_modele='arbre', fic=data_ess_na, essence='PIG')


# fonction pour loader le modele de l'essence traitée et du modele désiré
# type_modele = 'standAndTree' ou 'treeOnly'
# essence = code d'essence en majuscule
get_modele <- function(type_modele, essence) {
  # type_modele = 'treeOnly'; essence='PIG';

  # aller chercher le modele et les parametres de l'essence traitée
  modele <- get(paste0("defil_", type_modele, ".", tolower(essence)))

  return(modele)
}




# fonction pour passer chacune des essences d'un fichier, ne traite que les essences avec un modele de défilement, élimine les autres, donc garder les autres arbres dans un autre fichier
# fic = data avec les variables nécessaires pour l'utilisation d'un modèle de défilement, une ligne par iter/placette/arbre/hauteur
# variables qui doicent être dans fic:
# essence= code de l'essence en majuscule, ex: SAB
# sdom_bio = sous-domaine: ex: 2EST
# cl_drai = classe de drainage, ex: '2'
# veg_pot = code de végétation potentielle , ex: 'MS2'
# DHP_Ae = dhp de l'arbre en mm
# HT_REELLE_M = hauteur de l'arbre en m
# HAUTEUR_M = hauteur à laquelle on veut estimer le diametre (m)
# nbTi_ha = nombre d'arbres à l'ha dans la placette
# st_ha = surface terrière en m2/ha dans la placette
# ALTITUDE = altitude de la placette (m)

get_diam <- function(fic) {
  ### CHANGER CE CODE POUR DU DATA.TABLE AU LIEU DU DPLYR

  # ne garder que les arbres dont l'essence a un modèle
  data_filtre <- fic %>%
    filter(essence %in% defil_liste_ess) %>%
    mutate(
      DIA_MM = 0, # cette variable doit être dans le fichier mais ne sert pas
      z = (HT_REELLE_M - HAUTEUR_M) / (HT_REELLE_M - 1.3),
      x = HAUTEUR_M / HT_REELLE_M
    )

  # s'il y a au moins 1 obs à traiter
  if (nrow(data_filtre) > 0) {
    # traite les arbres d'une essence à la fois
    data_tous <- NULL
    for (ess in defil_liste_ess) {
      # ess='BOP'

      data_ess <- data_filtre %>% filter(essence == ess)

      # s'il y a des arbres de cette essence
      if (nrow(data_ess) > 0) {
        # si l'ess n'est pas PIB, essayer le modèle complet (il n'y a pas de modèle complet pour PIB)
        if (ess != "PIB") {
          # créer les groupes de veg_pot
          if (!all(is.na(defil_group_vp[[ess]]))) { # si la colonne ess n'est pas vide, on fait l'association
            group_temp <- defil_group_vp
            group_temp$group.veg <- group_temp[[ess]]
            data_ess <- left_join(data_ess, group_temp[, c("veg_pot", "group.veg")], by = "veg_pot")
          }
          # créer les groupes de sous-domaine
          if (!all(is.na(defil_group_sd[[ess]]))) { # si la colonne ess n'est pas vide, on fait l'association
            group_temp <- defil_group_sd
            group_temp$group.sDomBio <- group_temp[[ess]]
            data_ess <- left_join(data_ess, group_temp[, c("sdom_bio", "group.sDomBio")], by = "sdom_bio")
          }
          # créer les groupes de la classe de drainage
          if (!all(is.na(defil_group_dr[[ess]]))) { # si la colonne ess n'est pas vide, on fait l'association
            group_temp <- defil_group_dr
            group_temp$group.drainage <- group_temp[[ess]]
            data_ess <- left_join(data_ess, group_temp[, c("cl_drai", "group.drainage")], by = "cl_drai")
          }

          # on essaie de prédire avec le modèle complet
          # aussitot qu'il y a une ligne avec une variable nécessaire au modèle mais avec un NA, ça ne fonctionne pour aucune ligne
          # il faut donc séparer le fichier en deux, ceux avec des NA et ceux sans NA
          data_ess_non_na <- data_ess %>% filter(complete.cases(.))
          data_ess_na <- data_ess %>% filter(!complete.cases(.))

          if (nrow(data_ess_non_na) > 0) {
            # aller chercher le modele de l'essence traitée
            modele <- get_modele(type_modele = "standAndTree", essence = ess)

            # calcul de diam
            data_ess_non_na$pred_mm2 <- predict(modele, newdata = data_ess_non_na, level = 0)

            # ajouter correction de biais d'une prediction modèle complet
            data_ess_non_na <- correction_biais(type_modele = "complet", fic = data_ess_non_na, essence = ess)
          }
        }

        if (nrow(data_ess_na) > 0 | ess == "PIB") { # si le modèle complet ne peut pas fonctionner pour certaines lignes ou si ess=PIB, utiliser le modèle arbre

          # aller chercher le modele de l'essence traitée
          modele <- get_modele(type_modele = "treeOnly", essence = ess)

          # calcul de diam
          data_ess_na$pred_mm2 <- predict(modele, newdata = data_ess_na, level = 0)

          # ajouter correction de biais d'une prediction modèle arbre
          data_ess_na <- correction_biais(type_modele = "arbre", fic = data_ess_na, essence = ess)
        }

        # remettre les 2 fichiers en un seul
        data_ess <- bind_rows(data_ess_na, data_ess_non_na)

        # on accumule les ess
        data_tous <- bind_rows(data_tous, data_ess) %>% select(-contains("group."))
      }
    }
  } else {
    data_tous <- NULL
  }

  return(data_tous)
}



##################################################

# data_diam1 <- get_diam(fic=data_diam1)
# data_diam2 <- get_diam(fic=data_diam2)
