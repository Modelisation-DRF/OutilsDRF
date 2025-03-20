#' Calcul du volume des billes commerciales à partir d'arbres
#'
#' Cette fonction calcule le volume de billes commerciales pour des arbres en tenant compte
#' des critères de diamètre minimal au fin bout et de longueur. Elle segmente d'abord chaque arbre en
#' sections de 2 pieds, estime le diamètre à chaque hauteur, puis découpe l'arbre en billes
#' selon les paramètres spécifiés. L'algorithme priorise les billes de plus haute valeur et
#' maximise l'utilisation de chaque arbre.
#'
#' @param fichier_billes Un data.frame ou data.table contenant les données d'arbres avec les colonnes suivantes:
#'   \itemize{
#'     \item essence - Essence de l'arbre en majuscule, ex: SAB
#'     \item id_pe - Identifiant de la placette
#'     \item no_arbre - Numéro de l'arbre dans la placette
#'     \item HAUTEUR_M - Hauteur totale de l'arbre (en m)
#'     \item sdom_bio - Sous-domaine: ex: 2EST
#'     \item cl_drai - Classe de drainage, ex: '2'
#'     \item veg_pot - Code de végétation potentielle , ex: 'MS2'
#'     \item DHP_Ae - Diamètre à hauteur de poitrine de l'arbre (en mm)
#'     \item nbTi_ha - Nombre d'arbres à l'ha dans la placette
#'     \item st_ha - Surface terrière en m2/ha dans la placette
#'     \item ALTITUDE - Altitude de la placette (en m)
#'     \item nom_grade1, nom_grade2, nom_grade3 - Noms des types de billes
#'     \item long_grade1, long_grade2, long_grade3 - Longueurs des billes en pieds
#'     \item diam_grade1, diam_grade2, diam_grade3 - Diamètres minimaux au fin bout des billes en cm
#'   }
#'
#' @return Une data.table contenant les billes extraites avec les colonnes:
#'   \itemize{
#'     \item id_pe - Identifiant de la placette
#'     \item no_arbre - Numéro de l'arbre
#'     \item dhpcm - Diamètre à hauteur de poitrine de l'arbre en cm
#'     \item ht - Hauteur de l'arbre en m
#'     \item vol_bille_dm3 - Volume de la bille en dm³
#'     \item grade_bille - Type (grade) de la bille
#'     \item diam_fb_cm - Diamètre minimal au fin bout de la bille en cm
#'     \item long_bille_pied - Longueur de la bille en pied, NA si aucune valeur spécifiée au départ
#'   }
#'
#' @details
#' L'algorithme procède en plusieurs étapes:
#' \enumerate{
#'   \item Segmente chaque arbre en sections de 2 pieds
#'   \item Calcule le diamètre à chaque hauteur via la fonction get_diam
#'   \item Calcule le volume de chaque section
#'   \item Applique l'algorithme de découpe en billes en tenant compte des contraintes
#'   \item Attribue un type (grade) à chaque bille selon les critères
#'   \item Calcule le volume final de chaque bille
#' }
#'
#' La fonction gère plusieurs cas spéciaux:
#' \itemize{
#'   \item Billes avec diamètre minimum mais sans longueur fixe
#'   \item Priorités entre les différents types de billes
#' }
#'
#' La fonction dépend de variables globales:
#' \itemize{
#'   \item defil_liste_ess - Liste des essences valides
#'   \item Les tables de groupes écologiques utilisées par get_diam
#' }
#'
#' @examples
#'   \dontrun{
#'     # Préparation des données d'entrée(exemple)
#'     # Si valeur NA, l'utilisateur n'a pas donnée de valeur pour le paramètre correspondant.
#'     donnees_arbre <- data.frame(
#'       essence = rep(c('BOP'), 3),
#'       id_pe = rep(1, 3),
#'       no_arbre = 1:3,
#'       sdom_bio = rep(c("3OUEST"), 3),
#'       cl_drai = rep(NA, 3),
#'       veg_pot = rep('MS2', 3),
#'       DHP_Ae = c(120, 150, 300),
#'       HT_REELLE_M = rep(0, 3),
#'       HAUTEUR_M = c(13, 20, 28),
#'       nbTi_ha = NA,
#'       st_ha = NA,
#'       ALTITUDE = NA,
#'       nom_grade1 = "vmb",
#'       long_grade1 = 8,
#'       diam_grade1 = 9,
#'       nom_grade2 = "pate",
#'       long_grade2 = 4,
#'       diam_grade2 = 5,
#'       nom_grade3 = "autre",
#'       long_grade3 = NA,
#'       diam_grade3 = 3
#'     )
#'
#'     # Calcul des volumes de billes
#'     resultats <- calcul_vol_bille(donnees_arbre)
#'   }
#' @seealso get_diam(dans le fichier Equation_defil.R) pour le calcul des diamètres le long du tronc
#'
#' @import data.table
#'
#' @export

calcul_vol_bille <- function(fichier_billes) {
  # Conversion en data.table pour optimiser les opérations
  setDT(fichier_billes)
  # Initialisation de la table de résultats
  data_billes <- data.table()

  # Constantes de conversion d'unités
  # Ces ratios permettent de convertir différentes unités(unité de travail interne)
  ratio_pouce_metre <- 39.3700787  # Nombre de pouces dans un mètre
  ratio_cm_mm <- 10                # Nombre de milimètres dans un centimètre
  ratio_cm_metre <- 100            # Nombre de centimètres dans un mètre
  ratio_mm_metre <- 1000           # Nombre de millimètres dans un mètre

  # Facteur d'échelle pour éviter les dépassements numériques lors des calculs de volume
  # Les volumes sont temporairement divisés par ce facteur puis remultipliés à la fin
  scale <- 10**15

  # Longueur de section standard (24.5 pouces convertis en mètres)
  section_metre <- 24.5 / ratio_pouce_metre

  # Hauteur de souche standard en mètres (point de départ des mesures)
  dhs <- 0.15

  # Facteur de conversion pour obtenir des dm³ (décimètres cubes)
  factor <- as.double(1 / 8 * 1000)

  # Filtrage des données pour ne conserver que les essences d'arbres valides
  data_filtre <- fichier_billes[essence %in% defil_liste_ess]
  # S'assure que le résultat est toujours une data.table
  setDT(data_filtre)

  # Commence le traitement uniquement s'il y a des données après filtrage
  if (nrow(data_filtre) > 0) {
    # Extraction des paramètres de billes à partir des données
    # Chaque bille (jusqu'à 3 types) a un nom, une longueur et un diamètre minimal
    # _1, _2, _3 sont les vecteurs bruts extraits des données
    nom_grade_1 <- data_filtre$nom_grade1
    log_length_1 <- data_filtre$long_grade1
    diam_value_1 <- as.numeric(data_filtre$diam_grade1 / ratio_cm_metre)  # Conversion cm -> m
    nom_grade_2 <- data_filtre$nom_grade2
    log_length_2 <- data_filtre$long_grade2
    diam_value_2 <- as.numeric(data_filtre$diam_grade2 / ratio_cm_metre)
    nom_grade_3 <- data_filtre$nom_grade3
    log_length_3 <- data_filtre$long_grade3
    diam_value_3 <- as.numeric(data_filtre$diam_grade3 / ratio_cm_metre)

    # Extraction des valeurs uniques pour chaque paramètre
    # Ces valeurs sont souvent les mêmes pour tous les arbres, on prend donc le premier élément
    nom_grade1 <- nom_grade_1[1]
    log_length1 <- log_length_1[1]
    diam_value1 <- diam_value_1[1]
    nom_grade2 <- nom_grade_2[1]
    log_length2 <- log_length_2[1]
    diam_value2 <- diam_value_2[1]
    nom_grade3 <- nom_grade_3[1]
    log_length3 <- log_length_3[1]
    diam_value3 <- diam_value_3[1]

    # Ces valeurs déterminent de combien de segments on "saute" lors de l'analyse pour les différents types de billes
    jump_log1 <- log_length1 / 2
    jump_log2 <- log_length2 / 2
    jump_log3 <- log_length3 / 2

    # Détermination de la disponibilité des paramètres pour chaque type de bille
    # has_diam: le diamètre minimal est spécifié
    # has_log: la longueur est spécifiée
    has_diam1 <- !is.na(diam_value1)
    has_diam2 <- !is.na(diam_value2)
    has_diam3 <- !is.na(diam_value3)
    has_log1 <- !is.na(log_length1)
    has_log2 <- !is.na(log_length2)
    has_log3 <- !is.na(log_length3)

    # Création d'un identifiant de groupe unique pour chaque arbre
    # Cela permet de traiter les arbres individuellement tout en gardant un lien avec les données d'origine
    data_filtre[, group_id := .GRP, by = .(id_pe, no_arbre)]

    # Division de chaque arbre en sections de 2 pieds (convertis en mètres)
    # Pour chaque arbre (group_id), on crée une séquence de hauteurs, de la souche jusqu'à la hauteur totale
    data_all_sections <- data_filtre[, .(
      HT_REELLE_M = seq(dhs, HAUTEUR_M, by = section_metre)
    ), by = group_id]

    # Jointure pour récupérer les informations nécessaires de l'arbre pour chaque section
    # On garde uniquement les colonnes nécessaires au calcul du diamètre
    data_all_sections <- data_all_sections[data_filtre,
                                           on = "group_id",
                                           .(essence, group_id, id_pe, no_arbre, sdom_bio, cl_drai, veg_pot, DHP_Ae,
                                             HAUTEUR_M, nbTi_ha, st_ha, ALTITUDE, HT_REELLE_M)]

    # Calcul du diamètre prédit à chaque hauteur de section pour tous les arbres
    # Utilise la fonction get_diam qui applique des modèles de défilement
    diam_all_sections <- get_diam(data_all_sections)

    # Conversion du diamètre prédit de mm² en mètres
    # pred_mm2_corr est en mm², on prend la racine carrée puis on convertit en mètres
    data_all_sections[, DIAM_PREDICT := diam_all_sections[, sqrt(pred_mm2_corr) / ratio_mm_metre]]

    # Extraction des colonnes essentielles pour l'algorithme de sélection des billes
    data_treatment <- data_all_sections[, .(
      id_pe = id_pe,
      no_arbre = no_arbre,
      HT_REELLE_M = HT_REELLE_M,
      DIAM_PREDICT = DIAM_PREDICT
    ), by = group_id]

    # Calcul du volume pour chaque section d'arbre
    # - diam_hauteur_fb: diamètre au fin bout (à l'extrémité de la section)
    # - volume_section: volume de chaque section individuelle
    # - volume_cumulatif: somme cumulée des volumes depuis le début
    data_treatment[, c("diam_hauteur_fb", "volume_section", "volume_cumulatif") :=
                     {
                       # Décalage du vecteur DIAM_PREDICT pour obtenir le diamètre fin bout
                       diam_fb <- c(DIAM_PREDICT[-1], NA)

                       # Calcul du volume de chaque section (formule de tronc de cône)
                       vol_sec <- ifelse(!is.na(diam_fb),
                                         (pi * section_metre * factor * (DIAM_PREDICT^2 + diam_fb^2)) / scale,
                                         NA_real_)

                       # Création de la somme cumulée des volumes(ne reset pas lors du changement d'arbre, il ajoute
                       #le volume de la bille suivante au total des précédents)
                       vol_cum <- c(0, cumsum(ifelse(is.na(vol_sec), 0, vol_sec))[-length(vol_sec)])

                       list(diam_fb, vol_sec, vol_cum)
                     }]

    # Création d'un vecteur pour stocker les longueurs spéciales
    # Ce vecteur est utilisé quand l'utilisateur spécifie un diamètre minimal sans longueur
    #special_lengths_vector <- numeric(length = max(data_treatment$group_id))
    #names(special_lengths_vector) <- 1:max(data_treatment$group_id)

    # Création de variables booléennes pour simplifier les conditions
    # has_set: a à la fois le diamètre ET la longueur spécifiés
    # no_log: a le diamètre spécifié MAIS PAS la longueur
    has_set1 <- has_diam1 && has_log1
    has_set2 <- has_diam2 && has_log2
    has_set3 <- has_diam3 && has_log3

    no_log1 <- has_diam1 && !has_log1
    no_log2 <- has_diam2 && !has_log2
    no_log3 <- has_diam3 && !has_log3

    # PARTIE CRITIQUE: Algorithme de découpe des arbres en billes
    # Cette section détermine comment chaque arbre sera découpé en billes commerciales
    cut_positions <- data_treatment[, {
      # Ne collecte que les positions des coupes valides
      positions <- integer()
      grade_types <- character()  # Vecteur pour stocker le type de grade pour chaque coupe
      # Suppression de next_vols de cette section

      # Définition des variables locales pour un accès plus facile
      dp <- DIAM_PREDICT
      vc <- volume_cumulatif
      ht <- HT_REELLE_M
      n <- .N
      i <- 1
      j <- 0

      # Drapeau nécessaire pour ajuster les indices des cas spéciaux consécutifs
      special_case1_accessed <- FALSE
      special_case2_accessed <- FALSE

      # Pré-calcul des diamètres suivants pour les grades de longueur fixe
      next_diam1 <- if(has_set1) {
        c(dp[(1 + jump_log1):n], rep(NA, jump_log1))
      } else {
        rep(NA, n)
      }
      next_diam2 <- if(has_set2) {
        c(dp[(1 + jump_log2):n], rep(NA, jump_log2))
      } else {
        rep(NA, n)
      }
      next_diam3 <- if(has_set3) {
        c(dp[(1 + jump_log3):n], rep(NA, jump_log3))
      } else {
        rep(NA, n)
      }

      while(i <= n) {
        # Détermine la position actuelle à utiliser (ajustée si un cas spécial a été détecté)
        current_pos <- if(special_case1_accessed || special_case2_accessed) i - 1 else i

        # Ajoute la position aux positions de coupe
        positions <- c(positions, current_pos)

        # Réinitialise les drapeaux
        special_case1_accessed <- FALSE
        special_case2_accessed <- FALSE

        # Vérifie si l'un des critères de grade est rempli et avance en conséquence
        if(has_set1 && !is.na(next_diam1[i]) && diam_value1 < next_diam1[i]) {
          # Grade 1 de longueur fixe
          grade_types <- c(grade_types, nom_grade1)
          next_i <- i + jump_log1
          i <- next_i
        }
        else if(has_set2 && !is.na(next_diam2[i]) && diam_value2 < next_diam2[i]) {
          # Grade 2 de longueur fixe
          grade_types <- c(grade_types, nom_grade2)
          next_i <- i + jump_log2
          i <- next_i
        }
        else if(has_set3 && !is.na(next_diam3[i]) && diam_value3 < next_diam3[i]) {
          # Grade 3 de longueur fixe
          grade_types <- c(grade_types, nom_grade3)
          next_i <- i + jump_log3
          i <- next_i
        }
        # CAS SPÉCIAUX: Gestion des types de billes avec diamètre spécifié mais sans longueur fixe
        else if(no_log1 && diam_value1 < dp[i] && !is.na(dp[i])) {
          #Ajustement du drapeau
          special_case1_accessed <- TRUE
          # Grade 1 de longueur variable
          grade_types <- c(grade_types, nom_grade1)

          # Trouve où le diamètre passe en dessous du seuil
          drop_positions <- which(dp[(i+1):n] < diam_value1)
          if(length(drop_positions) > 0) {
            j <- i + drop_positions[1] - 1  # Dernière position où le diamètre est encore valide
          } else {
            j <- n  # Utilise toute la longueur restante
          }
          i <- j + 1
        }
        else if(no_log2 && diam_value2 < dp[i] && !is.na(dp[i])) {
          special_case2_accessed <- TRUE
          grade_types <- c(grade_types, nom_grade2)

          drop_positions <- which(dp[(i+1):n] < diam_value2)
          if(length(drop_positions) > 0) {
            j <- i + drop_positions[1] - 1
          } else {
            j <- n
          }

          i <- j + 1
        }
        else if(no_log3 && diam_value3 < dp[i] && !is.na(dp[i])) {
          grade_types <- c(grade_types, nom_grade3)

          drop_positions <- which(dp[(i+1):n] < diam_value3)
          if(length(drop_positions) > 0) {
            j <- i + drop_positions[1] - 1
          } else {
            j <- n
          }

          # Suppression de la ligne next_vols
          i <- j + 1
        }
        else {
          # Plus de segments valides ne peuvent être extraits
          grade_types <- c(grade_types, NA)  # Étiquette par défaut pour les coupes non classées
          # Suppression de la ligne next_vols
          break
        }
      }

      # Retourne les positions où les coupes doivent se produire et leurs types de grade
      .(cut_positions = positions, grade_type = grade_types)
    }, by = group_id]

    # Création d'un index pour chaque ligne dans son groupe respectif
    data_treatment[, row_within_group := seq_len(.N), by = group_id]

    # Transformation de cut_positions en format long
    cut_positions_long <- cut_positions[, .(
      group_id = group_id,
      row_within_group = cut_positions,
      grade_type = grade_type
    )]

    # Jointure en une seule opération pour extraire uniquement les lignes nécessaires
    cuts_data <- data_treatment[cut_positions_long, on = .(group_id, row_within_group)]

    # Suppression de la colonne temporaire
    cuts_data[, row_within_group := NULL]

    # Tri par group_id pour assurer l'ordre correct pour les opérations suivantes
    setkey(cuts_data, group_id)

    # Utilisation des opérations data.table pour calculer les longueurs, diamètres, volumes
    cuts_data[, `:=`(
      # Calcul des informations de position suivante en utilisant shift() avec group_id
      next_height = shift(HT_REELLE_M, -1, type="lag"),
      next_diam = shift(DIAM_PREDICT, -1, type="lag"),
      next_vol = shift(volume_cumulatif, -1, type="lag")
    ), by = group_id]

    # Calcul des propriétés des billes
    cuts_data[, `:=`(
      next_log_length = next_height - HT_REELLE_M,
      diam_hauteur_fb = next_diam,
      next_vol_bille = next_vol - volume_cumulatif
    )]

    # Calcul des propriétés des billes
    cuts_data[, `:=`(
      next_log_length = next_height - HT_REELLE_M,
      diam_hauteur_fb = next_diam
    )]

    # Utilisation de grade_type pour créer des colonnes supplémentaires
    cuts_data[, `:=`(
      grade_name = grade_type  # Utilisation directe de la colonne grade_type
    )]

    # Suppression des lignes contenant des valeurs NA
    cuts_data <- na.omit(cuts_data)

    # Création de la colonne diam_fb_cm basée sur le type de grade
    cuts_data[, diam_fb_cm := {
      # Valeur par défaut (ne sera jamais utilisée en pratique)
      result <- NA_real_

      # Attribution des valeurs de diamètre en fonction de la correspondance entre grade_type et nom_grade
      if (!is.null(nom_grade1) && !is.na(nom_grade1) && nchar(nom_grade1) > 0) {
        result <- fcase(
          grade_type == nom_grade1, diam_value1,
          default = result
        )
      }

      if (!is.null(nom_grade2) && !is.na(nom_grade2) && nchar(nom_grade2) > 0) {
        result <- fcase(
          grade_type == nom_grade2, diam_value2,
          default = result
        )
      }

      if (!is.null(nom_grade3) && !is.na(nom_grade3) && nchar(nom_grade3) > 0) {
        result <- fcase(
          grade_type == nom_grade3, diam_value3,
          default = result
        )
      }

      result
    }]

    # Création de la colonne long_bille_pied basée sur le type de grade
    cuts_data[, long_bille_pied := {
      # Valeur par défaut
      result <- NA_real_

      # Attribution des valeurs de longueur en fonction de la correspondance entre grade_type et nom_grade
      # et seulement si la valeur long_grade correspondante n'est pas nulle/NA
      if (!is.null(nom_grade1) && !is.na(nom_grade1) && nchar(nom_grade1) > 0) {
        if (has_log1) {
          result <- fcase(
            grade_type == nom_grade1, log_length1,
            default = result
          )
        } else {
          result <- fcase(
            grade_type == nom_grade1, NA_real_,
            default = result
          )
        }
      }

      if (!is.null(nom_grade2) && !is.na(nom_grade2) && nchar(nom_grade2) > 0) {
        if (has_log2) {
          result <- fcase(
            grade_type == nom_grade2, log_length2,
            default = result
          )
        } else {
          result <- fcase(
            grade_type == nom_grade2, NA_real_,
            default = result
          )
        }
      }

      if (!is.null(nom_grade3) && !is.na(nom_grade3) && nchar(nom_grade3) > 0) {
        if (has_log3) {
          result <- fcase(
            grade_type == nom_grade3, log_length3,
            default = result
          )
        } else {
          result <- fcase(
            grade_type == nom_grade3, NA_real_,
            default = result
          )
        }
      }

      result
    }]

    #On crée une table temporaire avec les colonnes nécessaires à la table finale
    final_join <- unique(data_filtre[, .(group_id, DHP_Ae, HAUTEUR_M)])
    setkey(final_join, group_id)  # Set key for faster joins

    #On fait un join avec les colonnes DHP_Ae et HAUTEUR_M pour la table finale
    cuts_data[final_join, `:=`(
      DHP_Ae = i.DHP_Ae,
      HAUTEUR_M = i.HAUTEUR_M
    ), on = "group_id"]

    # Nettoie les données et garde seulement les colonnes essentielles
    cuts_data[, `:=`(
      id_pe = id_pe,
      no_arbre = no_arbre,
      dhpcm = DHP_Ae,
      ht = HAUTEUR_M,
      volume = next_vol_bille,
      grade_bille = grade_type,
      diam_fb_cm = diam_fb_cm,
      long_bille_pied = long_bille_pied
    )]

    # Filtre les résultats finaux:
    # - Élimine les lignes avec valeurs manquantes
    # - Algorithme crée une donnée non utilisable(billes de volume 0 dans cas spécial), on doit la retirer
    # - Ajuste avec des valeurs de conversions différentes colonnes pour obtenir les bons résultats à l'écran
    # - Remultiplie les volumes par le facteur d'échelle
    data_billes <- cuts_data[!is.na(volume) & !is.na(id_pe) & !is.na(no_arbre) & !is.na(DHP_Ae) & !is.na(ht)
                             & !is.na(grade_bille) & !is.na(diam_fb_cm) & volume > 0,
                             .(id_pe, no_arbre, dhpcm = dhpcm / ratio_cm_mm , ht, vol_bille_dm3 = volume * scale,
                               grade_bille, diam_fb_cm = diam_fb_cm * ratio_cm_metre, long_bille_pied)]
  }
  else{
    #Retourne une data.table vide puisque aucune essence n'existe ou n'est valide
    return(data_billes)
  }
  # Retourne la table finale des billes avec leurs volumes
  return(data_billes)
}

##################################################
#tic()
#calcul_vol_bille(data_diam7)
#toc()
