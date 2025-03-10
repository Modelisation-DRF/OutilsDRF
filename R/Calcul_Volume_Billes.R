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
#'     \item grade - Type (grade) de la bille
#'     \item volume - Volume de la bille en dm³
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
  # Ces ratios permettent de convertir différentes unités en mètres (unité de travail interne)
  ratio_pouce_metre <- 39.3700787  # Nombre de pouces dans un mètre
  ratio_cm_mm <- 10
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
    #print("all_sections done")

    # Jointure pour récupérer les informations nécessaires de l'arbre pour chaque section
    # On garde uniquement les colonnes nécessaires au calcul du diamètre
    data_all_sections <- data_all_sections[data_filtre,
                                           on = "group_id",
                                           .(essence, group_id, id_pe, no_arbre, sdom_bio, cl_drai, veg_pot, DHP_Ae,
                                             HAUTEUR_M, nbTi_ha, st_ha, ALTITUDE, HT_REELLE_M)]

    # Calcul du diamètre prédit à chaque hauteur de section pour tous les arbres
    # Utilise la fonction get_diam qui applique des modèles de défilement
    #tic()
    diam_all_sections <- get_diam(data_all_sections)
    #toc()
    #print("get_diam done")

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
    #tic()
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
    #toc()
    #print("volume done")
    # Création d'un vecteur pour stocker les longueurs spéciales
    # Ce vecteur est utilisé quand l'utilisateur spécifie un diamètre minimal sans longueur
    special_lengths_vector <- numeric(length = max(data_treatment$group_id))
    names(special_lengths_vector) <- 1:max(data_treatment$group_id)

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
    all_cuts <- data_treatment[, {
      # Initialisation des vecteurs pour stocker les résultats
      # - result: indices des positions de coupe
      # - next_lengths: longueurs des billes
      # - next_diams: diamètres fin bout des billes
      # - next_vols: volumes des billes
      # On travaille toujours avec la bille suivante et c'est les valeurs de celle-ci qu'on ajoute
      result <- numeric()
      next_lengths <- numeric()
      next_diams <- numeric()
      next_vols <- numeric()

      # Définition d'abréviations pour simplifier le code
      dp <- DIAM_PREDICT        # Diamètre prédit à chaque hauteur
      vc <- volume_cumulatif    # Volume cumulatif à chaque hauteur
      ht <- HT_REELLE_M         # Hauteur réelle de chaque section
      n <- .N                   # Nombre de sections pour cet arbre
      j <- 0                    # Variable temporaire pour les cas spéciaux

      # Pré-calcul des diamètres après sauts pour chaque type de bille
      # Ceci permet d'évaluer rapidement si une bille potentielle respecte les critères de diamètre
      next_diam1 <- if(has_set1) {
        # Décale le vecteur de diamètres par la longueur de la bille (en nombre de sections)
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

      # Début de l'algorithme de découpe des billes
      i <- 1  # Position de départ

      while(i <= n) {
        # Enregistre la position courante comme point de coupe potentiel
        result <- c(result, i)

        # Vérifie si une bille de type 1 peut être découpée à partir de cette position
        if(has_set1 && !is.na(next_diam1[i]) && diam_value1 < next_diam1[i]) {
          # Si le diamètre au bout de la bille potentielle est supérieur au minimum requis
          next_i <- i + jump_log1  # Calcule la position après la bille

          # Enregistre les informations de cette bille
          next_lengths <- c(next_lengths, ht[next_i] - ht[i])  # Longueur de la bille
          next_diams <- c(next_diams, dp[next_i])              # Diamètre fin bout
          next_vols <- c(next_vols, vc[next_i] - vc[i])        # Volume de la bille

          i <- next_i  # Avance à la position après cette bille
        }
        # Vérifications similaires pour les billes de type 2 et 3
        else if(has_set2 && !is.na(next_diam2[i]) && diam_value2 < next_diam2[i]) {
          next_i <- i + jump_log2
          next_lengths <- c(next_lengths, ht[next_i] - ht[i])
          next_diams <- c(next_diams, dp[next_i])
          next_vols <- c(next_vols, vc[next_i] - vc[i])
          i <- next_i
        }
        else if(has_set3 && !is.na(next_diam3[i]) && diam_value3 < next_diam3[i]) {
          next_i <- i + jump_log3
          next_lengths <- c(next_lengths, ht[next_i] - ht[i])
          next_diams <- c(next_diams, dp[next_i])
          next_vols <- c(next_vols, vc[next_i] - vc[i])
          i <- next_i
        }
        # CAS SPÉCIAUX: Gestion des types de billes avec diamètre spécifié mais sans longueur fixe
        # Dans ce cas, on cherche la plus longue bille possible respectant le diamètre minimal
        else if(no_log1 && diam_value1 < dp[i] && !is.na(dp[i])) {
          # Trouve toutes les positions sur l'arbre où le diamètre dépasse le minimum requis
          next_positions <- which(dp[i:n] > diam_value1)

          # Détermine la position la plus élevée respectant le critère
          if(length(next_positions) > 0) {
            j <- i + next_positions[length(next_positions)]
          } else {
            j <- n
          }

          # Calcule la longueur de cette bille spéciale
          total_jump <- (j - i) - 1
          special_length <- section_metre * total_jump

          # Stocke cette longueur pour référence future
          special_lengths_vector[.BY$group_id] <<- special_length

          # Enregistre les informations de cette bille
          next_lengths <- c(next_lengths, special_length)
          next_diams <- c(next_diams, dp[j - 1])
          next_vols <- c(next_vols, vc[j - 1] - vc[i])

          i <- j  # Avance à la position après cette bille(il n'y aura plus aucune bille valide)
        }
        # Cas spéciaux similaires pour les types 2 et 3
        else if(no_log2 && diam_value2 < dp[i] && !is.na(dp[i])) {
          next_positions <- which(dp[i:n] > diam_value2)
          if(length(next_positions) > 0) {
            j <- i + next_positions[length(next_positions)]
          } else {
            j <- n
          }

          total_jump <- (j - i) - 1
          special_length <- section_metre * total_jump

          special_lengths_vector[.BY$group_id] <<- special_length
          next_lengths <- c(next_lengths, special_length)
          next_diams <- c(next_diams, dp[j - 1])
          next_vols <- c(next_vols, vc[j - 1] - vc[i])
          i <- j
        }
        else if(no_log3 && diam_value3 < dp[i] && !is.na(dp[i])) {
          next_positions <- which(dp[i:n] > diam_value3)
          if(length(next_positions) > 0) {
            j <- i + next_positions[length(next_positions)]
          } else {
            j <- n
          }

          total_jump <- (j - i) - 1
          special_length <- section_metre * total_jump

          special_lengths_vector[.BY$group_id] <<- special_length
          next_lengths <- c(next_lengths, special_length)
          next_diams <- c(next_diams, dp[j - 1])
          next_vols <- c(next_vols, vc[j - 1] - vc[i])
          i <- j
        }
        else {
          # Aucun type de bille ne peut être découpé à partir de cette position
          # On ajoute des NA et on arrête la découpe pour cet arbre
          next_lengths <- c(next_lengths, NA)
          next_diams <- c(next_diams, NA)
          next_vols <- c(next_vols, NA)
          break
        }
      }

      # Retourne les résultats de la découpe pour cet arbre
      .(
        id_pe = id_pe[result],           # Identifiant de placette
        no_arbre = no_arbre[result],     # Numéro d'arbre
        HT_REELLE_M = HT_REELLE_M[result], # Hauteur de la coupe
        DIAM_PREDICT = DIAM_PREDICT[result], # Diamètre à la hauteur de la coupe
        next_log_length = next_lengths,   # Longueur de la bille
        diam_hauteur_fb = next_diams,     # Diamètre fin bout
        next_vol_bille = next_vols        # Volume de la bille
      )
    }, by = group_id]  # Effectue cette opération pour chaque arbre individuellement

    #Enlever les lignes avec des NA
    all_cuts <- na.omit(all_cuts)

    # CLASSIFICATION DES BILLES selon leur type (grade)
    # Plusieurs cas selon les combinaisons de paramètres spécifiés par l'utilisateur
    #tic()
    # CAS 1: Tous les types de billes ont diamètre ET longueur spécifiés
    if(has_set3 && has_set2 && has_set1) {
      # Conversion des longueurs en mètres (depuis des unités de billes)
      length1 <- log_length1 * 12.25 / ratio_pouce_metre
      length2 <- log_length2 * 12.25 / ratio_pouce_metre
      length3 <- log_length3 * 12.25 / ratio_pouce_metre

      # Identification des billes qui correspondent aux longueurs spécifiées
      # Une petite tolérance (1e-6) est utilisée pour gérer les erreurs d'arrondi
      meets_length1 <- abs(all_cuts$next_log_length - length1) < 1e-6
      meets_length2 <- abs(all_cuts$next_log_length - length2) < 1e-6
      meets_length3 <- abs(all_cuts$next_log_length - length3) < 1e-6

      # Identification des billes qui respectent les diamètres minimaux
      meets_diam1 <- all_cuts$diam_hauteur_fb >= diam_value1
      meets_diam2 <- all_cuts$diam_hauteur_fb >= diam_value2
      meets_diam3 <- all_cuts$diam_hauteur_fb >= diam_value3

      # Attribution du type (grade) à chaque bille en fonction des critères
      # fcase est une version optimisée de ifelse pour data.table
      all_cuts[, current_grade := fcase(
        meets_length1 & meets_diam1, nom_grade1,
        meets_length2 & meets_diam2, nom_grade2,
        meets_length3 & meets_diam3, nom_grade3
      )]
    }

    # CAS 2: Les types 1 et 2 ont diamètre ET longueur, le type 3 peut être incomplet
    else if(!has_set3 && has_set2 && has_set1) {
      length1 <- log_length1 * 12.25 / ratio_pouce_metre
      length2 <- log_length2 * 12.25 / ratio_pouce_metre

      meets_length1 <- abs(all_cuts$next_log_length - length1) < 1e-6
      meets_length2 <- abs(all_cuts$next_log_length - length2) < 1e-6
      meets_diam1 <- all_cuts$diam_hauteur_fb >= diam_value1
      meets_diam2 <- all_cuts$diam_hauteur_fb >= diam_value2

      # Sous-cas: Le type 3 a un nom mais pas de longueur fixe
      if(!is.null(nom_grade3) && length(nom_grade3) > 0) {
        all_cuts[, current_grade := fcase(
          meets_length1 & meets_diam1, nom_grade1,
          meets_length2 & meets_diam2, nom_grade2,
          default = nom_grade3  # Toutes les autres billes sont de type 3
        )]
      }
      # Sous-cas: Le type 3 n'existe pas
      else {
        all_cuts[, current_grade := fcase(
          meets_length1 & meets_diam1, nom_grade1,
          meets_length2 & meets_diam2, nom_grade2,
          default = NA_character_  # Les autres billes n'ont pas de type
        )]
      }
    }

    # CAS 3: Seul le type 1 a diamètre ET longueur, les autres peuvent être incomplets
    else if(!has_set3 && !has_set2 && has_set1) {
      length1 <- log_length1 * 12.25 / ratio_pouce_metre

      meets_length1 <- abs(all_cuts$next_log_length - length1) < 1e-6
      meets_diam1 <- all_cuts$diam_hauteur_fb >= diam_value1

      # Sous-cas: Le type 2 a un nom mais pas de longueur fixe
      if(!is.null(nom_grade2) && length(nom_grade2) > 0) {
        all_cuts[, current_grade := fcase(
          meets_length1 & meets_diam1, nom_grade1,
          default = nom_grade2  # Toutes les autres billes sont de type 2
        )]
      }
      # Sous-cas: Le type 2 n'existe pas
      else {
        all_cuts[, current_grade := fcase(
          meets_length1 & meets_diam1, nom_grade1,
          default = NA_character_  # Les autres billes n'ont pas de type
        )]
      }
    }

    # CAS 4: Aucun type n'a de longueur fixe (seulement des diamètres)
    else {
      # Dans ce cas, toutes les billes sont attribuées au type 1
      all_cuts[, current_grade := nom_grade1]
    }

    # ASSIGNATION DES LONGUEURS DE BILLES pour la table finale
    # Cette étape attribue la longueur correcte à chaque bille selon son type

    # CAS 1: Tous les types ont longueur fixe
    if(has_set3 && has_set2 && has_set1) {
      all_cuts[, current_log_length := fcase(
        current_grade == nom_grade1, log_length1 * 12.25/ ratio_pouce_metre,
        current_grade == nom_grade2, log_length2 * 12.25/ ratio_pouce_metre,
        current_grade == nom_grade3, log_length3 * 12.25/ ratio_pouce_metre
      )]
    }
    # CAS 2: Types 1 et 2 ont longueur fixe, type 3 peut varier
    else if(!has_set3 && has_set2 && has_set1) {
      if(!is.null(nom_grade3)) {
        # Type 3 existe mais sans longueur fixe
        all_cuts[, current_log_length := fcase(
          current_grade == nom_grade1, log_length1 * 12.25 / ratio_pouce_metre,
          current_grade == nom_grade2, log_length2 * 12.25 / ratio_pouce_metre,
          current_grade == nom_grade3, special_lengths_vector[group_id]  # Utilise la longueur spéciale calculée
        )]
      }
      else {
        # Type 3 n'existe pas
        all_cuts[, current_log_length := fcase(
          current_grade == nom_grade1, log_length1 * 12.25 / ratio_pouce_metre,
          current_grade == nom_grade2, log_length2 * 12.25 / ratio_pouce_metre,
          default = NA_real_
        )]
      }
    }
    # CAS 3: Seul le type 1 a longueur fixe
    else if(!has_set3 && !has_set2 && has_set1){
      if(!is.null(nom_grade2)) {
        # Type 2 existe mais sans longueur fixe
        all_cuts[, current_log_length := fcase(
          current_grade == nom_grade1, log_length1 * 12.25 / ratio_pouce_metre,
          current_grade == nom_grade2, special_lengths_vector[group_id]  # Utilise la longueur spéciale
        )]
      } else {
        # Type 2 n'existe pas
        all_cuts[, current_log_length := fcase(
          current_grade == nom_grade1, log_length1 * 12.25 / ratio_pouce_metre,
          default = NA_real_
        )]
      }
    }
    # CAS 4: Le type 1 n'a pas de longueur fixe
    else {
      # Utilise la longueur spéciale pour toutes les billes
      all_cuts[, current_log_length := special_lengths_vector[group_id]]
    }

    all_cuts[, diam_fb_cm := {
      # Valeur par défaut au cas(ne sera jamais utilisé techniquement)
      result <- NA_real_

      #On donne les valeurs nécessaires à la table finale, seulement si le type existe
      if (!is.null(nom_grade1) && !is.na(nom_grade1) && nchar(nom_grade1) > 0) {
        result <- fcase(
          current_grade == nom_grade1, diam_value1,
          default = result
        )
      }

      if (!is.null(nom_grade2) && !is.na(nom_grade2) && nchar(nom_grade2) > 0) {
        result <- fcase(
          current_grade == nom_grade2, diam_value2,
          default = result
        )
      }

      if (!is.null(nom_grade3) && !is.na(nom_grade3) && nchar(nom_grade3) > 0) {
        result <- fcase(
          current_grade == nom_grade3, diam_value3,
          default = result
        )
      }

      result
    }]

    all_cuts[, long_bille_pied := {
      #Avec indéfinie dans le cas ou nous n'avons pas de log_length, on doit transformer le log_length en char lorsqu'il
      #est présent, pour ne pas avoir d'erreur de typage
      result <- NA_real_
      indefinie <- NA_real_

      #On donne la longueur pour la table finale seulement si le type existe(et selon les cas spéciaux où log_length existe ou non)
      if (!is.null(nom_grade1) && !is.na(nom_grade1) && nchar(nom_grade1) > 0) {
        if(has_set1){
        result <- fcase(
          current_grade == nom_grade1, log_length1,
          default = result
        )}
        else{
          result <- fcase(
            current_grade == nom_grade1, NA_real_,
            default = result
        )}
      }

      if (!is.null(nom_grade2) && !is.na(nom_grade2) && nchar(nom_grade2) > 0) {
        if(has_set2){
          result <- fcase(
            current_grade == nom_grade2, log_length2,
            default = result
          )}
        else{
          result <- fcase(
            current_grade == nom_grade2, NA_real_,
            default = result
        )}
      }


      if (!is.null(nom_grade3) && !is.na(nom_grade3) && nchar(nom_grade3) > 0) {
        if(has_set3){
          result <- fcase(
            current_grade == nom_grade3, log_length3,
            default = result
          )}
        else{
          result <- fcase(
            current_grade == nom_grade3, NA_real_,
            default = result
        )}
      }

      result
    }]

    #On crée une table temporaire avec les colonnes nécessaires à la table finale
    final_join <- unique(data_filtre[, .(group_id, DHP_Ae, HAUTEUR_M)])
    setkey(final_join, group_id)  # Set key for faster joins

    #On fait un join avec les colonnes DHP_Ae et HAUTEUR_M pour la table finale
    all_cuts[final_join, `:=`(
      DHP_Ae = i.DHP_Ae,
      HAUTEUR_M = i.HAUTEUR_M
    ), on = "group_id"]

    # Nettoie les données et garde seulement les colonnes essentielles
    all_cuts[, `:=`(
      id_pe = id_pe,
      no_arbre = no_arbre,
      dhpcm = DHP_Ae,
      ht = HAUTEUR_M,
      volume = next_vol_bille,
      grade_bille = current_grade,
      diam_fb_cm = diam_fb_cm,
      long_bille_pied = long_bille_pied
    )]

    # Filtre les résultats finaux:
    # - Élimine les lignes avec valeurs manquantes
    # - Ajuste avec des valeurs de conversions différentes colonnes pour obtenir les bons résultats à l'écran
    # - Remultiplie les volumes par le facteur d'échelle
    data_billes <- all_cuts[!is.na(volume) & !is.na(id_pe) & !is.na(no_arbre) & !is.na(DHP_Ae) & !is.na(ht)
                            & !is.na(grade_bille) & !is.na(diam_fb_cm),
                            .(id_pe, no_arbre, dhpcm = dhpcm / ratio_cm_mm , ht, vol_bille_dm3 = volume * scale,
                              grade_bille, diam_fb_cm = diam_fb_cm * ratio_cm_metre, long_bille_pied)]
  #toc()
  #print("rest done")
  }
  else{
    #retourne une data.table vide puisque aucune essence n'existe ou n'est valide
    return(data_billes)
  }
  # Retourne la table finale des billes avec leurs volumes
  return(data_billes)
}

##################################################
#tic()
#calcul_vol_bille(data_diam7)
#toc()
