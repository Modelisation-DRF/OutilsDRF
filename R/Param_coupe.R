#' Calcule la probabilité de coupe des arbres selon un traitement donné
#'
#' @description
#' Cette fonction calcule la probabilité qu'un arbre soit coupé en fonction d'un traitement sylvicole
#' et de ses caractéristiques. Elle peut fonctionner en mode déterministe ou stochastique.
#'
#' @param data_tree data.table. Table des données d'arbres ayant les colonnes suivantes
#'   \itemize{
#'     \item essence - Essence de l'arbre en majuscule, ex: SAB
#'     \item id_pe - Identifiant de la placette
#'     \item no_arbre - Numéro de l'arbre dans la placette
#'     \item dhpcm - Diamètre à hauteur de poitrine de l'arbre (en cm)
#'     \item nbTi_ha - Nombre d'arbres à l'ha dans la placette
#'     \item st_ha - Surface terrière en m2/ha dans la placette
#'   }
#' @param trt_coupe Le numéro de traitement de coupe, un entier entre 0 et 18
#' \itemize{
#'   \item 0: Coupe d'amélioration
#'   \item 1: Coupe d'éclaircie
#'   \item 2: Coupe de jardinage avant 1997
#'   \item 3: Coupe de jardinage 1997-2004 Cerf
#'   \item 4: Coupe de jardinage 1997-2004
#'   \item 5: Coupe de jardinage après 2004 Cerf
#'   \item 6: Coupe de jardinage après 2004
#'   \item 7: Coupe progressive
#'   \item 8: Éclaircie commerciale
#'   \item 9: Éclaircie sélective
#'   \item 10: Coupe partielle 35% chantier forêt feuillue R-06
#'   \item 11: Coupe partielle 45% chantier forêt feuillue R-06
#'   \item 12: CPI_CP Outaouais
#'   \item 13: CPI_RL Outaouais
#'   \item 14: CRS Outaouais
#'   \item 15: Coupe jardinage CIMOTFF
#'   \item 16: Coupe de jardinage par groupe d'arbres CIMOTFF
#'   \item 17: Coupe progressive irrégulière couvert parmanent CIMOTFF
#'   \item 18: Coupe progressive irrégulière à régénération lente CIMOTFF
#'   }
#' @param mode_simul Mode de simulation: "DET" (déterministe) ou "STO" (stochastique).
#'   Par défaut "DET".
#' @param nb_iter Nombre d'itérations pour la simulation. Par défaut 1.
#' @param seed_value Valeur de la graine pour la génération de nombres aléatoires.
#'   Si NULL, aucune graine n'est définie. Par défaut NULL.
#'
#' @return data.table. Table contenant les données d'arbres avec leur probabilité de coupe
#'   et, en mode stochastique, une décision de coupe (OUI/NON).
#'
#' @details
#' La fonction fusionne les données d'arbres avec les paramètres du traitement choisi
#' et calcule la probabilité de coupe à l'aide d'un modèle logistique.
#' En mode stochastique, la fonction génère une variable aléatoire pour chaque arbre
#' et détermine si l'arbre est coupé ou non.
#'
#' La fonction dépend de variables globales:
#' \itemize{
#'   \item coupe_ass_ess - Liste des famille d'essences valides
#' }
#'
#' @examples
#' \dontrun{
#' # Mode déterministe
#' resultats_det <- prob_coupe(mes_arbres, trt_coupe = 5)
#'
#' # Mode stochastique avec 10 itérations
#' resultats_sto <- prob_coupe(mes_arbres, trt_coupe = 5, mode_simul = "STO",
#'                             nb_iter = 10, seed_value = 123)
#' }
#'
#' @import data.table
#' @export
prob_coupe <- function(data_tree, trt_coupe, mode_simul="DET", nb_iter=1, seed_value=NULL) {

  #Conditions à respecter pour la fonction:
  if (trt_coupe < 0 || trt_coupe > 18) stop("Erreur: Le traitement de coupe doit être entre 0 et 18.")

  if (mode_simul != "DET" && mode_simul != "STO") stop("Erreur: Le mode de simulation doit être DET(déterministe) ou
                                                       STO(stochastique).")

  if (mode_simul == "STO" && nb_iter < 2) stop("Erreur: Lorsque le mode stochastique est choisi, le nombre d'itération doit
                                               être de 2 minimum.")

  if (mode_simul == "DET" && nb_iter != 1) stop("Erreur: Lorsque le mode déterministe est choisi, le nombre d'itération doit
                                               être de 1.")

  if(!is.null(seed_value)) {
    set.seed(seed_value)
  }

  setDT(data_tree)
  data_param <- param_coupe(trt_coupe, mode_simul, nb_iter)
  setDT(data_param)
  setnames(data_param, "essence", "essence_coupe")

  # Ajouter le numéro de traitement aux données d'arbres
  temp_table <- data_tree[, num_trt := trt_coupe]

  # Stocker les arbres originaux avant les fusions
  data_arbre_complet <- copy(temp_table)

  # Première fusion avec les associations d'essences
  # Ajout de allow.cartesian = TRUE pour permettre les produits cartésiens
  data_mid_table <- merge(temp_table, coupe_ass_ess, by = c("num_trt", "essence"),
                          all.x = FALSE, allow.cartesian = TRUE)

  # Deuxième fusion avec les paramètres par itération
  # Ajout de allow.cartesian = TRUE pour permettre les produits cartésiens
  data_full_table <- merge(data_mid_table, data_param,
                           by = c("num_trt", "essence_coupe", "code_trt"),
                           allow.cartesian = TRUE)

  # Identifier les arbres perdus en comparant l'original aux données fusionnées finales
  arbre_non_valide <- data_arbre_complet[!paste(id_pe, no_arbre) %in%
                                           paste(data_full_table$id_pe, data_full_table$no_arbre)]

  # Si nous avons perdu des arbres et qu'il y a des itérations
  if(nrow(arbre_non_valide) > 0 && length(unique(data_param$iter)) > 0) {
    # Obtenir les itérations uniques
    all_iters <- unique(data_param$iter)
    # Créer une liste pour stocker les copies des arbres perdus pour chaque itération
    arbre_non_valide_list <- vector("list", length(all_iters))
    # Pour chaque itération, créer une copie des arbres perdus
    for(i in seq_along(all_iters)) {
      iter_val <- all_iters[i]
      arbre_non_valide_copy <- copy(arbre_non_valide)
      arbre_non_valide_copy[, iter := iter_val]
      # Ajouter des valeurs par défaut pour les colonnes de paramètres
      param_cols <- setdiff(names(data_full_table), names(arbre_non_valide_copy))
      for(col in param_cols) {
        if(col %in% c("b0", "b1_s", "b2_s", "b3_s", "b4_s", "b5_s", "b6_s", "b7_s")) {
          arbre_non_valide_copy[, (col) := 0]
        } else {
          arbre_non_valide_copy[, (col) := NA]
        }
      }
      arbre_non_valide_list[[i]] <- arbre_non_valide_copy
    }
    # Combiner tous les arbres perdus (maintenant avec des itérations) en un seul data.table
    arbre_non_valide <- rbindlist(arbre_non_valide_list, fill = TRUE)
  }

  # Calculer XB en utilisant l'équation
  data_full_table[, d := dhpcm - 23]       # d = dhp - 23
  data_full_table[, m := as.numeric(dhpcm > 23)]  # m = 1 si dhp > 23, 0 sinon
  data_full_table[, N := nbTi_ha]           # N = nombre d'arbres par ha
  data_full_table[, BA := st_ha]            # BA = surface terrière (m²/ha)

  # Calculer XB en utilisant la formule
  data_full_table[, XB := b0 + b1_s + (b2_s + b3_s * m) * d +
                    (b4_s + b5_s * m) * d^2 + b6_s * log(N + 1) + b7_s * BA]

  # Calculer la probabilité de coupe
  data_full_table[, prob_coupe := exp(XB) / (1 + exp(XB))]

  #Si mode stochastique, on transforme le paramètre pour la coupe à "OUI" ou "NON"
  if(mode_simul == "STO"){
    data_full_table[, nb_alea := runif(.N)]  # Générer un nombre aléatoire pour chaque ligne
    data_full_table[, COUPE := ifelse(prob_coupe > nb_alea, "OUI", "NON")]  # Déterminer l'état de coupe
  }
  # Combiner arbres valides et non-valides
  all_trees <- rbindlist(list(data_full_table, arbre_non_valide), fill = TRUE)

  # Réarranger les données en ordre id_pe et no_arbresélectionn
  setorder(all_trees, id_pe, no_arbre)

  if(mode_simul == "DET"){
    # Sélectionner uniquement les colonnes désirées pour le mode déterministe
    cols_to_keep <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "prob_coupe")
  }

  if(mode_simul == "STO"){
    # Sélectionner uniquement les colonnes désirées pour le mode déterministe
    cols_to_keep <- c("id_pe", "no_arbre", "essence", "dhpcm", "st_ha", "nbTi_ha", "num_trt", "COUPE")
  }

  # Conserver uniquement les colonnes spécifiées
  result <- all_trees[, ..cols_to_keep]

  return(result)
}

##############################################################

#prob_coupe(data_tree1, 5, "DET")

#prob_coupe(data_tree2, 12, "STO", 5, 123)

