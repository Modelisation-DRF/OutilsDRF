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
#' @param mode_simul Mode de simulation: "DET" (déterministe) ou "STO" (stochastique).
#'   Par défaut "DET".
#' @param seed_value Valeur de la graine pour la génération de nombres aléatoires.
#'   Si NULL, aucune graine n'est définie. Par défaut NULL.
#' @param modifier Modulateur de probabilité. Peut être:
#'   \itemize{
#'     \item Une valeur numérique entre -80 et 160 (s'applique à toutes les essences)
#'     \item Un data.frame à 2 colonnes: essence et modifier (modulation par essence)
#'     \item NULL (aucune modulation)
#'   }
#'
#' @return data.table. Table contenant les données d'arbres avec leur probabilité de coupe
#'   et, en mode stochastique, une décision de coupe (OUI/NON).
#'
#' @examples
#' \dontrun{
#' # Modulation uniforme
#' resultats1 <- prob_coupe(mes_arbres, trt_coupe = 5, modifier = 20)
#'
#' # Modulation par essence
#' mod_essence <- data.frame(essence = c("SAB", "EPN"), modifier = c(30, -50))
#' resultats2 <- prob_coupe(mes_arbres, trt_coupe = 5, modifier = mod_essence)
#' }
#'
#' @import data.table
#' @export
prob_coupe <- function(data_tree, trt_coupe, mode_simul="DET", seed_value=NULL, modifier = NULL) {

  #Conditions à respecter pour la fonction:
  if (trt_coupe < 0 || trt_coupe > 18) stop("Erreur: Le traitement de coupe doit être entre 0 et 18.")

  if (mode_simul != "DET" && mode_simul != "STO") stop("Erreur: Le mode de simulation doit être DET(déterministe) ou STO(stochastique).")

  # Validation du paramètre modifier
  if (!is.null(modifier)) {
    if (is.numeric(modifier) && length(modifier) == 1) {
      # Modulation uniforme
      if (modifier < -80 || modifier > 160) stop("Erreur: Le modifier doit être entre -80 et 160.")
    } else if (is.data.frame(modifier)) {
      # Modulation par essence
      if (!all(c("essence", "modifier") %in% names(modifier))) {
        stop("Erreur: Le data.frame modifier doit contenir les colonnes 'essence' et 'modifier'.")
      }
      if (any(modifier$modifier < -80 | modifier$modifier > 160)) {
        stop("Erreur: Toutes les valeurs de modifier doivent être entre -80 et 160.")
      }
    } else {
      stop("Erreur: Le paramètre modifier doit être soit un nombre, soit un data.frame, soit NULL.")
    }
  }

  if(!is.null(seed_value)) {
    set.seed(seed_value)
  }

  nom_var <- names(data_tree)

  # Générer les paramètres de l'équation
  setDT(data_tree)
  data_param <- param_coupe(trt_coupe, mode_simul, nb_iter=1, seed_value = seed_value)
  setDT(data_param)
  setnames(data_param, "essence", "essence_coupe")

  # Ajouter le numéro de traitement aux données d'arbres
  temp_table <- data_tree[, num_trt := trt_coupe]

  # Première fusion avec les associations d'essences
  data_mid_table <- merge(temp_table, coupe_ass_ess, by = c("num_trt", "essence"),
                          all.x = TRUE)

  # Deuxième fusion avec les paramètres par itération
  data_full_table <- merge(data_mid_table, data_param,
                           by = c("num_trt", "essence_coupe", "code_trt"),
                           all.x = TRUE)

  # Calculer XB en utilisant l'équation
  data_full_table[, d := dhpcm - 23]
  data_full_table[, m := as.numeric(dhpcm > 23)]
  data_full_table[, N := nbTi_ha]
  data_full_table[, BA := st_ha]

  # Calculer XB en utilisant la formule
  data_full_table[, XB := b0 + b1_s + (b2_s + b3_s * m) * d +
                    (b4_s + b5_s * m) * d^2 + b6_s * log(N + 1) + b7_s * BA]

  # Calculer la probabilité de base
  data_full_table[, prob_coupe := exp(XB) / (1 + exp(XB))]

  # Appliquer la modulation selon le type de modifier
  if (!is.null(modifier)) {

    if (is.numeric(modifier) && length(modifier) == 1) {
      # Modulation uniforme pour toutes les essences
      modifier_factor <- 1 + modifier / 100
      data_full_table[, prob_coupe := modifier_factor * prob_coupe]

    } else if (is.data.frame(modifier)) {
      # Modulation par essence
      setDT(modifier)

      # Fusionner avec les modifiers par essence
      data_full_table <- merge(data_full_table, modifier,
                               by = "essence", all.x = TRUE)

      # Appliquer la modulation seulement aux essences spécifiées
      data_full_table[!is.na(modifier),
                      prob_coupe := (1 + modifier / 100) * prob_coupe]

      # Supprimer la colonne modifier temporaire
      data_full_table[, modifier := NULL]
    }
  }

  # S'assurer que les probabilités restent dans [0, 1]
  data_full_table[, prob_coupe := fifelse(prob_coupe > 1, 1,
                                          fifelse(prob_coupe < 0, 0, prob_coupe))]

  # Si mode stochastique, déterminer la coupe
  if(mode_simul == "STO"){
    data_full_table[, nb_alea := runif(.N)]
    data_full_table[, COUPE := ifelse(prob_coupe > nb_alea, "OUI", "NON")]
  }

  # Réarranger les données en ordre id_pe et no_arbre
  setorder(data_full_table, id_pe, no_arbre)

  # Sélectionner les colonnes à retourner
  if(mode_simul == "DET"){
    cols_to_keep <- c(nom_var, "prob_coupe")
  } else {
    cols_to_keep <- c(nom_var, "prob_coupe", "COUPE")
  }

  result <- data_full_table[, ..cols_to_keep]
  return(result)
}

##############################################################

#test_ess_df <- mod_essence <- data.frame(
#  essence = c("BOP", "EPB", "BOJ"),
#  modifier = c(20, -50, 10)
#)

#prob_coupe(data_tree1, 5, "DET", 123, test_ess_df)

#prob_coupe(data_tree2, 12, "STO", 123, -60)

library()
