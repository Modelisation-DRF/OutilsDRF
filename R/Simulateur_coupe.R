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
#' @param seed_value Valeur de la graine pour la génération de nombres aléatoires.
#'   Si NULL, aucune graine n'est définie. Par défaut NULL.
#'
#' @param modifier Valeur de modulateur de probabilité entre -80 et 160. Par défaut 0.
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
#' resultats_sto <- prob_coupe(mes_arbres, trt_coupe = 8, mode_simul = "STO",
#'                             seed_value = 123, modifier = -50)
#' }
#'
#' @import data.table
#' @export
prob_coupe <- function(data_tree, trt_coupe, mode_simul="DET", seed_value=NULL, modifier = 0) {

  #Conditions à respecter pour la fonction:
  if (trt_coupe < 0 || trt_coupe > 18) stop("Erreur: Le traitement de coupe doit être entre 0 et 18.")

  if (mode_simul != "DET" && mode_simul != "STO") stop("Erreur: Le mode de simulation doit être DET(déterministe) ou
                                                       STO(stochastique).")

  if (modifier < -80 || modifier > 160) stop("Erreur: Le modifier doit être entre -80 et 160.")

  if(!is.null(seed_value)) {
    set.seed(seed_value)
  }

  nom_var <- names(data_tree)

  # Générer les paramètres de l'équation
  setDT(data_tree)
  data_param <- param_coupe(trt_coupe, mode_simul, nb_iter=1, seed_value = seed_value) # ici, toujours générer 1 seule itération, meme si mode stochastique, car on appliquera la coupe que sur une itérration à la fois, et un pas de simulation à la fois
  setDT(data_param)
  setnames(data_param, "essence", "essence_coupe")

  # Ajouter le numéro de traitement aux données d'arbres
  temp_table <- data_tree[, num_trt := trt_coupe]

  # Première fusion avec les associations d'essences
  # On garde tous les arbres de la table x, et il y aura des NA dans essence_coupe si essence n'est pas dans y
  # Plus besoin de gérer les arbres qu'on perd
  data_mid_table <- merge(temp_table, coupe_ass_ess, by = c("num_trt", "essence"),
                          all.x = TRUE)

  # Deuxième fusion avec les paramètres par itération
  # On garde les arbres de la table x, et ceux sans eq de coupe auront les paramètres à NA
  # Plus besoin de gérer les arbres qu'on perd
  data_full_table <- merge(data_mid_table, data_param,
                           by = c("num_trt", "essence_coupe", "code_trt"),  #Ne pas mettre iter dans le by, car on le fera toujours sur une seul iter à la fois
                           all.x = TRUE)

  # Calculer XB en utilisant l'équation
  data_full_table[, d := dhpcm - 23]       # d = dhp - 23
  data_full_table[, m := as.numeric(dhpcm > 23)]  # m = 1 si dhp > 23, 0 sinon
  data_full_table[, N := nbTi_ha]           # N = nombre d'arbres par ha
  data_full_table[, BA := st_ha]            # BA = surface terrière (m²/ha)

  # Calculer XB en utilisant la formule
  data_full_table[, XB := b0 + b1_s + (b2_s + b3_s * m) * d +
                    (b4_s + b5_s * m) * d^2 + b6_s * log(N + 1) + b7_s * BA]

  modifier_factor = 1

  modifier_factor = modifier_factor + modifier / 100
  if(modifier_factor != 1){
    # Calculer la probabilité de coupe avec le modulateur
    data_full_table[, prob_coupe := modifier_factor * (exp(XB) / (1 + exp(XB)))]
  }

  else{
    # Calculer la probabilité de coupe
    data_full_table[, prob_coupe := exp(XB) / (1 + exp(XB))]
  }

  data_full_table[, prob_coupe := fifelse(prob_coupe > 1, 1, fifelse(prob_coupe < 0, 0, prob_coupe))]

  #Si mode stochastique, on transforme le paramètre pour la coupe à "OUI" ou "NON"
  if(mode_simul == "STO"){
    data_full_table[, nb_alea := runif(.N)]  # Générer un nombre aléatoire pour chaque ligne
    data_full_table[, COUPE := ifelse(prob_coupe > nb_alea, "OUI", "NON")]  # Déterminer l'état de coupe
  }

  # Réarranger les données en ordre id_pe et no_arbre
  setorder(data_full_table, id_pe, no_arbre)

  if(mode_simul == "DET"){
    # Sélectionner uniquement les colonnes désirées pour le mode déterministe
    cols_to_keep <- c(nom_var, "prob_coupe") # pour garder toutes les colonnes du fichier d'intrant, même celles dont on n'avait pas besoin
  }

  if(mode_simul == "STO"){
    # Sélectionner uniquement les colonnes désirées pour le mode stochastique
    cols_to_keep <- c(nom_var, "prob_coupe", "COUPE")
  }

  # Conserver uniquement les colonnes spécifiées
  result <- data_full_table[, ..cols_to_keep]

  return(result)
}

##############################################################

#prob_coupe(data_tree1, 5, "DET", 123, 100)

#prob_coupe(data_tree2, 12, "STO", 123, -60)

