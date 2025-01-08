################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#   Function for estimating tree volume for a list of trees    #
#                                                              #
#                                                              #
#   Use data/tarif_ass_ess.rda                                 #
#                                                              #
################################################################


#' Estime le volume marchand sans écorce de chacun des arbres avec l'équation de Fortin et al. (2007)
#'
#' @description Estime le volume marchand sans écorce de chacun des arbres avec l'équation de Fortin et al. (2007). La fonction permet l'estimation pour une liste d'arbres regroupés en placette.
#' L'estimation peut être déterministe ou stochastique.
#'
#' @details
#' L'équation pour estimer le volume marchand a été étalonnée avec un modèle linéaire mixte où l'essence est une covariable dans l'équation (Fortin et al. (2007).
#' Le modèle inclut un effet aléatoire de placette et et un effet alétoire de virée, mais seul l'effet aléatoire de placette est simulé en mode stochastique.
#' La fonction estime le volume de façon déterministe ou stochastique.
#'
#' Fortin, M., J. DeBlois, S. Bernier et G. Blais, 2007. Mise au point d’un tarif de cubage général pour
#' les forêts québécoises : une approche pour mieux évaluer l’incertitude associée aux prévisions.
#' For. Chron. 83: 754-765.
#'
#' @param fic_arbres Une table contenant la liste d'arbres regroupés en placettes avec les informations suivantes:
#' \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item dhpcm: dhp (cm) de l'arbre ou classe de dhp (>9 cm)
#'    \item essence: code d'essence de l'arbre (ex: SAB, EPN, BOP)
#'    \item no_arbre: identifiant de l'arbre ou de la combinaison dhp/essence
#'    \item hauteur_pred: hauteur de l'arbre (m)
#'    \item iter: numéro de l'itération, seulement si mode stochastique, doit être numéroté de 1 à nb_iter
#'    \item step: numéro de la step, seulement si mode stochastique, doit être numéroté de 1 à nb_step. Obligatoire même si le fichier n'est qu'une liste d'arbres à un moment donné.
#' }
#' @param mode_simul Mode de simulation (STO = stochastic, DET = deterministic), par défaut "DET"
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé, doit être > 1. Ignoré si \code{mode_simul="DET"},
#' @param nb_step Le nombre d'années pour lesquelles on veut estimer la hauteur pour un même arbre (par défaut 1), ignoré si \code{mode_simul="DET"}.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.

#' @return La table \code{fic_arbres} avec une colonne contenant le volume marchand estimé en dm3 (vol_dm3).
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' # Exemple 1: DETERMINISTE: une seule année par arbre ----------------------------------------
#' # Estimer la hauteur et ensuite le volume
#' DataHt <- relation_h_d(fic_arbres=fic_arbres_test)
#' DataHtVol <- cubage(fic_arbres=DataHt)
#'
#' # Exemple 2: DETERMINISTE: plusieurs années par arbre -------------------------------------
#' # Estimer la hauteur et ensuite le volume
#' DataHt <- relation_h_d(fic_arbres=fic_artemis_det, grouping_vars = 'annee')
#' DataHtVol <- cubage(fic_arbres=DataHt)
#'
#' # Exemple 3: STOCHASTIQUE: plusieurs itérations et plusieurs step -------------------------------
#' nb_iter <- length(unique(fic_artemis_sto$iter)) # 10
#' nb_step <- length(unique(fic_artemis_sto$annee)) # 5
#' ht <- relation_h_d(fic_arbres=fic_artemis_sto, mode_simul='STO', nb_iter=nb_iter, nb_step=nb_step)
#' vol <- cubage(fic_arbres=ht, mode_simul='STO', nb_iter=nb_iter, nb_step=nb_step)
#' }
#'
cubage <- function (fic_arbres, mode_simul='DET', nb_iter=1, nb_step=1, seed_value=NULL){

  # en mode stochastique, les variables iter et step sont obligatoires
  if (mode_simul=='STO'){
     if (length(setdiff(c("iter","step"), names(fic_arbres))) >0) { stop("les colonnes iter et step doivent etre dans fic_arbres avec mode_simul=STO")}
  }

  # générer les paramètres du tarif de cubage
  parametre_vol <- param_vol(fic_arbres=fic_arbres, mode_simul=mode_simul, nb_iter=nb_iter, nb_step=nb_step, seed_value=seed_value)

  # association des essences aux essences du tarif de cubage (tarif_ass_ess.rda)
  tarif_ass_ess2 <- tarif_ass_ess
  setDT(tarif_ass_ess2)
  arbre_vol <- merge(fic_arbres, tarif_ass_ess2, by = "essence", all.x=T) # un vrai left_join
  arbre_vol[
    , `:=`(
      essence_orig = essence,
      essence = essence_volume
    )
  ][, essence_volume := NULL
  ]

if (mode_simul=='DET'){
  # Ajout des paramètres des effets fixes du tarif au fichier des arbres
  # dans le fichier parametre_vol, essence est le code de l'essence du modèle de hauteur, une des 26
  # dans le fichier arbre_vol, essence est l'essence original et essence_volume est l'essence du modele
   arbre_vol2 <- merge(arbre_vol, parametre_vol$effet_fixe, by = "essence", all.x=T)
}

if (mode_simul=='STO'){

  liste_ess <- unique(tarif_ass_ess$essence_volume) # liste des essences

  # ajouter les effets fixes
  arbre_vol2a <- merge(arbre_vol, parametre_vol$effet_fixe, by = c("iter","essence"), all.x=T)

  # ajouter de l'effet aléatoire de placette et l'erreur residuelle à tous les arbres
  arbre_vol2 <- merge(arbre_vol2a, parametre_vol$random, by = c("iter", "step", "id_pe", "no_arbre", "essence"), all.x=T)
  arbre_vol2[,`:=`(resid = 0)]

  # garder la colonne de l'erreur residuelle de l'essence
  for (ess in liste_ess) {  # liste_ess contient les noms des essences (comme 'BOJ')
    arbre_vol2[essence == ess, resid := .SD[[ess]]]
  }

  arbre_vol2[, (liste_ess) := NULL]

}

# Calcul du volume

# volume en dm3;
# dhp en cm;
# hauteur en m;
# dres=1 pour résineux;
# ht_dhp = hauteur_pred/dhp;
# cylindre = pi*dhp**2*hauteur_pred/40;
# vol = -b1 x ht_dhp + (b2m + b3m*dres*dhp)*cylindre (mais le négatif est déjà appliqué au b1)

arbre_vol2[, `:=`(
  cylindre = (pi * dhpcm * dhpcm * hauteur_pred) / 40,
  vol_dm3 = b1 * (hauteur_pred / dhpcm) + (b2 + b3 * as.integer(essence %in% c('EPB', 'EPN', 'EPR', 'MEL', 'PIB', 'PIG', 'PIR', 'PRU', 'SAB', 'THO')) * dhpcm) * ((pi * dhpcm * dhpcm * hauteur_pred) / 40) + random_plot + resid
)]

# Mettre un minimum de 4 à `vol_dm3`
arbre_vol2[vol_dm3 < 4, vol_dm3 := 4]

# Supprimer les colonnes inutiles
arbre_vol2[, c("cylindre", "essence", "b1", "b2", "b3", "random_plot", "resid") := NULL]

# Renommer `essence_orig` en `essence`
setnames(arbre_vol2, "essence_orig", "essence")

return (arbre_vol2)
}






