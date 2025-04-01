#' Détermine la classe de qualité des arbres à partir de leur priorité de récolte MSCR
#'
#' @description  Détermine la classe de qualité des arbres à partir de leur priorité de récolte MSCR
#' avec les équations de Power et Havreljuk (2018).
#' Les paramètres peuvent être générés de façon déterministe ou stochastique.
#'
#' @details
#' Power, H. et Havreljuk, F., 2018. Predicting hardwood quality and its evolution over time in Quebec's forests.
#' Forestry (91).
#'
#' @param fic_arbres Dataframe avec une ligne par placette/arbre et les colonnes suivantes
#' \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item no_arbre: identifiant de l'arbre dans la placette
#'    \item dhpcm: dhp (cm) de l'arbre
#'    \item essence: code d'essence de l'arbre (ex: SAB, EPN, BOP)
#'    \item priorecol: code de priorité de récolte de l'arbre: M, S, C ou R
#'    \item sdom_bio: code du sous-domaine bioclimatique de la placette, en majuscule (ex: 1, 2EST, 4OUEST), seuls les domaines 1 à 6 sont traités
#'    \item ptot: précipitations totales annuelle moyenne sur la période 1980-2010 (mm) de la placette
#'    \item tmoy: température annuelle moyenne sur la période 1980-2010 (Celcius) de la placette
#'    \item sum_st_ha: Surface terrière marchande de la placette (m2/ha)
#'    \item coupe: 0 si pas de coupe partielle dans la placette, 1 si présence de coupe partielle
#' }
#' @param mode_simul Le mode de simulation (STO = stochastique, DET = déterministe), par défaut "DET".
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé, doit être > 1. Ignoré si \code{mode_simul="DET"},
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return La fonction retourne, pour le mode stochastique, un dataframe avec les colonnes id_pe, no_arbre, iter, qualite (la classe de qualité de l'arbre).
#' Pour le mode déterministe, la fonction retourne un dataframe avec les colonnes id_pe, no_arbre et les 4 colonnes (prop_A, prop_B, prob_C, prop_B) contenant la proportion de chaque classe de qualité
#'
#' @import data.table
# #' @export
#'
#' @examples
#' \dontrun{
#' # Mode déterministe
#' data_qualite <- attrib_qualite(ex_qualite)
#'
#' # Mode stochastique, pour 10 itérations
#' data_qualite <- attrib_qualite(ex_qualite, mode_simul='STO', nb_iter=10)
#'}
#'
attrib_qualite <- function(fic_arbres, mode_simul='DET', nb_iter=1, seed_value=NULL){


  # une des variables des équations de qualité est un regroupement de sous-domaines, et ce regroupement change selon l'essence et selon l'équation 1, 2 ou 3
  # il fat donc attribuer un groupe de sous-domaines à partir du sous-domaine fourni dans fic_arbres, et cette attribution doit être faire par essence et par équation
  # ces associations sont dans la liste qualite0_ass_sdom (un élément par essence),
  # l'élément est un dataframe avec une colonne pour identifier l'équation, la colonne sdom_bio est le lien avec le sdom_bio de fic_arbres, et la colonne sdom est le groupe d'essences pour l'équation

  # ligne de code pour générer les bi, une liste de 3 éléments, un par équation, chacun un dataframe contenant les bi par essence
  bi <- param_qualite0(mode_simul=mode_simul, nb_iter=nb_iter)

  # pour appliquer l'équation, une des variable est priorecol (fournie dans fic_arbres)
  # le bi associé à priorecol change selon la valeur de priorecol (4 valeurs):
  # si la valeur est S, il faut utiliser le bi de priorecol_S
  # si la valeur est C, il faut utiliser le bi de priorecol_C
  # si la valeur est R, il faut utiliser le bi de priorecol_R
  # si la valeur est M, on met bi=0

  # pour appliquer l'équation, une des variable est sdom (groupe de sous-domaines, à partir de sdom_bio dans fic_arbres et de son association):
  # si la valeur est 1, il faut utiliser le bi de sdom_1
  # si la valeur est 2EST, il faut utiliser le bi de sdom_2EST
  # si la valeur est 2ouEST, il faut utiliser le bi de sdom_2OUEST
  # etc.

  # comment écrire l'equation 1
  # names(bi[[1]])
  # xb = b_intercept_C +
  #      b_priorecol_x +  (choisir priorecol_C ou priorecol_R ou priorecol_S selon priorecol de l'arbre, Si M, 0)
  #      b_sdom_x +       (choisir sdom_1 ou priorecol_R ou sdom_2EST ou etc. selon sdom de l'arbre, Si 6OUEST, 0)
  #      b_sum_st_ha * sum_st_ha +
  #      b_tmoy * tmoy +
  #      b_coupe * coupe +
  #      b_dhpcm * dhpcm
  #      b_dhpcm_x * dhpcm +  (choisir b_dhpcm*priorecol_C ou b_dhpcm*priorecol_R ou b_dhpcm*priorecol_S selon priorecol de l'arbre, Si M, 0)
  #      b_dhpcm*sum_st_ha * dhpcm * sum_st_ha

  # comment écrire les 2 équations de equation 2
  # names(bi[[2]])
  # xb_b = b_intercept_B +
  #        b_priorecol_x +  (choisir priorecol_C ou priorecol_R ou priorecol_S selon priorecol de l'arbre, Si M, 0)
  #        b_sdom_x +       (choisir sdom_1 ou priorecol_R ou sdom_2EST ou etc. selon sdom de l'arbre, Si 5OUEST, 6EST ou 6OUEST, 0)
  #        b_tmoy * tmoy +
  #        b_dhpcm * dhpcm
  #        b_sum_st_ha * sum_st_ha +
  #        b_sum_st_ha_x * sum_st_ha +  (choisir b_sum_st_ha*priorecol_C ou b_sum_st_ha*priorecol_R ou b_sum_st_ha*priorecol_S selon priorecol de l'arbre, Si M, 0)
  #        b_coupe * coupe +

  # xb_c est la meme equation que xb_b, mais avec b_intercept_C au lieu de b_intercept_B

  # comment écrire les 3 équations de equation 3
  # names(bi[[3]])
  # xb_a = b_intercept_A +
  #        b_priorecol_x +  (choisir priorecol_C ou priorecol_R ou priorecol_S selon priorecol de l'arbre, Si M, 0)
  #        b_sdom_x +       (choisir sdom_1 ou priorecol_R ou sdom_2EST ou etc. selon sdom de l'arbre, Si 5OUEST, 6EST ou 6OUEST, 0)
  #        b_tmoy * tmoy +
  #        b_ptot * ptot +
  #        b_dhpcm * dhpcm
  #        b_sum_st_ha * sum_st_ha +
  #        b_sum_st_ha_x * sum_st_ha +  (choisir b_sum_st_ha*priorecol_C ou b_sum_st_ha*priorecol_R ou b_sum_st_ha*priorecol_S selon priorecol de l'arbre, Si M, 0)
  #        b_dhpcm*intercept_A * dhpcm

  # xb_b est la meme equation que xb_a, mais avec b_intercept_B et b_dhpcm*intercept_B
  # xb_c est la meme equation que xb_a, mais avec b_intercept_C et b_dhpcm*intercept_C

  # voir le powerpoint pour les autres choses a calculer



}
