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
#' Les essences avec une équations sont  BOJ BOP CHX ERR ERS FEN HEG PEU
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
#'    \item iter: numéro de l'itération, seulement si mode stochastique, doit être numéroté de 1 à nb_iter
#' }
#' @param mode_simul Le mode de simulation (STO = stochastique, DET = déterministe), par défaut "DET".
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé. Ignoré si \code{mode_simul="DET"},
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return La fonction retourne, pour le mode stochastique, un dataframe avec les colonnes id_pe, no_arbre, iter,
#' qualite (la classe de qualité de l'arbre). Il y a une ligne par itération pour chaque arbre.
#' Pour le mode déterministe, la fonction retourne un dataframe avec les colonnes id_pe, no_arbre et les 4 colonnes
#' (prop_A, prop_B, prob_C, prop_B) contenant la proportion de chaque classe de qualité
#'
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' # Mode déterministe
#' data_qualite <- attrib_qualite(ex_qualite)
#'
#' # Mode stochastique, pour 10 itérations
#' data_qualite <- attrib_qualite(ex_qualite_sto, mode_simul = "STO", nb_iter = 5)
#' }
#'
attrib_qualite <- function(fic_arbres, mode_simul = "DET", nb_iter = 1, seed_value = NULL) {
  # une des variables des équations de qualité est un regroupement de sous-domaines, et ce regroupement change selon
  # l'essence et selon l'équation 1, 2 ou 3 il faut donc attribuer un groupe de sous-domaines à partir du sous-domaine
  # fourni dans fic_arbres, et cette attribution doit être faire par essence et par équation ces associations sont dans
  # la liste qualite0_ass_sdom (un élément par essence), l'élément est un dataframe avec une colonne pour identifier
  # l'équation, la colonne sdom_bio est le lien avec le sdom_bio de fic_arbres, et la colonne sdom est le groupe
  # d'essences pour l'équation

  # fic_arbres=ex_qualite; mode_simul="DET"; nb_iter = 1; seed_value = NULL
  # fic_arbres=entree; mode_simul="STO"; nb_iter = 2; seed_value = 1234

  # ligne de code pour générer les bi, une liste de 3 éléments, un par équation, chacun un dataframe contenant les bi par essence
  bi <- param_qualite(type_qualite = "attrib", mode_simul = mode_simul, nb_iter = nb_iter, seed_value = seed_value)

  # Joindre les 3 dataframe BI en 1, pour pouvoir faire un join avec le fichier d'arbre et les calculs
  joinedBi <- bind_rows(bi[[1]], bi[[2]], bi[[3]]) %>%
    replace(is.na(.), 0)


  # Obtenir l'equation a utiliser pour chaque arbre
  fic_arbres <- fic_arbres %>%
    lazy_dt() %>%
    mutate(Equation = ifelse(dhpcm > 23 & dhpcm <= 33, 1, ifelse(dhpcm > 33 & dhpcm <= 39, 2, ifelse(dhpcm > 39, 3, NA)))) %>%
    as.data.frame()

  # Obtenir l'association du sous-domaine, nécessaire car on ne peut pas faire la différence entre une absence de sdom est dû à un sdom manquant dans les données de calibration vs le sdom dans l'intercept
  fic_arbres <- fic_arbres %>%
    lazy_dt() %>%
    left_join(qualite0_ass_sdom, by = c("essence", "Equation", "sdom_bio"), keep = FALSE) %>%
    as.data.frame()

  # Joindre chaque arbre avec les valeurs BI selon l'essence et l'equation
  by_var = c("essence", "Equation")
  if (mode_simul=='STO') {by_var = c("iter", "essence", "Equation")}
  fic_arbres <- fic_arbres %>%
    lazy_dt() %>%
    left_join(joinedBi, by = by_var, keep = FALSE) %>%
    as.data.frame()

  # Equation 1: qualite C ou D
  # Equation 2: qualite B, C ou D
  # equation 3: qualite A, B, C ou D
  # les paramètres de toutes les équations sont dans un seul fichier, avec des 0 si le paramètre ne s'applique pas à une équation en particulier
  # on peut donc écrire une seule équation avec l'ensemble des variables possibles
  # il restera à ajouter le bon intercept
  fic_arbres <- fic_arbres %>%
    lazy_dt() %>%
    mutate(
      # equation commune pour toutes les qualités et les 3 groupes des dhp
      xb =
        b_sum_st_ha * sum_st_ha +
        b_tmoy * tmoy +
        b_coupe * coupe +
        b_dhpcm * dhpcm +
        b_dhpcm_x_sum_st_ha * dhpcm * sum_st_ha +
        b_ptot * ptot +
        case_when(
          priorecol == "S" ~ b_priorecol_S,
          priorecol == "C" ~ b_priorecol_C,
          priorecol == "R" ~ b_priorecol_R,
          priorecol == "M" ~ 0) +
        case_when(
          sdom == "1" ~ b_sdom_1,
          sdom == "2EST" ~ b_sdom_2EST,
          sdom == "2OUEST" ~ b_sdom_2OUEST,
          sdom == "3EST" ~ b_sdom_3EST,
          sdom == "3OUEST" ~ b_sdom_3OUEST,
          sdom == "4EST" ~ b_sdom_4EST,
          sdom == "4OUEST" ~ b_sdom_4OUEST,
          sdom == "5EST" ~ b_sdom_5EST,
          sdom == "5OUEST" ~ b_sdom_5OUEST,
          sdom == "6EST" ~ b_sdom_6EST) +
        case_when(
          priorecol == "S" ~ b_dhpcm_x_priorecol_S * dhpcm,
          priorecol == "C" ~ b_dhpcm_x_priorecol_C * dhpcm,
          priorecol == "R" ~ b_dhpcm_x_priorecol_R * dhpcm,
          priorecol == "M" ~ 0) +
        case_when(
          priorecol == "S" ~ b_sum_st_ha_x_priorecol_S * sum_st_ha,
          priorecol == "C" ~ b_sum_st_ha_x_priorecol_C * sum_st_ha,
          priorecol == "R" ~ b_sum_st_ha_x_priorecol_R * sum_st_ha,
          priorecol == "M" ~ 0),

      xb_C = xb + b_intercept_C + b_dhpcm_x_intercept_C * dhpcm,  # prob C est possible pour les 3 groupes de dhp (Equation 1 2 ou 3)
      xb_B = ifelse(Equation %in% c(2,3), xb + b_intercept_B + b_dhpcm_x_intercept_B * dhpcm, NA),  # prob B est possible seulement pour eq 2 et 3
      xb_A = ifelse(Equation==3, xb + b_intercept_A + b_dhpcm_x_intercept_A * dhpcm, NA),  # prob A est possible seulement pour eq 3

      # Calculer la probabilite de chaque qualite possible pour chaque arbre
      prob_C = 1 / (1 + exp(-xb_C)),
      prob_B = 1 / (1 + exp(-xb_B)),
      prob_A = 1 / (1 + exp(-xb_A))

    ) %>%
    as.data.frame()

  # Calculer la probabilite de chaque qualite possible pour chaque placette (si xb_x est NA, la probabilite est NA)
  fic_arbres <- fic_arbres %>%
    lazy_dt() %>%
    mutate(
      prob_C = 1 / (1 + exp(-xb_C)),
      prob_B = ifelse(!is.na(xb_B), 1 / (1 + exp(-xb_B)), NA),
      prob_A = ifelse(!is.na(xb_A), 1 / (1 + exp(-xb_A)), NA)
    ) %>%
    as.data.frame()

  # Determiner la qualite (mode stochastique) ou la proportion de la qualite (mode deterministe), pour chaque placette
  if (mode_simul == "DET") {
    # Proportion de chaque qualite
    fic_arbres <- fic_arbres %>%
      lazy_dt() %>%
      mutate(
        prop_D = 1 - prob_C,
        prop_C = ifelse(!is.na(prob_B), prob_C - prob_B, prob_C),
        prop_B = ifelse(!is.na(prob_A), prob_B - prob_A, prob_B),
        prop_A = ifelse(prob_A == 0, NA, prob_A)
      ) %>%
      # Select des colonnes finales à retourner
      dplyr::select(id_pe, sdom_bio, tmoy, ptot, sum_st_ha, coupe, no_arbre, dhpcm, essence, priorecol, prop_A, prop_B, prop_C, prop_D) %>%
      as.data.frame()
  } else {

    if (length(seed_value) > 0) {
      set.seed(seed_value)
    }

    # Generer un nombre aleatoire entre 0 et 1 pour chaque placette
    fic_arbres <- fic_arbres %>%
      lazy_dt() %>%
      mutate(randNum = runif(nrow(fic_arbres), 0, 1)) %>%
      as.data.frame()

    # Utiliser le nombre aleatoire pour determiner la qualite
    fic_arbres <- fic_arbres %>%
      lazy_dt() %>%
      mutate(
        qualite =
          if_else(!is.na(prob_A) & randNum <= prob_A,
            "A",
            if_else(!is.na(prob_B) & randNum <= prob_B,
              "B",
              if_else(randNum <= prob_C,
                "C",
                "D"
              )
            )
          )
      ) %>%
      # Select des colonnes finales à retourner
      dplyr::select(id_pe, sdom_bio, tmoy, ptot, sum_st_ha, coupe, no_arbre, dhpcm, essence, priorecol, iter, qualite) %>%
      as.data.frame()
  }

  return(fic_arbres)
}
