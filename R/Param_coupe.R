#' Génère les paramètres pour estimer la probabilité de coupe de chacun des arbres regroupés en placettes
#'
#' @description Génère les paramètres pour estimer la probabilité d'un arbre d'être coupé, où les arbres sont regroupés en placettes.
#' Les paramètres peuvent être générés de façon déterministe ou stochastique.
#' Si le mode stochastique est utilisé, les paramètres seront générés pour tous les arbres/itétations/années.
#' Si le mode déterministe est utilisé, les paramètres seront générés pour les arbres du fichier fourni en entrée.
#'
#' @details
#' Fortin, M., 2014. Using a segmented logistic model to predict trees to be harvested
#' in forest growth forecasts. Forest Systems(1), 139-152.
#'
#' Power, H., 2015. Comparaison des traitements de récolte effectués dans les régions 06 et 07
#' par l'entreprise "Lauzon-Planchers de bois exclusifs inc." avec les simulations de
#' traitements génériques CP35_40cm et CP45_40cm. Gouvernement du Québec, Gouvernement du Québec, ministère
#' des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Avis technique SSRF-7. 16 p.
#'
#' @param trt_coupe Le numéro de traitement de coupe, un entier entre 0 et 18
#' #' \enumerate{
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
#' @param mode_simul Le mode de simulation (STO = stochastique, DET = déterministe), par défaut "DET".
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé, doit être > 1. Ignoré si \code{mode_simul="DET"},
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return La fonction retourne, pour le mode stochastique, une table avec une liste de paramètres par itérations.
#' Pour le mode déterministe, la fonction retourne une table de paramètres.
#' Les variables sont
#'
#' @import data.table
# #' @export
#'
#' @examples
#' \dontrun{
#' # Mode déterministe
#' parametre_coupe <- param_coupe()
#'
#' # Mode stochastique, pour une seule année et 10 itérations
#' parametre_coupe <- param_coupe(mode_simul = "STO", nb_iter = 10)
#' }
#'
param_coupe <- function(trt_coupe, mode_simul = "DET", nb_iter = 1, seed_value = NULL) {
  # trt_coupe=10; mode_simul='DET'; nb_iter=1; seed_value=NULL;
  # trt_coupe=10; mode_simul='STO'; nb_iter=10; seed_value=NULL;

  if (mode_simul == "STO") {
    if (nb_iter == 1) {
      stop("Le nombre d'iterations doit etre plus grand que 1 en mode stochastique")
    }
  }

  if (length(seed_value) > 0) {
    set.seed(seed_value)
  }

  # le fichier des tous les paramètres des modèles de coupe est dans: coupe_param
  # c'est une table avec les paramètre dans une seule colonne

  # le fichier des matrices de covariances des effets fixes : coupe_param_covb
  # une liste de un élément pas traitement de coupes


  # choisir le trt
  param <- coupe_param %>%
    filter(num_trt == trt_coupe) %>%
    dplyr::select(code_trt, num_trt, essence, effect, estimate)
  covb <- coupe_param_covb[[trt_coupe + 1]]

  if (mode_simul == "STO") {
    # le modèle de coupe n'a pas d'effet alétoire ni d'erreur résiduelle
    # la seule partie qui est stochastique est cellle des effets fixes
    # les paramètres du modèle pour une itération seront utilisés pour tous les arbres, de toutes les placettes, pour toutes leurs steps
    # il y aura donc une série de paramètres par itération

    # lecture des effets fixes du trt
    param2 <- param %>% dplyr::select(estimate)

    # générer une série de paramètres d'effets fixes, une par itération
    # il faut donc autant de séries qu'il y a d'itérations, les itérations sont en lignes, les effets fixes en colonne
    # pour que mvrnorm() fonctionne avec empirical=T, il faut au moins autant de n que la longueur du vecteur mu à simuler
    mu <- as.matrix(param2)
    l_mu <- length(mu)
    if (nb_iter < l_mu) {
      nb_iter_temp <- l_mu
    } else {
      nb_iter_temp <- nb_iter
    }
    param_cp <- as.data.frame(matrix(
      rockchalk::mvrnorm(
        n = nb_iter_temp,
        mu = mu,
        Sigma = covb,
        empirical = T
      ),
      nrow = nb_iter_temp
    ))[1:nb_iter, ]
    # transposer les paramètres
    nom <- data.frame(var = names(param_cp))
    param_a <- bind_cols(param, nom) %>% dplyr::select(-estimate)
    param_cp_tr <- param_cp %>%
      mutate(iter = row_number()) %>%
      group_by(iter) %>%
      pivot_longer(cols = all_of(names(param_cp)), names_to = "var", values_to = "estimate")
    param_cp_tr2 <- left_join(param_cp_tr, param_a, by = "var") %>% dplyr::select(-var)
  }

  if (mode_simul == "DET") {
    # le fichier aura autant de lignes que d'essences du modele de coupe dans le fichier d'entrée
    param_cp_tr2 <- param %>% mutate(iter = 1)
  }

  # préparer le fichier des paramètres pour appliquer l'équation

  # effets qui ne dépendent pas de l'essence et les transposer
  ess_non <- param_cp_tr2 %>% filter(is.na(essence))
  ess_non_tr <- ess_non %>%
    select(iter, code_trt, num_trt, effect, estimate) %>%
    group_by(iter, code_trt, num_trt) %>%
    pivot_wider(names_from = effect, values_from = estimate)

  # transposer les effets qui dépendent de l'essence
  param_ess <- param_cp_tr2 %>%
    filter(!is.na(essence)) %>%
    group_by(iter, code_trt, num_trt, essence) %>%
    pivot_wider(names_from = effect, values_from = estimate)

  # ajouter les effet qui ne dépendent pas de l'essence
  param_ess2 <- left_join(param_ess, ess_non_tr, by = c("iter", "code_trt", "num_trt"))

  # ajouter les paramètres qui ne sont pas présents dans ce trt
  list_effets <- c("b1_s", "b2_s", "b4_s", "b3_s", "b5_s", "b6_s", "b7_s", "b0", "b4", "b5", "b6", "b3", "b2")
  effet_presents <- param_ess2 %>%
    ungroup() %>%
    select(-iter, -code_trt, -num_trt, -essence) %>%
    names()
  effets_manquants <- setdiff(list_effets, effet_presents)
  for (var in effets_manquants) {
    param_ess2[[var]] <- NA
  }

  # copier les parmètres qui ne dépendent pas de l'essence dans les colonnes des effets des essences
  param_ess3 <- param_ess2 %>%
    mutate(
      b2_s = ifelse(!is.na(b2), b2, b2_s),
      b3_s = ifelse(!is.na(b3), b3, b3_s),
      b4_s = ifelse(!is.na(b4), b4, b4_s),
      b5_s = ifelse(!is.na(b5), b5, b5_s),
      b6_s = ifelse(!is.na(b6), b6, b6_s)
    ) %>%
    select(-c(b2, b3, b4, b5, b6))

  # mettre des 0 au lieu des NA
  param_ess3 <- param_ess3 %>% replace(is.na(.), 0)

  return(param_ess3)
}
