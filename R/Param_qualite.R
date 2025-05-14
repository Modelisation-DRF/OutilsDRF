#' Génère les paramètres des équations de qualité
#'
#' @description Génère les paramètres des équations d'attribution ou d'évolution de la classe de qualité des arbres,
#' où les arbres sont regroupés en placettes. Les paramètres peuvent être générés de façon déterministe ou stochastique.
#' Si le mode stochastique est utilisé, une série de paramètres par itération par essence sont générés.
#' Si le mode déterministe est utilisé, une série de paramètres par essence sont générés.
#'
#' @details
#' Power, H. et Havreljuk, F., 2018. Predicting hardwood quality and its evolution over time in Quebec's forests.
#' Forestry (91).
#'
#' @param type_qualite "attrib" pour générer les paramètres des modèles d'attribution de qualité ou "evol" pour les paramètres d'évolution de la qualité
#' @param mode_simul Le mode de simulation (STO = stochastique, DET = déterministe), par défaut "DET".
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé. Ignoré si \code{mode_simul="DET"},
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return La fonction retourne une liste de 3 éléments, chacun étant un dataframe dont les colonnes sont les paramètres
#' des équations de qualité et les lignes sont les essences. Le premier contient les paramètres pour les dhp de 23 à 33 cm,
#' le 2e pour les dhp de 33 à 39 cm et les 3e pour les dhp>39cm. En mode stochastique, les datframe contient une lignes par essence/iter
#'
#' @import data.table
# #' @export
#'
#' @examples
#' \dontrun{
#' # Mode déterministe
#' bi <- param_qualite()
#'
#' # Mode stochastique, pour 10 itérations
#' bi <- param_qualite(mode_simul = "STO", nb_iter = 10)
#' }
#'
param_qualite <- function(type_qualite, mode_simul = "DET", nb_iter = 1, seed_value = NULL) {

  if (length(seed_value) > 0) {
    set.seed(seed_value)
  }

  # le dataframe interne contenant tous les b_i des équations : qualite_evol_param ou qualite0_param
  # une ligne par essence/équation/b_i

  # les matrices de covariances des effets fixes (pour le mode stochastique) : qualite_evol_param_covb ou qualite0_param_covb
  # une liste de 8 éléments, un par essence

  # Paramètres pour attribution de la qualité
  parametre <- qualite0_param
  covariance <- qualite0_param_covb
  # Paramètre pour évolution de la qualité
  if (type_qualite=='evol') {

    parametre <- qualite_evol_param
    covariance <- qualite_evol_param_covb

  }

  # Pour le mode stochastique
  if (mode_simul == "STO") {

    # faire une boucle sur essence
    bi_qual_8ess <- NULL

    for (i in 1:length(qualite0_ess)) { # qualite0_ess est un fichier interne

      bi <- parametre %>% filter(essence == qualite0_ess[i]) %>% dplyr::select(-essence)
      covb <- covariance[[i]]

      # le modèle de qualité n'a pas d'effet aléatoire ni d'erreur résiduelle
      # la seule partie qui est stochastique est celle des effets fixes
      # les bi du modèle pour une itération seront utilisés pour tous les arbres, de toutes les placettes
      # il y aura donc une série de bi par itération

      # lecture des effets fixes pour chacune des 3 equations
      bi_qual_3eq <- NULL
      for (j in 1:3) {

        bi2 <- bi %>% filter(Equation == j) %>% dplyr::select(b_i)
        covb2 <- covb[[j]]

        # générer une série de bi, une par itération
        # il faut donc autant de séries qu'il y a d'itérations, les itérations sont en lignes, les bi en colonne
        # pour que mvrnorm() fonctionne avec empirical=T, il faut au moins autant de n que la longueur du vecteur mu à simuler
        mu <- as.matrix(bi2)
        l_mu <- length(mu)
        # seulement si le fichier des paramètres n'est pas vide (pour evolution de la qualité, CHX n'a pas d'eq 2 ni d'eq 3)
        if (l_mu>0) {
          if (nb_iter < l_mu) {
            nb_iter_temp <- l_mu
          } else {
            nb_iter_temp <- nb_iter
          }
          bi_qual <- as.data.frame(matrix(
            rockchalk::mvrnorm(
              n = nb_iter_temp,
              mu = mu,
              Sigma = covb2,
              empirical = T
            ),
            nrow = nb_iter_temp
          ))[1:nb_iter, ]
          # transposer les bi pour les avoir en ligne
          nom <- data.frame(var = names(bi_qual))
          bi_a <- bind_cols(bi[bi$Equation == j, ], nom) %>% dplyr::select(-b_i)
          bi_qual_tr <- bi_qual %>%
            mutate(iter = row_number()) %>%
            group_by(iter) %>%
            pivot_longer(cols = all_of(names(bi_qual)), names_to = "var", values_to = "b_i")
          bi_qual_tr2 <- left_join(bi_qual_tr, bi_a, by = "var") %>% dplyr::select(-var)

          # accumuler les bi des 3 équations
          bi_qual_3eq <- bind_rows(bi_qual_3eq, bi_qual_tr2)
        }
      }
      # ajouter le code de l'essence
      bi_qual_3eq <- bi_qual_3eq %>% mutate(essence = qualite0_ess[i])
      # accumuler les bi des essences
      bi_qual_8ess <- bind_rows(bi_qual_8ess, bi_qual_3eq)
    }
  } # fin du sTO

  # pour le mode déterministe
  if (mode_simul == "DET") {
    # le fichier aura autant de lignes que d'essences
    bi_qual_8ess <- parametre %>% mutate(iter = 1)
  }

  # transposer le data des bi separément pour chacune des equations pour que toutes les variables soient présentes pour toutes les essences
  bi_qual_8ess <- bi_qual_8ess %>% mutate(var_i = paste0("b_", var_i)) # ajouter b_ en avant du nom du bi
  bi_eq1 <- bi_qual_8ess %>%
    filter(Equation == 1) %>%
    group_by(Equation, iter, essence) %>%
    pivot_wider(values_from = "b_i", names_from = "var_i") %>%
    ungroup()
  bi_eq2 <- bi_qual_8ess %>%
    filter(Equation == 2) %>%
    group_by(Equation, iter, essence) %>%
    pivot_wider(values_from = "b_i", names_from = "var_i") %>%
    ungroup()
  bi_eq3 <- bi_qual_8ess %>%
    filter(Equation == 3) %>%
    group_by(Equation, iter, essence) %>%
    pivot_wider(values_from = "b_i", names_from = "var_i") %>%
    ungroup()

  # ici, ajouter du code au besoin pour modifier les dataframes bi_eq1, bi_eq2, bi_eq3 pour qu'ils soient structurés de façon à faciliter son utiliser dans la fonction qui appliquera les équations

  return(list(bi_eq1, bi_eq2, bi_eq3))
}
