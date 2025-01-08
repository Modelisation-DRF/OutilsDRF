################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#                                                              #
#   Utilise                                                    #
#   ht_param_fixe.rda                                          #
#   ht_param_cov.rda                                           #
#   ht_param_random.rda                                        #
#   ht_liste_ess.rda                                           #
#                                                              #
################################################################
#' Génère les paramètres pour estimer la hauteur totale de chacun des arbres regroupés en placettes, avec l'équation de Auger (2016)
#'
#' @description Génère les paramètres pour estimer la hauteur totale de chacun des arbres regroupés en placettes, avec l'équation de Auger (2016).
#' Les paramètres peuvent être générés de façon déterministe ou stochastique.
#' Si le mode stochastique est utilisé, les paramètres seront générés pour tous les arbres/itétations/années.
#' Si le mode déterministe est utilisé, les paramètres seront générés pour les essences du fichier fourni en entrée.
#'
#' @details
#' L'équation pour estimer la hauteur totale a été étalonnée avec un modèle linéaire mixte, par essence (Auger 2016).
#' Le modèle inclut un effet aléatoire de placette et une corrélation de type CorCar1 sur les erreurs résiduelles.
#'
#' Auger, I., 2016. Une nouvelle relation hauteur-diamètre tenant compte de l’influence de la station et
#' du climat pour 27 essences commerciales du Québec. Gouvernement du Québec, ministère
#' des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Note de recherche forestière no 146. 31 p.
#'
#' @param fic_arbres Une table contenant la liste d'arbres dont la hauteur est à estimer avec leur essence.
#' La table doit contenir les variables \code{no_arbre} (l'identifiant de l'arbre) et \code{id_pe} (l'identifiant de la placette)
#' et \code{essence} (le code d'essence). Si mode stochastique, la table doit aussi contenir les colonnes step et iter.
#' Si pas de step, créer une colonne avec step=1
#' @param mode_simul Le mode de simulation (STO = stochastique, DET = déterministe), par défaut "DET".
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé, doit être > 1. Ignoré si \code{mode_simul="DET"},
#' @param nb_step Le nombre d'années pour lesquelles on veut estimer la hauteur pour un même arbre (par défaut 1), ignoré si \code{mode_simul="DET"}.
#' @param dt La durée de l'intervalle de temps entre deux mesures d'un même arbre si \code{nb_step>1} (par défaut 10), ignoré si \code{mode_simul="DET"}.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#'
#' @return La fonction retourne, pour le mode stochastique, une table avec autant de lignes que de placette/arbre/iter/step, contenant tous les paramètres nécessaires pour appliquer l'équation h-d.
#' Pour le mode déterministe, la fonction retourne une table de paramètres avec une ligne par essence présente dans le fichier \code{fic_arbres}.
#' Les variables sont
#' \enumerate{
#'   \item id_pe, no_arbre, iter (numérotés de 1 à nb_iter), step (numérotés de 1 à nb_step) (pour le mode stochastique seulement)
#'   \item essence utilisée pour le modèle de hauteur
#'   \item tous les paramètres des effets fixes des essences du fichier, une colonne par paramètre
#'   \item random_ldhp2 effet aléatoire de placette pour l'essence/iter, 0 si mode déterministe
#'   \item res_arbre: erreur_residuelle de l'arbre pour une iter/step, 0 si mode déterministe
#' }
#' @import data.table
# #' @export
#'
#' @examples
#' \dontrun{
#' # Mode déterministe
#' parametre_ht_dhp <- param_ht(fic_arbres=fic_arbres_test)
#'
#' # Mode stochastique, pour une seule année et 10 itérations
#' parametre_ht_dhp <- param_ht(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10)
#'
#' # Mode stochastique, plusieurs années et plusieurs itérations
#' parametre_ht_dhp <- param_ht(fic_arbres=fic_arbres_test, mode_simul='STO', nb_iter=10, nb_step=5)
#'}
#'
param_ht <- function(fic_arbres, mode_simul='DET', nb_iter=1, nb_step=1, dt=10, seed_value=NULL){

  # ne garder que les variables nécessaires si stochastique
  if (mode_simul=='STO'){
    fic_arbres <- fic_arbres %>% dplyr::select(id_pe, no_arbre, essence, iter, step) # si sto, on a besoin de iter et step
  }

  if (mode_simul=='STO'){
    if (nb_iter==1) {stop("Le nombre d'iterations doit etre plus grand que 1 en mode stochastique")}
  }

  if (length(seed_value)>0) {set.seed(seed_value)}

  # le fichier des tous les paramètres des modèles de ht est dans: ht_param_fixe
  # c'est une liste avec un élément pas essence, soit une table avec une seule ligne et autant de colonne que de paramètres

  # le fichier des matrices de covariances des effets fixes : ht_param_cov
  # une liste de un élément pas essence

  # le fichier des effets aléatoires: ht_param_random
  # une table avec 3 lignes par essences: random, rho, resid

  ht_ass_ess2 <- ht_ass_ess
  setDT(ht_ass_ess2)
  setDT(fic_arbres)

  # établir la liste des essences pour lesquelles on a besoin d'une relation h-d
  fic_arbres_temp2 <- merge(fic_arbres, ht_ass_ess2, by = "essence", all.x = T)
  fic_arbres_temp2a <- fic_arbres_temp2 %>% rename(essence_orig=essence) %>% rename(essence=essence_hauteur)
  ht_liste_ess_temp <- unique(fic_arbres_temp2$essence_hauteur)
  ht_liste_ess_complet <- unique(ht_ass_ess2$essence_hauteur)

  if (mode_simul=='STO') {

    # liste des arbres
    liste_arbre <- unique(fic_arbres_temp2[, .(id_pe, no_arbre, ess_arbre = essence_hauteur)]) # ici c'est important d'inclure essence dans le unique, car un même numéro d'arbre peut avoir des essences différentes, à cause des recrues stochastique de samare

    # liste des placettes
    liste_place <- unique(fic_arbres$id_pe)

    # liste des placettes x nb_iter pour accueillir les effets aléatoire de placette
    data_plot <- expand_grid(iter = 1:nb_iter, id_pe = liste_place)

    # liste des arbres x nb_iter pour accueillir les erreurs résiduelles
    data_arbre <- as.data.frame(unclass(expand_grid(iter = 1:nb_iter, liste_arbre)))

    setDT(data_plot)
    setDT(data_arbre)

    # fonction pour générer un element de la matrice de var-cov
    f <- function(i, j, var_res, rho, dt) { var_res * rho^(abs(j-i)*dt) } # correlation sp(pow)

  # faire une essence à la fois et les mettre dans des listes: une liste de listes
  tous <- list()
  for (ess in ht_liste_ess_complet) {

      # lecture des effets fixes pour l'essence
      param2_tr <- ht_param_fixe[[ess]] %>% dplyr::select(-essence)

      # lecture des matrices de covariances des effets fixes de l'essence
      covparam <- ht_param_cov[[ess]]
      covparam$essence <- NULL

      # générer une série de paramètres d'effets fixes, une par itération par essence, qui sera utilisée pour tous les arbres de cette essence de toutes les placettes
      # il faut donc autant de séries qu'il y a d'itérations, les itérations sont en lignes, les effets fixes en colonne
      # pour que mvrnorm() fonctionne avec empirical=T, il faut au moins autant de n que la longueur du vecteur mu à simuler
      mu = as.matrix(param2_tr)
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu} else {nb_iter_temp=nb_iter}
      param_ht = as.data.frame(matrix(rockchalk::mvrnorm(n = nb_iter_temp,
                                              mu = mu,
                                              Sigma = as.matrix(covparam),
                                              empirical = T
                                              ),
                                      nrow=nb_iter_temp))[1:nb_iter,]
      names(param_ht) <- names(param2_tr)
      param_ht <- param_ht %>% mutate(iter = row_number(), essence=ess)

      # ne faire cette partie que pour les essences du fichier
      if (ess %in% ht_liste_ess_temp)
      {
      # fichier des effets aléatoires, corrélation et erreur résiduelle
      rand_ht <- ht_param_random[ht_param_random$essence==ess,] %>% dplyr::select(-essence)

      # générer un effet aléatoire de placette qui va s'appliquer sur le paramètre associé à ldhp2, le même pour tous les arbres d'une essence donnée de la placette
      # il faut donc autant d'effet aléatoire qu'il y a de placettes x nb_iter
      std2 <- rand_ht[rand_ht$CovParm=='ldhp2' & rand_ht$Subject=="placette", 1]
      # générer l'effet aléatoire de placettes seulement pour les placettes où l'ess est présente
      place_ess <- unique(data_arbre[ess_arbre == ess, .(id_pe)])
      if (length(std2$estimate)>0) { random_ldhp2 = as.data.frame(rockchalk::mvrnorm(n=nb_iter*nrow(place_ess), mu=0, Sigma = as.matrix(std2), empirical = T)) } else {random_ldhp2 = as.data.frame(rep(0, nb_iter*nrow(place_ess)))} # le SAB n'a pas d'effet aléatoire
      names(random_ldhp2) <- 'random_ldhp2'
      data_plot2 <- inner_join(data_plot,place_ess, by="id_pe")
      rand_plot = bind_cols(data_plot2,random_ldhp2) %>% mutate(essence=ess)

      # générer une erreur résiduelle à l'échelle de l'arbre, donc autant de ligne que d'arbres  * nb_iter * time step
      varres = rand_ht[rand_ht$CovParm=='Residual', 1]$estimate  # variance residuel
      rho = rand_ht[rand_ht$CovParm=='SP(POW)', 1]$estimate  # corrélation temporelle
      # remplir la matrice de var-cov
      varcov = expand.grid(i=1:nb_step, j=1:nb_step)
      varcov = matrix(f(varcov$i, varcov$j, varres, rho, dt), nrow=nb_step)
      n_arbre=length(liste_arbre$no_arbre)
      mu = rep(0,nb_step)
      l_mu = length(mu)
      if (nb_iter<l_mu) {nb_iter_temp=l_mu} else {nb_iter_temp=nb_iter}
      # pour l'essence ess, sélectionner seulement les arbres de cette ess
      nombre_arbre_ess <- nrow(data_arbre %>% filter(ess_arbre==ess, iter==1))
      residu = as.data.frame(matrix(rockchalk::mvrnorm(n=nb_iter_temp*nombre_arbre_ess, mu=mu, Sigma = varcov, empirical=T), nrow=nb_iter_temp*nombre_arbre_ess))
      #cov_empirique <- cov(residu) # c'est exactement la bonne matrice!

      if (nb_step>1){residu=residu[1:(nb_iter*nombre_arbre_ess),]}
      residu = as.data.frame(bind_cols(data_arbre[data_arbre$ess_arbre==ess,],residu))
      #var_empirique <- var(residu$V1)

      # transposer les erreurs res pour avoir les time step en ligne
      setDT(residu)
      residu <- melt(
        residu,
        id.vars = c("iter", "id_pe", "no_arbre","ess_arbre"),
        measure.vars = patterns("^V"),
        variable.name = "step",
        value.name = 'res_arbre'
      ) # instantanné avec data.table au lieu des plusieurs minutes
      # Convertir "step" en numérique (en supprimant "V")
      residu[, step := as.numeric(gsub("V", "", step))] # 8 sec
      residu$essence <- ess

      }
      if (!(ess %in% ht_liste_ess_temp))
      {
        rand_plot <- NULL
        residu <- NULL
      }
    temp <- list('essence'=ess, 'effet_fixe'=param_ht, 'random_placette'=rand_plot, 'erreur_residuelle'=residu)
    tous <- append(tous, list(temp))


  }
    names(tous) <- ht_liste_ess_complet

  # Faire des tables avec toutes les essences

  # effets fixes, une ligne par essence/iter (essence du modele de ht présentes dans le fic_arbres)
  # il faut faire lapply sur toutes les essences, même si pas dans le fichier, car sinon il va manquer des paramètres pour appliquer l'équation sans tenir compte de l'essence
  param_ht_tr <- bind_rows(lapply(ht_liste_ess_complet, function(x) tous[[x]]$effet_fixe))
  # remplacer tous les NA par des 0
  param_ht_tr <-  param_ht_tr %>% replace(is.na(.), 0)
  # ensuite on peut sélectionner seulement les essences présentes dans le fichier des arbres
  param_ht_tr <- param_ht_tr[param_ht_tr$essence %in% ht_liste_ess_temp,]

  # effet aléatoire: une ligne par placette/essence/iter: toutes les placettes auront toutes les essences du modele de ht présentes dans le fic_arbres
  rand <- bind_rows(lapply(ht_liste_ess_temp, function(x) tous[[x]]$random_placette)) # le fichier random_placette a autant de lignes que de placette x nb_iter dans le fichier arbre, donc random_ldhp2 aura 27 x nb_placette

  # erreur résiduelle: une ligne par arbre/iter/step/essence: tous les arbres/iter auront toutes les essences du modele de ht présentes dans le fic_arbres
  res <- bind_rows(lapply(ht_liste_ess_temp, function(x) tous[[x]]$erreur_residuelle)) # le fichier erreur_residuelle a autant de lignes que le nombre d'arbres dans le fichier arbre x nb_iter x nb_step, donc resid aura 27 x nb arbre
  # ce fichier a trop de lignes (une ligne par arbre/iter/step), car chacun des arbres ne sont pas toujours présents à toutes les itérations ni a toutes les step, en particulier si le fichier provient de Samare qui est stochastique ou de Artémis-STO. Faire le merge correctement à la fin

  # merger effets fixe et random
  param0 <- left_join(rand, param_ht_tr, by = c("essence","iter")) # rapide

  # ajouter erreurs résiduelles
  param1 <- merge(res, param0, by = c("id_pe", "essence", "iter"), all.x=T)


  # pour chaque arbre, ne garder que les paramètres de son essence
  # le fichier aura donc autant de lignes que fic_arbres_temp2a
  param2 <- merge(fic_arbres_temp2a, param1, by = c("id_pe", "no_arbre", "iter", "step", "essence"), all.x=T)
  param2 <- param2[
    , c("essence_orig", "ess_arbre") := NULL
  ]
  }

  if (mode_simul=='DET'){
    # le fichier aura autant de lignes que d'essences du modele de ht fans le fichier d'entrée
    param <- bind_rows(lapply(ht_liste_ess_complet, function(x) ht_param_fixe[[x]] %>% mutate(essence=x, random_ldhp2=0, res_arbre=0))) # il faut le faire sur les 27 ess car il me faut tous les paramètres pour quela fct marche
    # ne garder les essences du fichier
    param <- param %>% filter(essence %in% ht_liste_ess_temp)
    # remplacer tous les NA par des 0
    param2 <-  param %>% replace(is.na(.), 0)

  }
  return(param2)
}

