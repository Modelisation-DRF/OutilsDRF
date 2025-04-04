################################################################
#   ISABELLE AUGER                                             #
#                                                              #
#   isabelle.augere@mrnf.gouv.qc.ca                            #
#    last udate       July 2023                                #
#                                                              #
#                                                              #
#                                                              #
#  Utilise                                                     #
#       ht_ass_ess.rda                                         #
#       ht_ass_mil.rda                                         #
#       ht_ass_vp.rda                                          #
#       ht_ass_sd.rda                                          #
#       ht_ass_pert.rda                                        #
#       ht_liste_ess.rda                                       #
#       regeco_ass_sdom.rda                                    #
#                                                              #
################################################################


#' Estime la hauteur totale de chacun des arbres avec l'équation de Auger (2016).
#'
#' @description Estime la hauteur totale en mètre de chacun des arbres avec l'équation de Auger (2016). La fonction permet l'estimation pour une liste d'arbres regroupés en placette.
#' L'estimation peut être déterministe ou stochastique.
#'
#' @details
#' L'équation pour estimer la hauteur totale a été étalonnée avec un modèle linéaire mixte, par essence (Auger 2016).
#' Le modèle inclut un effet aléatoire de placette et une corrélation de type CorCar1 sur les erreurs résiduelles.
#' La fonction estime la hauteur de façon déterministe ou stochastique. Si \code{mode_simul}='STO', les paramètres doivent être générés préalablement avec la fonction \code{param_ht}.
#' Voir les exemples.
#'
#' Auger, I., 2016. Une nouvelle relation hauteur-diamètre tenant compte de l’influence de la station et
#' du climat pour 27 essences commerciales du Québec. Gouvernement du Québec, ministère
#' des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Note de recherche forestière no 146. 31 p.
#'
#' @param fic_arbres Une table contenant la liste d'arbres regroupés au minimum en placettes avec les informations suivantes:
#' \itemize{
#'    \item id_pe: identifiant unique de la placette
#'    \item dhpcm: dhp (cm) de l'arbre ou classe de dhp (>9 cm)
#'    \item essence: code d'essence de l'arbre (ex: SAB, EPN, BOP)
#'    \item no_arbre: identifiant de l'arbre ou de la combinaison dhp/essence, nécessaire seulement si \code{mode_simul}='STO'
#'    \item nb_tige: nombre d'arbres de l'essence et de la classe de dhp, dans 400 m2 (pour calculer la surface terrière de la placette)
#'    \item sdom_bio: sous-domaine bioclimatique, en majuscule (ex: 1, 2E, 4O), seuls les domaines 1 à 6 sont traités
#'    \item veg_pot: code de végétation potentielle, 3 premiers caractères du type écologique  (ex: FE3, MS2)
#'    \item milieu: 4e caractère du type écologique, doit être caractère, une valeur de 0 à 9 (ex: 2)
#'    \item p_tot: précipitation totale annuelle moyenne sur la période 1980-2010 (mm)
#'    \item t_ma: température annuelle moyenne sur la période 1980-2010 (Celcius)
#'    \item altitude: altitude (m)
#'    \item reg_eco: Optionnel. Code de la région écologique. Vous pouvez fournir la région au lieu du sous-domaine, alors mettre le paramètre \code{reg_eco=TRUE}
#'    \item sum_st_ha: Optionel. Surface terrière marchande de la placette (m2/ha). Si non fournie, elle sera calculée à partir des arbres du fichier \code{fic_arbres}
#'    \item dhp_moy: Optionel. Diamètre quadratique moyen des arbres marchands de la placette (cm). Si non fourni, il sera calculé à partir des arbres du fichier \code{fic_arbres}
#'    \item iter: numéro de l'itération, seulement si mode stochastique, doit être numéroté de 1 à nb_iter
#'    \item step: numéro de la step, seulement si mode stochastique, doit être numéroté de 1 à nb_step. Obligatoire même si le fichier n'est qu'une liste d'arbres à un moment donné.
#' }
#' @param mode_simul Mode de simulation : STO = stochastistique, DET = déterministe), par défaut "DET"
#' @param grouping_vars Optionel. Si \code{mode_simul}='DET', les colonnes à ajouter comme variables de groupement, en plus de id_pe, pour calculer la surface terrière d'une placette.
#' Par exemple, si le fichier des arbres contient plus d'une année par arbre, ajouter la colonne identifiant l'année comme variable de groupement: grouping_vars='var1'.
#' S'il y a plusieurs variables de groupement: grouping_vars=c('var1','var2').
#' @param nb_iter Le nombre d'itérations si le mode stochastique est utilisé, doit être > 1. Ignoré si \code{mode_simul="DET"},
#' @param nb_step Le nombre d'années pour lesquelles on veut estimer la hauteur pour un même arbre (par défaut 1), ignoré si \code{mode_simul="DET"}.
#' @param dt La durée de l'intervalle de temps entre deux mesures d'un même arbre si \code{nb_step>1} (par défaut 10), ignoré si \code{mode_simul="DET"}.
#' @param seed_value Optionnel. La valeur du seed pour la génération de nombres aléatoires. Généralement utilisé pour les tests de la fonction.
#' @param reg_eco Optionel. Mettre à \code{TRUE} si reg_eco est fourni dans \code{fic_arbres} au lieu de sdom_bio. reg_eco sera converti en sdom_bio. La colonne sdom_bio ne doit pas être dans \code{fic_arbres}.
#' @param use_ass_ess binaire
#' \itemize{
#'    \item TRUE: Par défaut. les essences sans relation h-d seront associées à une des 27 essences avec le fichier d'association interne.
#'    \item FALSE: les essences sans relation h-d n'auront pas de hauteur estimée
#'    }
#'
#' @return La table \code{fic_arbres} avec une colonne contenant la hauteur estimée en mètres (hauteur_pred).
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' # Exemple 1: DETERMINISTE: un seule année par arbre ---------------------------------------
#' DataHt <- relation_h_d(fic_arbres = fic_arbres_test)
#'
#' # Exemple 2: DETERMINISTE: avec grouping_vars, plusieurs années par arbre ----------------
#' DataHt <- relation_h_d(fic_arbres = fic_artemis_det, grouping_vars = "annee")
#'
#' # Exemple 3: STOCHASTIQUE: plusieurs années par arbre ------------------------------------
#' nb_iter <- length(unique(fic_artemis_sto$iter))
#' nb_step <- length(unique(fic_artemis_sto$step))
#' DataHt <- relation_h_d(
#'   fic_arbres = fic_artemis_sto, mode_simul = "STO",
#'   nb_iter = nb_iter, nb_step = nb_step
#' )
#' }
#'
relation_h_d <- function(fic_arbres, mode_simul = "DET", nb_iter = 1, nb_step = 1, dt = 10, seed_value = NULL, grouping_vars = NULL, reg_eco = FALSE, use_ass_ess = TRUE) {
  # fic_arbres=fic_arbres_test; mode_simul="DET"; nb_iter=1; nb_step=1; dt=10; seed_value=NULL; grouping_vars=NULL; reg_eco=FALSE;

  # le parametre grouping_vars ne peut pas etre utilisé avec le mode stochastique
  # en mode stochastique, les variables iter et step sont obligatoires
  if (mode_simul == "STO") {
    if (length(grouping_vars) > 0) {
      stop("grouping_vars ne peut pas etre utilise avec mode_simul=STO")
    }
    if (length(setdiff(c("iter", "step"), names(fic_arbres))) > 0) {
      stop("les colonnes iter et step doivent etre dans fic_arbres avec mode_simul=STO")
    }
  }

  if (mode_simul == "STO") {
    grouping_vars <- c("id_pe", "iter", "step")
  }
  if (mode_simul == "DET") {
    grouping_vars <- c("id_pe", grouping_vars)
  }

  # générer les paramètres de la relation h_d
  parametre_ht <- param_ht(fic_arbres = fic_arbres, mode_simul = mode_simul, nb_iter = nb_iter, nb_step = nb_step, dt = dt, seed_value = seed_value)

  setDT(fic_arbres)

  # si reg_eco est fourni, faire l'association entre reg_eco et sdom_bio
  if (reg_eco == TRUE) {
    regeco_ass_sdom2 <- regeco_ass_sdom
    setDT(regeco_ass_sdom2)
    fic_arbres <- fic_arbres[regeco_ass_sdom2[, .(reg_eco, sdom_bio)], on = "reg_eco", nomatch = 0]
  }

  # Si sum_st_ha et/ou dhp_moy ne sont dans le fichier, on calcule le 2
  nom <- c("sum_st_ha", "dhp_moy")
  nom_fic <- names(fic_arbres)
  calcule_var_dendro <- FALSE
  if (!(nom[1] %in% nom_fic) | !(nom[2] %in% nom_fic)) {
    fic_arbres[, `:=`(
      sum_st_ha = sum(pi * (dhpcm / 2 / 100)^2 * nb_tige * 25, na.rm = TRUE),
      dens = sum(nb_tige * 25, na.rm = TRUE)
    ), by = grouping_vars][
      , `:=`(
        dhp_moy = sqrt((sum_st_ha * 40000) / (dens * pi))
      )
    ]

    calcule_var_dendro <- TRUE
  }

  # preparer les autres variables necessaires
  arbre2 <- fic_arbres[, `:=`(
    type_eco4 = milieu,
    milieu = NULL,
    sdom_orig = sdom_bio,
    logdhp = log(dhpcm + 1),
    cl_perturb = "NON",
    rdhp = dhpcm / dhp_moy,
    sdom_bio = fifelse(
      sdom_bio == "1", "1OUEST",
      fifelse(
        sdom_bio == "2E", "2EST",
        fifelse(
          sdom_bio == "2O", "2OUEST",
          fifelse(
            sdom_bio == "3E", "3EST",
            fifelse(
              sdom_bio == "3O", "3OUEST",
              fifelse(
                sdom_bio == "4E", "4EST",
                fifelse(
                  sdom_bio == "4O", "4OUEST",
                  fifelse(
                    sdom_bio == "5E", "5EST",
                    fifelse(
                      sdom_bio == "5O", "5OUEST",
                      fifelse(
                        sdom_bio == "6E", "6EST",
                        fifelse(sdom_bio == "6O", "6OUEST", NA)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )]


  ht_ass_ess2 <- ht_ass_ess
  ht_ass_pert2 <- ht_ass_pert
  ht_ass_mil2 <- ht_ass_mil
  ht_ass_sd2 <- ht_ass_sd
  ht_ass_vp2 <- ht_ass_vp
  setDT(ht_ass_ess2)
  setDT(ht_ass_pert2)
  setDT(ht_ass_mil2)
  setDT(ht_ass_sd2)
  setDT(ht_ass_vp2)

  # ajouter l'essence associée au modèle de hauteur
  if (use_ass_ess == T) {
    arbre2 <- merge(arbre2, ht_ass_ess2, by = "essence", all.x = T) # je veux un vrai left_join
    arbre2[, `:=`(
      essence_orig = essence,
      essence = essence_hauteur
    )][, essence_hauteur := NULL]
  } else {
    arbre2[, `:=`(essence_orig = essence)]
  }


  # association des classes des variables categoriques selon l'essence au fichier des arbres
  # chacun des left_join sont interminables, des heures, alors que 20 sec avec merge de data.table
  arbre2 <- merge(arbre2, ht_ass_pert2, by = c("cl_perturb", "essence"), all.x = T)
  arbre2 <- merge(arbre2, ht_ass_mil2, by = c("type_eco4", "essence"), all.x = T)
  arbre2 <- merge(arbre2, ht_ass_sd2, by = c("sdom_bio", "essence"), all.x = T)
  arbre2 <- merge(arbre2, ht_ass_vp2, by = c("veg_pot", "essence"), all.x = T)

  setDT(parametre_ht)

  # merger le fichier des parametres au fichier des arbres
  if (mode_simul == "DET") {
    arbre2 <- merge(arbre2, parametre_ht, by = c("essence"), all.x = T)
  }
  if (mode_simul == "STO") {
    arbre2 <- merge(arbre2, parametre_ht, by = c("iter", "step", "id_pe", "no_arbre", "essence"), all.x = T)
  }

  # appliquer l'equation
  arbre2[
    , `:=`(
      # Colonnes calculées
      eq_ldhp = ef_ldhp * logdhp,
      eq_alt = ef_alt * altitude * logdhp,
      eq_ptot = ef_ptot * p_tot * logdhp,
      eq_tmoy = ef_tmoy * t_ma * logdhp,
      eq_st = ef_st * sum_st_ha * logdhp,
      eq_rdhp = ef_rdhp * rdhp * logdhp,
      eq_ldhp2 = (ef_ldhp2 + random_ldhp2) * logdhp^2,
      eq_pert = fifelse(
        pert == "NON", pert_NON * logdhp,
        fifelse(
          pert == "INT", pert_INT * logdhp,
          fifelse(pert == "MOY", pert_MOY * logdhp, NA_real_)
        )
      ),
      eq_sdom = fifelse(
        sdom == "1", sd_1 * logdhp,
        fifelse(
          sdom == "2EST", sd_2EST * logdhp,
          fifelse(
            sdom == "2OUEST", sd_2OUEST * logdhp,
            fifelse(
              sdom == "3EST", sd_3EST * logdhp,
              fifelse(
                sdom == "3OUEST", sd_3OUEST * logdhp,
                fifelse(
                  sdom == "4EST", sd_4EST * logdhp,
                  fifelse(
                    sdom == "4OUEST", sd_4OUEST * logdhp,
                    fifelse(
                      sdom == "5EST", sd_5EST * logdhp,
                      fifelse(
                        sdom == "5OUEST", sd_5OUEST * logdhp,
                        fifelse(
                          sdom == "6EST", sd_6EST * logdhp,
                          fifelse(sdom == "6OUEST", sd_6OUEST * logdhp, NA_real_)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      eq_mil = fifelse(
        milieu == "0", mil_0 * logdhp,
        fifelse(
          milieu == "1", mil_1 * logdhp,
          fifelse(
            milieu == "2", mil_2 * logdhp,
            fifelse(
              milieu == "3", mil_3 * logdhp,
              fifelse(
                milieu == "4", mil_4 * logdhp,
                fifelse(
                  milieu == "5", mil_5 * logdhp,
                  fifelse(
                    milieu == "6", mil_6 * logdhp,
                    fifelse(
                      milieu == "7", mil_7 * logdhp,
                      fifelse(
                        milieu == "8", mil_8 * logdhp,
                        fifelse(milieu == "9", mil_9 * logdhp, NA_real_)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      eq_vp = fifelse(
        vp == "FC1", vp_FC1 * logdhp,
        fifelse(
          vp == "FE1", vp_FE1 * logdhp,
          fifelse(
            vp == "FE2", vp_FE2 * logdhp,
            fifelse(
              vp == "FE3", vp_FE3 * logdhp,
              fifelse(
                vp == "FE4", vp_FE4 * logdhp,
                fifelse(
                  vp == "FE5", vp_FE5 * logdhp,
                  fifelse(
                    vp == "FE6", vp_FE6 * logdhp,
                    fifelse(
                      vp == "FO1", vp_FO1 * logdhp,
                      fifelse(
                        vp == "ME1", vp_ME1 * logdhp,
                        fifelse(
                          vp == "MF1", vp_MF1 * logdhp,
                          fifelse(
                            vp == "MJ1", vp_MJ1 * logdhp,
                            fifelse(
                              vp == "MJ2", vp_MJ2 * logdhp,
                              fifelse(
                                vp == "MS1", vp_MS1 * logdhp,
                                fifelse(
                                  vp == "XS2" & essence == "SAB", vp_XS2 * logdhp,
                                  fifelse(
                                    vp == "MS2" & essence != "SAB", vp_MS2 * logdhp,
                                    fifelse(
                                      vp == "MS4", vp_MS4 * logdhp,
                                      fifelse(
                                        vp == "MS6", vp_MS6 * logdhp,
                                        fifelse(
                                          vp == "RB1", vp_RB1 * logdhp,
                                          fifelse(
                                            vp == "RB5", vp_RB5 * logdhp,
                                            fifelse(
                                              vp == "RC3", vp_RC3 * logdhp,
                                              fifelse(
                                                vp == "RE1", vp_RE1 * logdhp,
                                                fifelse(
                                                  vp == "RE2", vp_RE2 * logdhp,
                                                  fifelse(
                                                    vp == "RE3", vp_RE3 * logdhp,
                                                    fifelse(
                                                      vp == "RE4", vp_RE4 * logdhp,
                                                      fifelse(
                                                        vp == "RP1", vp_RP1 * logdhp,
                                                        fifelse(
                                                          vp == "RS1", vp_RS1 * logdhp,
                                                          fifelse(
                                                            vp == "RS2", vp_RS2 * logdhp,
                                                            fifelse(
                                                              vp == "RS3", vp_RS3 * logdhp,
                                                              fifelse(
                                                                vp == "RS4", vp_RS4 * logdhp,
                                                                fifelse(
                                                                  vp == "RS5", vp_RS5 * logdhp,
                                                                  fifelse(vp == "RT1", vp_RT1 * logdhp, NA_real_)
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ]
  # Calcul de hauteur_pred
  arbre2[, hauteur_pred := fifelse(
    !is.na(essence) & dhpcm > 9,
    1.3 + res_arbre + eq_ldhp + eq_alt + eq_ptot + eq_tmoy + eq_st + eq_rdhp + eq_pert + eq_sdom + eq_mil + eq_vp + eq_ldhp2,
    NA_real_
  )][, hauteur_pred := fifelse(hauteur_pred < 1.3, 1.3, hauteur_pred)]
  arbre2[
    , `:=`(
      res_arbre = NULL,
      random_ldhp2 = NULL,
      # dens = NULL,
      # dhp_moy = NULL,
      logdhp = NULL,
      rdhp = NULL,
      # sum_st_ha = NULL,
      pert = NULL,
      milieu = NULL,
      vp = NULL,
      sdom = NULL,
      cl_perturb = NULL,
      sdom_bio = NULL,
      essence = NULL
    )
  ][
    , (names(arbre2)[grepl("sd_|pert_|mil_|vp_|ef_|eq_", names(arbre2))]) := NULL
  ][
    , `:=`(
      essence = essence_orig,
      milieu = type_eco4,
      sdom_bio = sdom_orig
    )
  ][
    , `:=`(
      essence_orig = NULL,
      type_eco4 = NULL,
      sdom_orig = NULL
    )
  ]

  # si les variables dendro ont été calculées, les enlever du fichier
  if (calcule_var_dendro == T) {
    arbre2[
      , `:=`(
        sum_st_ha = NULL,
        dens = NULL,
        dhp_moy = NULL
      )
    ]
  }

  return(arbre2)
}
