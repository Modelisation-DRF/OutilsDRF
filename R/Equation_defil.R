# fonction pour préparer les variables pour la correction de biais d'une prediction d'un diam avec un des 2 types de modèles
# type_modele = 'arbre' ou 'complet'
# fic = data avec les arbres et contenant la prédiction pred_mm2
# essence = code d'essence en majuscule
correction_biais <- function(type_modele, fic, essence) {
  # type_modele = 'arbre'; fic=data_ess_na; essence='PIG'

  # aller chercher les paramètres du modèle demandé
  random_arbre <- get(paste('defil_random_arbre', type_modele, essence, sep='_'))
  random_plot <- get(paste('defil_random_plot', type_modele, essence, sep='_'))
  alpha <- as.numeric(get(paste('defil_alpha', type_modele, essence, sep='_')))

  sigma_arbre1 <- as.numeric(sqrt(random_arbre[1,1]))
  sigma_arbre2 <- as.numeric(sqrt(random_arbre[2,2]))
  corr_arbre <- as.numeric(random_arbre[2,1]/(sigma_arbre1*sigma_arbre2))
  if (is.na(corr_arbre)) {corr_arbre <- 0}
  sigma_plot1 <- as.numeric(sqrt(random_plot[1,1]))
  sigma_plot2 <- as.numeric(sqrt(random_plot[2,2]))
  corr_plot <- as.numeric(random_plot[2,1]/(sigma_plot1*sigma_plot2))
  if (is.na(corr_plot)) {corr_plot <- 0}

  setDT(fic)
  a12 <- -fic$pred_mm2/alpha*log(fic$HT_REELLE_M/1.3)
  a21 <- a12
  a22 <- fic$pred_mm2 * (log(fic$HT_REELLE_M/1.3))^2
  correction <- 0.5 * (a12*corr_plot*sigma_plot1*sigma_plot2 + a21*corr_plot*sigma_plot1*sigma_plot2 + a22*sigma_plot2^2)
  + 0.5 * (a12*corr_arbre*sigma_arbre1*sigma_arbre2 + a21*corr_arbre*sigma_arbre1*sigma_arbre2 + a22*sigma_arbre2^2)
  pred_mm2_corr <- fic$pred_mm2+correction

  new_columns1 <- data.table(correction = correction, pred_mm2_corr = pred_mm2_corr)

  fic_biais <- cbind(fic, new_columns1)

  return(fic_biais)
}
#correction_biais(type_modele='arbre', fic=data_ess_na, essence='PIG')


# fonction pour loader le modele de l'essence traitée et du modele désiré
# type_modele = 'standAndTree' ou 'treeOnly'
# essence = code d'essence en

get_modele <- function(type_modele, essence) {
  # type_modele = 'treeOnly'; essence='PIG';

  # aller chercher le modele et les parametres de l'essence traitée
  modele <- get(paste0("defil_", type_modele, '.', tolower(essence)))

  return(modele)
}

#' Calcule les diamètres à différentes hauteurs selon des modèles de défilement
#'
#' Cette fonction traite les essences forestières qui possèdent un modèle de défilement
#' et élimine les autres. Les essences sans modèle(pas dans defil_liste_ess) doivent être conservées dans un
#' fichier séparé.
#'
#' @param fic data.table contenant les variables nécessaires pour utiliser un modèle
#'   de défilement (une ligne par iter/placette/arbre/hauteur)
#'
#' @details
#' Les variables suivantes doivent être présentes dans le paramètre `fic`:
#' \itemize{
#'   \item essence: code de l'essence en majuscule (ex: SAB)
#'   \item sdom_bio: sous-domaine bioclimatique (ex: 2EST)
#'   \item cl_drai: classe de drainage (ex: '2')
#'   \item veg_pot: code de végétation potentielle (ex: 'MS2')
#'   \item DHP_Ae: dhp de l'arbre en mm
#'   \item HT_REELLE_M: hauteur de l'arbre en m
#'   \item HAUTEUR_M: hauteur à laquelle on veut estimer le diamètre (m)
#'   \item nbTi_ha: nombre d'arbres à l'ha dans la placette
#'   \item st_ha: surface terrière en m2/ha dans la placette
#'   \item ALTITUDE: altitude de la placette (m)
#' }
#'
#' @return Une data.table contenant les données filtrées avec une colonne supplémentaire
#'   pred_mm2_corr représentant le diamètre calculé en mm2
#'
#' @examples
#'   \dontrun{
#'     # Préparation des données d'entrée(exemple)
#'     # Si valeur NA, l'utilisateur n'a pas donnée de valeur pour le paramètre correspondant.
#'     donnees_arbre <- data.frame(
#'       essence = rep(c('BOP'), 3),
#'       id_pe = rep(1, 3),
#'       no_arbre = 1:3,
#'       sdom_bio = rep(c("3OUEST"), 3),
#'       cl_drai = rep(NA, 3),
#'       veg_pot = rep('MS2', 3),
#'       DHP_Ae = c(120, 150, 300),
#'       HT_REELLE_M = rep(0, 3),
#'       HAUTEUR_M = c(13, 20, 28),
#'       nbTi_ha = NA,
#'       st_ha = NA,
#'       ALTITUDE = NA
#'     )
#'
#'     # Calcul du diamètre des sections
#'     resultats <- get_diam(donnees_arbre)
#'   }
#'
#' @import data.table
#' @export
get_diam <- function(fic) {
  # Assure que l'objet est bien une data.table pour optimiser les opérations
  setDT(fic)
  # Retourne une data.table vide si l'entrée est vide
  if(nrow(fic) == 0) return(data.table())

  # Filtre les données pour ne garder que les essences présentes dans la liste définie
  data_filtre <- fic[essence %in% defil_liste_ess]

  # Calcule et ajoute de nouvelles colonnes nécessaires au modèle:
  # - z: ratio de la position sur la hauteur relative (utilisé dans les équations de défilement)
  # - x: ratio de la hauteur réelle sur la hauteur totale
  # - DIA_MM: initialise le diamètre à 0 (sera calculé plus tard)
  # L'opérateur := modifie la data.table en place (sans créer de copie)
  data_filtre[, `:=`(
    z = (HAUTEUR_M - HT_REELLE_M) / (HAUTEUR_M - 1.3),
    x = HT_REELLE_M / HAUTEUR_M,
    DIA_MM = 0
  )]

  # Retourne une data.table vide si après filtrage aucune donnée ne reste
  if(nrow(data_filtre) == 0) return(data.table())

  # Extrait la liste des essences uniques et sépare PIB des autres essences
  # PIB (pin blanc) a un traitement particulier dans l'algorithme
  unique_essences <- unique(data_filtre$essence)
  non_pib_essences <- unique_essences[unique_essences != "PIB"]

  # Pré-calcule tous les modèles utilisés pour chaque essence
  # Cela évite de rappeler get_modele() plusieurs fois pour la même essence
  models <- list()
  for(e in unique_essences) {
    if(e != "PIB") {
      # Pour les essences non-PIB, deux modèles sont nécessaires:
      # - standAndTree: utilise les variables peuplement et arbre
      # - treeOnly: utilise uniquement les variables de l'arbre (cas incomplets)
      models[[paste0(e, "_standAndTree")]] <- get_modele("standAndTree", e)
      models[[paste0(e, "_treeOnly")]] <- get_modele("treeOnly", e)
    } else {
      # Pour PIB, seul le modèle "treeOnly" est utilisé
      models[["PIB_treeOnly"]] <- get_modele("treeOnly", e)
    }
  }

  # Pré-construit toutes les tables de jointure nécessaires
  # Ces tables permettent d'associer les groupes écologiques aux essences
  join_tables <- list()

  # Pour chaque essence (sauf PIB), prépare les tables de jointure
  for(curr_essence in non_pib_essences) {
    join_tables[[curr_essence]] <- list()

    # Table pour les groupes de végétation potentielle
    if(!is.null(defil_group_vp[[curr_essence]]) && any(!is.na(defil_group_vp[[curr_essence]]))) {
      vp_cols <- defil_group_vp[[curr_essence]]
      if(length(vp_cols) > 0) {
        # Crée une table contenant la végétation potentielle et son groupe associé
        vp_table <- data.table(
          veg_pot = defil_group_vp$veg_pot,
          group.veg = vp_cols
        )
        join_tables[[curr_essence]][["vp"]] <- vp_table
        # Définit la clé pour optimiser les jointures futures
        setkey(vp_table, veg_pot)
      }
    }

    # Table pour les groupes de domaine bioclimatique
    if(!is.null(defil_group_sd[[curr_essence]]) && any(!is.na(defil_group_sd[[curr_essence]]))) {
      sd_cols <- defil_group_sd[[curr_essence]]
      if(length(sd_cols) > 0) {
        # Crée une table contenant le domaine bioclimatique et son groupe associé
        sd_table <- data.table(
          sdom_bio = defil_group_sd$sdom_bio,
          group.sDomBio = sd_cols
        )
        join_tables[[curr_essence]][["sd"]] <- sd_table
        setkey(sd_table, sdom_bio)
      }
    }

    # Table pour les groupes de drainage
    if(!is.null(defil_group_dr[[curr_essence]]) && any(!is.na(defil_group_dr[[curr_essence]]))) {
      dr_cols <- defil_group_dr[[curr_essence]]
      if(length(dr_cols) > 0) {
        # Crée une table contenant la classe de drainage et son groupe associé
        dr_table <- data.table(
          cl_drai = defil_group_dr$cl_drai,
          group.drainage = dr_cols
        )
        join_tables[[curr_essence]][["dr"]] <- dr_table
        setkey(dr_table, cl_drai)
      }
    }
  }

  # Pré-alloue la liste des résultats pour chaque essence
  # Cela évite de redimensionner la liste à chaque ajout
  results_list <- vector("list", length(unique_essences))
  names(results_list) <- unique_essences

  # Traitement spécial pour l'essence PIB (pin blanc)
  if("PIB" %in% unique_essences) {
    # Extrait les données spécifiques à PIB
    pib_data <- data_filtre[essence == "PIB"]
    if(nrow(pib_data) > 0) {
      # Utilise le modèle pré-calculé pour PIB
      pib_model <- models[["PIB_treeOnly"]]
      # Prédit le diamètre en mm²
      pib_data[, pred_mm2 := predict(pib_model, newdata = .SD, level = 0)]
      # Applique la correction de biais au modèle arbre seulement
      results_list[["PIB"]] <- correction_biais("arbre", pib_data, "PIB")
    }
  }

  # Traitement des essences non-PIB
  for(curr_essence in non_pib_essences) {
    # Extrait les données spécifiques à l'essence courante
    curr_data <- data_filtre[essence == curr_essence]
    if(nrow(curr_data) == 0) next  # Passe à l'essence suivante si aucune donnée

    # Effectue les jointures avec les tables pré-construites
    if(!is.null(join_tables[[curr_essence]])) {
      # Jointure pour la végétation potentielle
      if(!is.null(join_tables[[curr_essence]][["vp"]])) {
        setkey(curr_data, veg_pot)  # Optimise la jointure
        # Jointure avec la table de végétation potentielle (nomatch=0 ignore les non-correspondances)
        curr_data <- curr_data[join_tables[[curr_essence]][["vp"]], nomatch = 0]
      }

      # Jointure pour le sous-domaine
      if(!is.null(join_tables[[curr_essence]][["sd"]])) {
        setkey(curr_data, sdom_bio)
        curr_data <- curr_data[join_tables[[curr_essence]][["sd"]], nomatch = 0]
      }

      # Jointure pour le drainage
      if(!is.null(join_tables[[curr_essence]][["dr"]])) {
        setkey(curr_data, cl_drai)
        curr_data <- curr_data[join_tables[[curr_essence]][["dr"]], nomatch = 0]
      }
    }

    # Sépare les cas complets (toutes variables disponibles) des cas incomplets
    complete_cases <- curr_data[complete.cases(curr_data)]
    incomplete_cases <- curr_data[!complete.cases(curr_data)]

    result_parts <- list()

    # Traite les cas complets avec le modèle "standAndTree"
    if(nrow(complete_cases) > 0) {
      model_key <- paste0(curr_essence, "_standAndTree")
      # Prédit le diamètre au carré en mm²
      complete_cases[, pred_mm2 := predict(models[[model_key]], newdata = .SD, level = 0)]
      # Applique la correction de biais pour le modèle complet
      result_parts$complete <- correction_biais("complet", complete_cases, curr_essence)
    }

    # Traite les cas incomplets avec le modèle "treeOnly"
    if(nrow(incomplete_cases) > 0) {
      model_key <- paste0(curr_essence, "_treeOnly")
      # Prédit le diamètre au carré en mm²
      incomplete_cases[, pred_mm2 := predict(models[[model_key]], newdata = .SD, level = 0)]
      # Applique la correction de biais pour le modèle arbre seulement
      result_parts$incomplete <- correction_biais("arbre", incomplete_cases, curr_essence)
    }

    # Combine les résultats des cas complets et incomplets pour cette essence
    if(length(result_parts) > 0) {
      results_list[[curr_essence]] <- rbindlist(result_parts, fill = TRUE)
    }
  }

  # Filtre les éléments NULL de la liste de résultats
  results_list <- results_list[!sapply(results_list, is.null)]

  # Combine tous les résultats et retourne la data.table finale
  if(length(results_list) > 0) {
    return(rbindlist(results_list, fill = TRUE))
  } else {
    return(data.table())  # Retourne une data.table vide si aucun résultat
  }
}
