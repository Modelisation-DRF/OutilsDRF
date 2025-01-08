#' Table avec la liste d'arbres de deux placettes du réseau des placettes temporaire du Québec
#'
#' Deux placettes des PET avec leur liste d'arbres
#'
#' @format ## `fic_arbres_test`
#' A data frame with 79 lignes et 13 colonnes:
#' \describe{
#'   \item{id_pe}{Identifiant de la placette}
#'   \item{sdom_bio}{Code du sous-domaine bioclimatique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{veg_pot}{Code de la végétation potentielle, 3 premiers caractères du type écologique}
#'   \item{milieu}{Code du milieu physique, , 4e caractère du type écologique}
#'   \item{altitude}{Altitude (m)}
#'   \item{t_ma}{Température annuelle moyenne}
#'   \item{tige_ha}{Nombre de tiges dans la classe de DHP=dhpcm et de l'essence=essence}
#'   \item{nb_tige}{tiges_ha converti pour une placette de 400 m2, donc nombre de tiges dans 400m2}
#'   \item{no_arbre}{Identifiant de la combinaison dhpcm/essence}
#'   \item{essence}{Code d'essence}
#'   \item{dhpcm}{Classe de DHP (cm)}
#' }
"fic_arbres_test"



#' Table reproduisant une exportation du simulateur Artémis en mode stochastique
#'
#' Les deux placettes du fichier d'exemple fic_arbres_test, avec 5 pas de simulation et 10 itérations, à l'échelle de l'arbre
#'
#' @format ## `fic_artemis_sto`
#' A data frame with 3950 lignes et 15 colonnes:
#' \describe{
#'   \item{step}{Identifiant du pas de simulation}
#'   \item{iter}{Identifiant de l'itération}
#'   \item{id_pe}{Identifiant de la placette}
#'   \item{sdom_bio}{Code du sous-domaine bioclimatique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{veg_pot}{Code de la végétation potentielle, 3 premiers caractères du type écologique}
#'   \item{milieu}{Code du milieu physique, , 4e caractère du type écologique}
#'   \item{altitude}{Altitude (m)}
#'   \item{t_ma}{Température annuelle moyenne}
#'   \item{tige_ha}{Nombre de tiges dans la classe de DHP=dhpcm et de l'essence=essence}
#'   \item{nb_tige}{tiges_ha converti pour une placette de 400 m2, donc nombre de tiges dans 400m2}
#'   \item{no_arbre}{Identifiant de la combinaison dhpcm/essence}
#'   \item{essence}{Code d'essence}
#'   \item{dhpcm}{Classe de DHP (cm)}
#'   \item{annee}{Année correspondant à la step}
#' }
"fic_artemis_sto"



#' Table reproduisant une exportation du simulateur Artémis en mode déterministe
#'
#' Les deux placettes du fichier d'exemple fic_arbres_test, avec 5 pas de simulation, à l'échelle de l'arbre
#'
#' @format ## `fic_artemis_det`
#' A data frame with 395 lignes et 14 colonnes:
#' \describe{
#'   \item{step}{Identifiant du pas de simulation}
#'   \item{id_pe}{Identifiant de la placette}
#'   \item{sdom_bio}{Code du sous-domaine bioclimatique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{veg_pot}{Code de la végétation potentielle, 3 premiers caractères du type écologique}
#'   \item{milieu}{Code du milieu physique, , 4e caractère du type écologique}
#'   \item{altitude}{Altitude (m)}
#'   \item{t_ma}{Température annuelle moyenne}
#'   \item{tige_ha}{Nombre de tiges dans la classe de DHP=dhpcm et de l'essence=essence}
#'   \item{nb_tige}{tiges_ha converti pour une placette de 400 m2, donc nombre de tiges dans 400m2}
#'   \item{no_arbre}{Identifiant de la combinaison dhpcm/essence}
#'   \item{essence}{Code d'essence}
#'   \item{dhpcm}{Classe de DHP (cm)}
#'   \item{annee}{Année correspondant à la step}
#' }
"fic_artemis_det"


#' Table sans le sous-domaine bioclimatique pour tester l'option reg_eco
#'
#' Une placette à l'échelle de l'arbre
#'
#' @format ## `fic_arbres_reg_eco`
#' A data frame with 79 lignes et 13 colonnes:
#' \describe{
#'   \item{id_pe}{Identifiant de la placette}
#'   \item{reg_eco}{Code de la région écologique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{veg_pot}{Code de la végétation potentielle, 3 premiers caractères du type écologique}
#'   \item{milieu}{Code du milieu physique, , 4e caractère du type écologique}
#'   \item{altitude}{Altitude (m)}
#'   \item{t_ma}{Température annuelle moyenne}
#'   \item{p_tot}{Précipitations totale annuelle}
#'   \item{tige_ha}{Nombre de tiges dans la classe de DHP=dhpcm et de l'essence=essence}
#'   \item{nb_tige}{tiges_ha converti pour une placette de 400 m2, donc nombre de tiges dans 400m2}
#'   \item{no_arbre}{Identifiant de la combinaison dhpcm/essence}
#'   \item{essence}{Code d'essence}
#'   \item{dhpcm}{Classe de DHP (cm)}
#' }
"fic_arbres_reg_eco"

