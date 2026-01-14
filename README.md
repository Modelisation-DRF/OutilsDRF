[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0) [![R-CMD-check](https://github.com/Modelisation-DRF/OutilsDRF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Modelisation-DRF/OutilsDRF/actions/workflows/R-CMD-check.yaml)
## Le package OutilsDRF

Un package contenant plusieurs outils de prédiction pour les simulateurs de croissance forestière de la DRF

Contact: Isabelle Auger - Ministère des Ressources naturelles et des Forêts du Québec

Courriel: isabelle.auger@mrnf.gouv.qc.ca

## Introduction
Le package contient plusieurs outils de prédiction:
Relations hauteur-diamètre pour estimer la hauteur totale en mètre d'un arbre
Tarif de cubage pour estimer le volume marchand brut en dm3 d'un arbre
Simulateur de coupe pour estimer la probabilité d'un arbre d'être coupé lors d'un traitement de récolte
Billonnage à partir d'équations de défilement pour estimer le volume des billes dans un arbre
Équations pour estimer la probabilité d'un arbre d'être coupé suite à un traitement sylvicole donné
Équations pour estimer la qualité ABCD d'un arbre à partir de son code MSCR


## Documentation et références
Auger, I., 2016. Une nouvelle relation hauteur-diamètre tenant compte de l’influence de la station et du climat pour 27 essences commerciales du Québec. Gouvernement du Québec, ministère des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Note de recherche forestière no 146. 31 p.

Fortin, M., J. DeBlois, S. Bernier et G. Blais, 2007. Mise au point d’un tarif de cubage général pour les forêts québécoises : une approche pour mieux évaluer l’incertitude associée aux prévisions. For. Chron. 83: 754-765.

Fortin, M., 2014. Using a segmented logistic model to predict trees to be harvested
in forest growth forecasts. Forest Systems(1), 139-152.

Power, H., 2015. Comparaison des traitements de récolte effectués dans les régions 06 et 07
par l'entreprise "Lauzon-Planchers de bois exclusifs inc." avec les simulations de
traitements génériques CP35_40cm et CP45_40cm. Gouvernement du Québec, Gouvernement du Québec, ministère
des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Avis technique SSRF-7. 16 p.

Schneider, R., M. Fortin, J.-P. Saucier, 2013. Équations de défilement en forêt naturelle pour les principales 
essences commerciales du Québec. Gouvernement du Québec, ministère des Ressources naturelles, Direction de la recherche forestière.
Mémoire de recherche forestière no 167. 40 p.

Schneider, R., M. Fortin, J.-P. Saucier, 2013. Équation de défilement pour le pin gris en peuplement naturel au Québec. Gouvernement du Québec, ministère des Ressources naturelles, Direction de la recherche forestière. Note de recherche forestière no 139. 6 p.

Power, H. et F. Havreljuk, 2018. Predicting hardwood quality and its evolution over time in Quebec’s forests. Forestry 91. 259-270.



## Dépendences
Aucune dépendence à des packages externes à CRAN

## Comment obtenir le code source
Taper cette ligne dans une invite de commande pour cloner le dépôt dans un sous-dossier "outilsdrf":

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
git clone https://github.com/Modelisation-DRF/OutilsDRF outilsdrf
```

## Comment installer le package OutilsDRF dans R

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
library(remotes)
remotes::install_github("Modelisation-DRF/OutilsDRF")
```

## Historique des versions

| Date |  Version  | Issues |      Détails     |
|:-----|:---------:|:-------|:-----------------|
| 2026-01-14 | 2.3.1 |  | Ajout fonctions qualité des arbres |
| 2025-09-19 | 2.2.1 |  | Ménage dans Imports et Depends et création du fichier package.R |
| 2025-05-13 | 2.2.0 |  | Finalisation billonnage et probabilité de coupe et ajout attribution de qualité |
| 2025-01-29 | 2.1.0 |  | ajout parametre use_ass_ess dans cubage et relation_h_d et calcul var dendro maintenant optionnel dans relation_h_d |
| 2025-01-08 | 2.0.0 |  | changement de nom package pour OutilsDRF et ajout équation de défilement et probablité de coupe |
| 2024-12-20 | 1.2.0 |  | utiliser package data.table au lieu de dplyr pour augmenter la vitesse des gros fichiers |
| 2024-05-23 | 1.1.6 |  | corriger fichier association du milieu pour le PEG, il manquait le milieu 9 |
| 2024-05-13 | 1.1.5 |  | corriger erreur fct f qui génère matrice covariances |
| 2024-04-15 | 1.1.4 |  | corriger bug quand nb_step>9 dans ht et vol |
| 2024-03-26 | 1.1.3 |  | déplacer les packages de depends à imports dans DESCRIPTION, utiliser la fct mvrnorm de rockchalk au lieu de MASS |
| 2024-02-22 | 1.1.2 |  | ajout de l'option na.rm=T dans le calcul de la st et densité de chaque placette |
| 2024-02-20 | 1.1.1 |  | correction de bugs mineurs détectés en utilisant un fichier de samare avec peu d'essences |
| 2024-02-08 | 1.1.0 | issue #1  | amélioration de la vitesse d'exécution en mode stochastique |
| 2023-11-30 | 1.0.0 | | première version stable |

