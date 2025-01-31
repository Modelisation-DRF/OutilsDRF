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
  a12 <- -fic$pred_mm2/alpha*log(fic$HAUTEUR_M/1.3)
  a21 <- a12
  a22 <- fic$pred_mm2 * (log(fic$HAUTEUR_M/1.3))^2
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

# fonction pour passer chacune des essences d'un fichier, ne traite que les essences avec un modele de défilement, élimine les autres, donc garder les autres arbres dans un autre fichier
# fic = data avec les variables nécessaires pour l'utilisation d'un modèle de défilement, une ligne par iter/placette/arbre/hauteur
# variables qui doicent être dans fic:
# essence= code de l'essence en majuscule, ex: SAB
# sdom_bio = sous-domaine: ex: 2EST
# cl_drai = classe de drainage, ex: '2'
# veg_pot = code de végétation potentielle , ex: 'MS2'
# DHP_Ae = dhp de l'arbre en mm
# HT_REELLE_M = hauteur de l'arbre en m
# HAUTEUR_M = hauteur à laquelle on veut estimer le diametre (m)
# nbTi_ha = nombre d'arbres à l'ha dans la placette
# st_ha = surface terrière en m2/ha dans la placette
# ALTITUDE = altitude de la placette (m)

get_diam <- function(fic) {
  setDT(fic)
  if(nrow(fic) == 0) return(data.table())

  # Pre-filter and calculate once
  data_filtre <- fic[essence %in% defil_liste_ess]
  if(nrow(data_filtre) == 0) return(data.table())

  data_filtre[, `:=`(
    z = (HT_REELLE_M - HAUTEUR_M) / (HT_REELLE_M - 1.3),
    x = HAUTEUR_M / HT_REELLE_M,
    DIA_MM = 0
  )]

  # Process all species at once
  result <- data_filtre[, {
    data_ess <- copy(.SD)
    data_ess_na <- data.table()
    data_ess_non_na <- data.table()

    if(.BY$essence != "PIB") {
      # Add group columns
      if(!is.null(defil_group_vp[[.BY$essence]]) && any(!is.na(defil_group_vp[[.BY$essence]]))) {
        group_temp <- copy(defil_group_vp)
        group_temp[, group.veg := get(.BY$essence)]
        data_ess <- data_ess[group_temp[, .(veg_pot, group.veg)], on = "veg_pot", nomatch = 0]
      }

      if(!is.null(defil_group_sd[[.BY$essence]]) && any(!is.na(defil_group_sd[[.BY$essence]]))) {
        group_temp <- copy(defil_group_sd)
        group_temp[, group.sDomBio := get(.BY$essence)]
        data_ess <- data_ess[group_temp[, .(sdom_bio, group.sDomBio)], on = "sdom_bio", nomatch = 0]
      }

      if(!is.null(defil_group_dr[[.BY$essence]]) && any(!is.na(defil_group_dr[[.BY$essence]]))) {
        group_temp <- copy(defil_group_dr)
        group_temp[, group.drainage := get(.BY$essence)]
        data_ess <- data_ess[group_temp[, .(cl_drai, group.drainage)], on = "cl_drai", nomatch = 0]
      }

      data_ess_non_na <- data_ess[complete.cases(data_ess)]
      data_ess_na <- data_ess[!complete.cases(data_ess)]

      if(nrow(data_ess_non_na) > 0) {
        modele <- get_modele(type_modele = "standAndTree", essence = .BY$essence)
        data_ess_non_na[, pred_mm2 := predict(modele, newdata = .SD, level = 0)]
        data_ess_non_na <- correction_biais(type_modele = "complet", data_ess_non_na, essence = .BY$essence)
      }
    }

    if(nrow(data_ess_na) > 0 || .BY$essence == "PIB") {
      data_ess_na <- copy(data_ess)
      modele <- get_modele(type_modele = "treeOnly", essence = .BY$essence)
      data_ess_na[, pred_mm2 := predict(modele, newdata = .SD, level = 0)]
      data_ess_na <- correction_biais(type_modele = "arbre", data_ess_na, essence = .BY$essence)
    }

    combined_data <- rbindlist(list(data_ess_na, data_ess_non_na), fill = TRUE)
    cols_to_keep <- names(combined_data)[!grepl("^group\\.", names(combined_data))]
    combined_data[, .SD, .SDcols = cols_to_keep]
  }, by = essence]

  return(result)
}

calcul_vol_bille <- function(fichier_billes) {
  #Objet data.table, chaque fois que l'on initiera une data.table, nous utiliserons ce code
  setDT(fichier_billes)
  data_billes <- data.table()
  #pour écrire les lignes dans le fichier
  #ratios de tranformations en mètre(on travaille en mètre durant tout le processus, on rechange les valeurs à la sortie)
  ratio_pouce_metre <- 39.3700787
  ratio_cm_metre <- 100
  ratio_mm_metre <- 1000
  #valeur hauteur souche, en mètre
  dhs <- 0.15
  #On garde dans le fichier que les essences valides
  data_filtre <- fichier_billes[essence %in% defil_liste_ess]
  setDT(data_filtre)
  if (nrow(data_filtre) > 0) {
    data_filtre[is.na(nom_grade1), column_name := "sciage court"]
    data_filtre[is.na(long_grade1), column_name := 8]
    data_filtre[is.na(diam_grade1), column_name := 20]
    nom_grade_1 <- data_filtre$nom_grade1
    log_length_1 <- data_filtre$long_grade1
    diam_value_1 <- data_filtre$diam_grade1 / ratio_cm_metre
    nom_grade_2 <- data_filtre$nom_grade2
    log_length_2 <- data_filtre$long_grade2
    diam_value_2 <- data_filtre$diam_grade2 / ratio_cm_metre
    nom_grade_3 <- data_filtre$nom_grade3
    log_length_3 <- data_filtre$long_grade3
    diam_value_3 <- data_filtre$diam_grade3 / ratio_cm_metre
    expanded_data <- data_filtre[, {
      list(HAUTEUR_M = round(seq(dhs, HT_REELLE_M, by = 24.5 / ratio_pouce_metre), 6))
    }, by = .(essence, id_pe, no_arbre, sdom_bio, cl_drai, veg_pot, DHP_Ae, HT_REELLE_M, nbTi_ha, st_ha, ALTITUDE)]
    print("check1")
    timing <- system.time({
       new_data <- expanded_data[, DIAM_PREDICT := {
         temp <- get_diam(.SD)
         sqrt(temp$pred_mm2_corr) / ratio_mm_metre
       }, by = .(id_pe, no_arbre)]
    })
    print(timing)
    print("check1.5")
    new_data1 <- new_data[, .(id_pe = id_pe, no_arbre = no_arbre,
                              HAUTEUR_M = HAUTEUR_M, DIAM_PREDICT = DIAM_PREDICT)]
    setDT(new_data1)
    #on crée une df temporaire contentant seulement les valeurs voulues
    data_first_cut <- data.table()

    nom_grade1 <- nom_grade_1[1]
    log_length1 <- log_length_1[1]
    diam_value1 <- diam_value_1[1]
    nom_grade2 <- nom_grade_2[1]
    log_length2 <- log_length_2[1]
    diam_value2 <- diam_value_2[1]
    nom_grade3 <- nom_grade_3[1]
    log_length3 <- log_length_3[1]
    diam_value3 <- diam_value_3[1]
    #current_hauteur <- dhs
    #n = 1
    #diam_hauteur_1 <- new_data1[HAUTEUR_M == current_hauteur]$pred_mm2_corr
    #diam_hauteur_2 <- new_data1[abs(HAUTEUR_M - (current_hauteur + n * log_length)) < 1e-6]$pred_mm2_corr

    ####### bien séparer chaque arbre pour le calcul
    jump_log1 <- abs(log_length1) / 2
    jump_log2 <- abs(log_length2) / 2
    jump_log3 <- abs(log_length3) / 2
    print("check2")
    data_first_cut <- new_data1[diam_value1 <= DIAM_PREDICT]
    data_first_cut[, cut_type := "cut_1"]  # Assign "cut_1" to initially selected rows
    all_cuts <- data_first_cut

    if (!is.na(diam_value2)){
      temp_data1 <- new_data1[diam_value2 <= DIAM_PREDICT]
      temp_data2 <- temp_data1[DIAM_PREDICT <= diam_value1]
      temp_data2[, cut_type := "cut_2"]
      all_cuts <- rbind(all_cuts, temp_data2)
    }
    if (!is.na(diam_value3)){
      temp_data3 <- new_data1[diam_value3 <= DIAM_PREDICT]
      temp_data4 <- temp_data3[DIAM_PREDICT <= diam_value2]
      temp_data4[, cut_type := "cut_3"]
      all_cuts <- rbind(all_cuts, temp_data4)

    }
    all_cuts <- all_cuts[, .SD, by = .(id_pe, no_arbre)]

    # Process each group separately
    data_check1 <- all_cuts[, {
      # Filter only 'cut_1' within each (id_pe, no_arbre) group
      group_rows <- .SD[cut_type == "cut_1"]
      # Select rows with jump_log1
      selected_rows <- group_rows[seq(1, .N, by = jump_log1)]
      # Find last selected height
      last_selected_height <- max(selected_rows$HAUTEUR_M)

      # Check remaining rows
      remaining_rows <- group_rows[HAUTEUR_M > last_selected_height]

      # If remaining rows exist, select rows at jump_log2 intervals
      if (nrow(remaining_rows) >= 0) {
        remaining_selected <- remaining_rows[, cut_type := "cut_2"]
        rbind(selected_rows, remaining_selected)
      } else {
        selected_rows
      }
    }, by = .(id_pe, no_arbre)]

    setDT(data_check1)
    #on prend les cut2 qu'on avait et on rajoute ceux qu'on vient de trier
    cut_2_original <- all_cuts[cut_type == "cut_2"]
    cut_2_temp <- unique(rbind(data_check1, cut_2_original))
    cut_2_temp <- cut_2_temp[, .SD, by = .(id_pe, no_arbre)]
    cut_2_only <- cut_2_temp[cut_type == "cut_2"]

    data_check2 <- cut_2_only[, {
      # Filter only 'cut_1' within each (id_pe, no_arbre) group
      group_rows <- .SD
      # Select rows with jump_log1
      selected_rows <- group_rows[seq(jump_log2, .N, by = jump_log2)]
      # Find last selected height
      last_selected_height <- max(selected_rows$HAUTEUR_M)

      # Check remaining rows
      remaining_rows <- group_rows[HAUTEUR_M > last_selected_height]

      # If remaining rows exist, select rows at jump_log2 intervals
      if (nrow(remaining_rows) >= 0) {
        remaining_selected <- remaining_rows[, cut_type := "cut_3"]
        rbind(selected_rows, remaining_selected)
      } else {
        selected_rows
      }
    }, by = .(id_pe, no_arbre)]
    setDT(data_check2)

    cut_3_original <- all_cuts[cut_type == "cut_3"]
    cut_3_temp <- rbind(data_check2, cut_3_original)
    cut_3_temp <- cut_3_temp[, .SD, by = .(id_pe, no_arbre)]
    cut_3_only <- cut_3_temp[cut_type == "cut_3"]

    data_check3 <- cut_3_only[, {
      # Filter only 'cut_1' within each (id_pe, no_arbre) group
      group_rows <- .SD
      # Select rows with jump_log1
      selected_rows <- group_rows[seq(jump_log3, .N, by = jump_log3)]

      # If remaining rows exist, select rows at jump_log2 intervals
      selected_rows
    }, by = .(id_pe, no_arbre)]

    setDT(data_check3)
    #all_cuts <- final_data1
    data_list <- list(
      data_check1[cut_type == "cut_1"],
      data_check2[cut_type == "cut_2"],
      data_check3[cut_type == "cut_3"]
    )

    data_complete <- unique(rbindlist(data_list))
    data_complete <- data_complete[order(id_pe, no_arbre)]
    data_complete[, `:=`(
      next_cut_type = shift(cut_type, -1),
      diam_hauteur2 = shift(DIAM_PREDICT, -1)
    ), by = .(id_pe, no_arbre)]

    # Create a column for log_length based on cut_type
    data_complete[, current_log_length := fcase(
      next_cut_type == "cut_1", log_length1 * 12.25/ ratio_pouce_metre,
      next_cut_type == "cut_2", log_length2 * 12.25/ ratio_pouce_metre,
      next_cut_type == "cut_3", log_length3 * 12.25/ ratio_pouce_metre
    )]

    data_complete[, current_grade := fcase(
      next_cut_type == "cut_1", nom_grade1,
      next_cut_type == "cut_2", nom_grade2,
      next_cut_type == "cut_3", nom_grade3
    )]
    # Calculate volumes for consecutive pairs
    data_billes_temp <- data_complete[, `:=`(id_pe = id_pe, no_arbre = no_arbre, grade = current_grade,
                                             volume = ifelse(!is.na(diam_hauteur2),
                                                             pi * current_log_length * (DIAM_PREDICT^2 + diam_hauteur2^2), NA_real_))]

    print("check6")
    data_billes <- na.omit(data_billes_temp)[, .(id_pe, no_arbre, grade, volume)]
  }
  #fwrite(data_billes, "data_billes2.csv", row.names = FALSE)
  return(data_billes)
}

##################################################
#get_diam(data_diam1)
calcul_vol_bille(data_diam6)
#data_diam5
#pretty optimized, need some more maybe?
#9900 in around 110 sec
