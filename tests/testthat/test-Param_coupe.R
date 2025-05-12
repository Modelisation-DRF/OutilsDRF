test_that("La fonction param_coupe génère les bons paramètres en mode DET pour trt=0", {

  obtenu <- param_coupe(trt_coupe=0, mode_simul='DET')
  # paramètres qui dépendent de l'essence
  obtenu_ess <- obtenu %>% select(essence, b1_s, b2_s)
  # paramètres qui ne dépendent pas de l'essence
  obtenu_sans_ess <- obtenu %>% select(b0, b4_s, b5_s, b6_s) %>% unique()
  names(obtenu_sans_ess) <- c('b0', 'b4', 'b5', 'b6')

  # paramètre brut dans un data interne: coupe_param
  attendu <- coupe_param %>% filter(num_trt==0)
  attendu <- attendu %>% select(essence, estimate, effect) %>%
    group_by(essence) %>%
    pivot_wider(names_from = effect, values_from = estimate)

  # paramètres qui dépendent de l'essence
  attendu_ess <- attendu %>% filter(!is.na(essence)) %>%
    select(essence, b1_s, b2_s)
  # paramètres qui ne dépendent pas de l'essence
  attendu_sans_ess <- attendu %>% select(essence, b0, b4, b5, b6) %>%
    filter(is.na(essence)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-essence)


  expect_equal(as.data.frame(obtenu_ess), as.data.frame(attendu_ess))
  expect_equal(as.data.frame(obtenu_sans_ess), as.data.frame(attendu_sans_ess))

})



test_that("La fonction param_coupe génère les bons paramètres en mode STO pour trt=0", {

  obtenu <- param_coupe(trt_coupe=0, mode_simul='STO', nb_iter = 500, seed_value = 5)

  # la moyenne des sto doit donner le deterministe
  obtenu <- obtenu %>% select(-code_trt, -num_trt, -iter) %>%
    group_by(essence) %>%
    summarise_all(mean)

  # paramètres qui dépendent de l'essence
  obtenu_ess <- obtenu %>% select(essence, b1_s, b2_s)
  # paramètres qui ne dépendent pas de l'essence
  obtenu_sans_ess <- obtenu %>% select(b0, b4_s, b5_s, b6_s) %>% unique()
  names(obtenu_sans_ess) <- c('b0', 'b4', 'b5', 'b6')

  # paramètre brut dans un data interne: coupe_param
  attendu <- coupe_param %>% filter(num_trt==0)
  attendu <- attendu %>% select(essence, estimate, effect) %>%
    group_by(essence) %>%
    pivot_wider(names_from = effect, values_from = estimate)

  # paramètres qui dépendent de l'essence
  attendu_ess <- attendu %>% filter(!is.na(essence)) %>%
    select(essence, b1_s, b2_s)
  # paramètres qui ne dépendent pas de l'essence
  attendu_sans_ess <- attendu %>% select(essence, b0, b4, b5, b6) %>%
    filter(is.na(essence)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-essence)


  expect_equal(as.data.frame(obtenu_ess), as.data.frame(attendu_ess))
  expect_equal(as.data.frame(obtenu_sans_ess), as.data.frame(attendu_sans_ess))

})
