test_that("Attribution de qualite en mode deterministe donne les bons resultats - leger", {
  # test incluant des essences sans équation et des dhp<23
  entree <- ex_qualite #%>% rename(sdom_bio = sdom) # fichier interne sous data
  # summary(entree$dhpcm) # 16 à 41 cm, dont les <23 n'ont pas d'équation
  # table(entree$essence)         # BOJ BOP CHX ERR ERS FEN HEG PEU EPN, donc une essence sans équation
  # table(qualite0_param$essence) # BOJ BOP CHX ERR ERS FEN HEG PEU

  expected <- as.data.frame(readRDS(test_path("fixtures/attrib_qualite", "resultatAttendu_attributionQualite_DET.rds"))) %>% rename(sdom_bio = sdom)
  expected <- expected %>% filter(id_pe==1 & no_arbre<=3 | id_pe==2 & no_arbre <=4 | id_pe==3 & no_arbre<=4)
  actual <- attrib_qualite(entree, "DET")

  expect_equal(actual, expected, ignore_attr = TRUE)
})

# test_that("Attribution de qualite en mode deterministe donne les bons resultats - lourd", {
#   actual <-
#     as.data.frame(readRDS(test_path("fixtures/attrib_qualite", "entreeQualiteLourd.rds"))) %>%
#     rename(sdom_bio=sdom) %>%
#     attrib_qualite("DET", seed_value = 0) %>%
#     lazy_dt() %>%
#     arrange(id_pe, no_arbre) %>%
#     as.data.frame() %>%
#
#   expected <- as.data.frame(readRDS(test_path("fixtures/attrib_qualite", "resultatQualiteLourd.rds"))) %>%
#     lazy_dt() %>%
#     arrange(id_pe, no_arbre) %>%
#     as.data.frame() %>%
#     rename(sdom_bio=sdom)
#
#   expect_equal(actual, expected, ignore_attr = TRUE)
# })

test_that("Attribution de qualite en mode deterministe donne les bons resultats - lourd avec ass ess", {
  actual <-
    as.data.frame(readRDS(test_path("fixtures/attrib_qualite", "entreeQualiteLourd_avec_ass_ess.rds"))) %>%
    select(-sdom) %>%
    attrib_qualite("DET", seed_value = 0) %>%
    lazy_dt() %>%
    arrange(id_pe, no_arbre) %>%
    as.data.frame()

  expected <- as.data.frame(readRDS(test_path("fixtures/attrib_qualite", "resultatQualiteLourd_avec_ass_ess.rds"))) %>%
    lazy_dt() %>%
    arrange(id_pe, no_arbre) %>%
    as.data.frame() %>%
    select(-sdom)

  expect_equal(actual, expected, ignore_attr = TRUE)
})

# test_that("Attribution de qualite en mode stochastique donne les bons resultats - lourd", {
#   set.seed(1234)
#   entree <- readRDS(test_path("fixtures/attrib_qualite", "entreeQualiteLourd.rds"))
#   actual <- attrib_qualite(entree, "STO", nb_iter = 2, seed_value = 0) %>%
#     lazy_dt() %>%
#     arrange(id_pe, no_arbre, iter) %>%
#     as.data.frame()
#
#   expected <- as.data.frame(readRDS(test_path("fixtures/attrib_qualite", "resultatAttributionSTO.rds")))
#
#   expect_equal(actual, expected)
# })


test_that("Attribution de qualite en mode stochastique donne les bons resultats - lourd", {
  set.seed(1234)
  entree <- readRDS(test_path("fixtures/attrib_qualite", "entreeQualiteLourd_avec_ass_ess.rds")) %>% select(-sdom)
  entree1 <- entree %>% mutate(iter=1)
  entree2 <- entree %>% mutate(iter=2)
  entree <- bind_rows(entree1, entree2)
  actual <- attrib_qualite(entree, "STO", nb_iter = 2, seed_value = 1234)
#  actual <- actual %>% arrange(id_pe, no_arbre, iter)
  expected <- as.data.frame(readRDS(test_path("fixtures/attrib_qualite", "resultatAttributionSTO_avec_ass_ess.rds")))

  expect_equal(actual, expected)
})

test_that("Attribution de qualite en mode stochastique fonctionne avec nb_iter=1", {
  set.seed(1234)
  entree <- readRDS(test_path("fixtures/attrib_qualite", "entreeQualiteLourd_avec_ass_ess.rds")) %>% select(-sdom) %>% mutate(iter=1)
  expect_no_error(attrib_qualite(entree, "STO", nb_iter = 1, seed_value = 1234))
  actual <- attrib_qualite(entree, "STO", nb_iter = 1, seed_value = 1234)
  expect_equal(nrow(entree), nrow(actual))
})
