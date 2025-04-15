test_that("Args are checked properly :", {
  expect_error(AUDITC(X_err, sample.int(2, size = 50, replace = TRUE), label.femme = 1))
  expect_error(AUDITC(X_err_3col, sample.int(2, size = 50, replace = TRUE), label.femme = 1))

  expect_error(CAST(X_err))
  expect_error(CAST(X_err_6col))

  expect_error(Charlson(X_err))
  expect_error(Charlson(X_err_20col))

  expect_error(Elixhauser.CI(X_err))
  expect_error(Elixhauser.CI(X_err_21col))
  expect_error(Elixhauser.CI(X_err_30col))

  expect_error(EDSR(X_err))
  expect_error(EDSR(X_err_21col))

  expect_error(GHQ12(X_err))
  expect_error(GHQ12(X_err_12col))

  expect_error(Glasgow(X_err))
  expect_error(Glasgow(X_err_3col))

  expect_error(HAD(X_err))
  expect_error(HAD(X_err_14col))

  expect_error(HSI(X_err))
  expect_error(HSI(X_err_2col))

  expect_error(MBI(X_err))
  expect_error(MBI(X_err_22col))

  expect_error(OSSS3(X_err))
  expect_error(OSSS3(X_err_3col))

  expect_error(PCLS(X_err))
  expect_error(PCLS(X_err_17col))

  expect_error(ProQOL(X_err))
  expect_error(ProQOL(X_err_30col))

  expect_error(PSS10(X_err))
  expect_error(PSS10(X_err_10col))

  expect_error(SF12(X_err))
  # expect_error(SF12(X_err_12col))

  expect_error(SLS(X_err))
  expect_error(SLS(X_err_3col))

  expect_error(Spiegel(X_err))
  expect_error(Spiegel(X_err_6col))

  expect_error(WCC(X_err))
  expect_error(WCC(X_err_27col))

  expect_error(WEMWBS(X_err))
  expect_error(WEMWBS(X_err_14col))
})


test_that("Output type are right :", {
  expect_true(is.list(auditc))
  expect_true(is.list(cast))
  expect_true(is.list(cci))
  expect_true(is.list(eci))
  expect_true(is.list(edsr))
  expect_true(is.list(ghq12))
  expect_true(is.list(glasgow))
  expect_true(is.list(had))
  expect_true(is.list(hsi))
  expect_true(is.list(imc))
  expect_true(is.list(mbi))
  expect_true(is.list(osss3))
  expect_true(is.list(pcls))
  expect_true(is.list(proqol))
  expect_true(is.list(pss10))
  expect_true(is.list(spiegel))
  expect_true(is.list(wcc))
  expect_true(is.numeric(wemwbs))

  expect_true(length(auditc) == 2 & all(names(auditc) == c("Valeur", "Classe")))
  expect_true(length(cast) == 2 & all(names(cast) == c("Valeur", "Classe")))
  expect_true(length(cci) == 2 & all(names(cci) == c("Score", "Prob_Survie_10ans")))
  expect_true(length(eci) == 2 & all(names(eci) == c("Score", "Prob_Survie_10ans")))
  expect_true(length(edsr) == 3 & all(names(edsr) == c("Valeur", "Dimension", "Classe")))
  expect_true(length(ghq12) == 2 & all(names(ghq12) == c("Valeur", "Classe")))
  expect_true(length(glasgow) == 2 & all(names(glasgow) == c("Valeur", "Classe")))
  expect_true(length(had) == 2 & all(names(had) == c("Valeur", "Classe")))
  expect_true(length(hsi) == 2 & all(names(hsi) == c("Valeur", "Classe")))
  expect_true(length(imc) == 2 & all(names(imc) == c("Valeur", "Classe")))
  expect_true(length(mbi) == 2 & all(names(mbi) == c("Valeur", "Classe")))
  expect_true(length(osss3) == 2 & all(names(osss3) == c("Valeur", "Classe")))
  expect_true(length(pcls) == 2 & all(names(pcls) == c("Valeur", "Classe")))
  expect_true(length(proqol) == 2 & all(names(proqol) == c("Valeur", "Classe")))
  expect_true(length(pss10) == 2 & all(names(pss10) == c("Valeur", "Classe")))
  expect_true(length(spiegel) == 2 & all(names(spiegel) == c("Valeur", "Classe")))
  expect_true(length(wcc) == 2 & all(names(wcc) == c("Valeur", "Classe")))
})

