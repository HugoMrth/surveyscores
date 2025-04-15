MRMI <- function(gender = NULL,
                      age = NULL,
                      isch.heart = NULL,
                      cerebro.vasc = NULL,
                      heart.fail = NULL,
                      periph.vasc = NULL,
                      diabetes = NULL,
                      cancer = NULL,
                      cancer.hist = NULL,
                      schizo = NULL,
                      depression = NULL,
                      subst.abuse = NULL,
                      dementia = NULL,
                      parkinson = NULL,
                      mult.sclero = NULL,
                      epilespy = NULL,
                      chornic.resp = NULL,
                      infl.bowel = NULL,
                      rheum.arthr = NULL,
                      HIV = NULL,
                      es.renal = NULL,
                      liver.pancr = NULL) {

  gender <- match.arg(gender, choices = c("M", "F"))

  if (!is.numeric(age)) stop("Age must be numeric (years)")

  if (is.null(gender) | is.null(age) | is.null(isch.heart) | is.null(cerebro.vasc) |
      is.null(heart.fail) | is.null(periph.vasc) | is.null(diabetes) | is.null(cancer) |
      is.null(cancer.hist) | is.null(schizo) | is.null(depression) | is.null(subst.abuse) |
      is.null(dementia) | is.null(parkinson) | is.null(mult.sclero) | is.null(epilespy) |
      is.null(chornic.resp) | is.null(infl.bowel) | is.null(rheum.arthr) | is.null(HIV) |
      is.null(es.renal) | is.null(liver.pancr)) {
    stop("All arguments are required")
  }

  if (!(all(isch.heart %in% c(0, 1, TRUE, FALSE)) & all(cerebro.vasc %in% c(0, 1, TRUE, FALSE)) &
      all(heart.fail %in% c(0, 1, TRUE, FALSE)) & all(periph.vasc %in% c(0, 1, TRUE, FALSE)) &
      all(diabetes %in% c(0, 1, TRUE, FALSE)) & all(cancer %in% c(0, 1, TRUE, FALSE)) &
      all(cancer.hist %in% c(0, 1, TRUE, FALSE)) & all(schizo %in% c(0, 1, TRUE, FALSE)) &
      all(depression %in% c(0, 1, TRUE, FALSE)) & all(subst.abuse %in% c(0, 1, TRUE, FALSE)) &
      all(dementia %in% c(0, 1, TRUE, FALSE)) & all(parkinson %in% c(0, 1, TRUE, FALSE)) &
      all(mult.sclero %in% c(0, 1, TRUE, FALSE)) & all(epilespy %in% c(0, 1, TRUE, FALSE)) &
      all(chornic.resp %in% c(0, 1, TRUE, FALSE)) & all(infl.bowel %in% c(0, 1, TRUE, FALSE)) &
      all(rheum.arthr%in% c(0, 1, TRUE, FALSE)) & all(HIV %in% c(0, 1, TRUE, FALSE)) &
      all(es.renal %in% c(0, 1, TRUE, FALSE)) & all(liver.pancr %in% c(0, 1, TRUE, FALSE)))) {
    stop("All arguments accept type must be c(0,1) or c(TRUE, FALSE)")
  }

  ind_mrmi <- ifelse(gender == "M", 1, 0) + floor((age - 65)/5) +
      isch.heart * 0 + cerebro.vasc * 1 + heart.fail * 1 + periph.vasc * 1 +
        diabetes * 1 + cancer * 3 + cancer.hist * 0 + schizo * 1 + depression * 1 +
          subst.abuse * 2 + dementia * 2 + parkinson * 1 + mult.sclero * 2 + epilespy * 1 +
            chornic.resp * 1 +  infl.bowel * 0 + rheum.arthr * 1 + HIV * 0 + es.renal * 2 +
              liver.pancr * 2

 ind_ermi <- ifelse(gender == "M", 0, 0) + floor((age - 65)/5) +
      isch.heart * 2 + cerebro.vasc * 3 + heart.fail * 3 + periph.vasc * 3 +
      diabetes * 4 + cancer * 7 + cancer.hist * 2 + schizo * 6 + depression * 5 +
      subst.abuse * 5 + dementia * 2 + parkinson * 5 + mult.sclero * 9 + epilespy * 3 +
      chornic.resp * 3 +  infl.bowel * 0 + rheum.arthr * 4 + HIV * 10 + es.renal * 16 +
      liver.pancr * 5


  return(list(
    MRMI = ind_mrmi,
    ERMI = ind_ermi
  ))
}
