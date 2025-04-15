adherence <- function(
    n.days.supplied = NA,
    n.days.gaps = NA,
    n.days.period = NA,
    n.days.last.supply = NA,
    surplus = NA,
    diff.first.last.supply = NA,
    quantity.supplied = NA,
    quantity.per.day = NA) {

  CMA <- n.days.supplied / n.days.period
  CMG <- n.days.gaps / n.days.period
  CMOS <- ifelse(is.na(surplus), n.days.gaps, -surplus)
  CMOS <- CMOS / n.days.period
  CR <- (n.days.supplied - n.days.last.supply) / diff.first.last.supply * 100
  CSA <- n.days.supplied / n.days.period
  DBR <- 1 - ((diff.first.last.supply - n.days.supplied)/diff.first.last.supply) * 100
  MPR <- n.days.supplied / n.days.period
  MPRm <- (n.days.supplied / (diff.first.last.supply + n.days.last.supply)) * 100
  MRA <- n.days.supplied / n.days.period * 100
  PDC <- max(n.days.supplied / n.days.period * 100, 1)
  RCR <- ((quantity.supplied / quantity.per.day) * 100) / diff.first.last.supply

  return(c(
    CMA = CMA,
    CMG = CMG,
    CMOS = CMOS,
    CR = CR,
    CSA = CSA,
    DBR = DBR,
    MPR = MPR,
    MPRm = MPRm,
    MRA = MRA,
    PDC = PDC,
    RCR = RCR
  ))
}
