ContinuityOfCare <- function(w, n = NA,
                vi = NA, vc = NA, vu = NA, vs = NA) {
  return(
    c(
      CPC = vc/w,
      UPC = vu/w,
      HCC = ifelse(vu/w >= 0.5, 1, 0),
      HSC = ifelse(vs/w >= 0.5, 1, 0),
      HI = sum((vi/w)^2),
      CI = ifelse(ifelse(vu/w > 0.5, 1, 0) == 0 & ifelse(vs/w > 0.5, 1, 0) == 0,
                  0,
                  ifelse(ifelse(vu/w > 0.5, 1, 0) == 1, 1, 0.5)),
      COC = (sum((vi/w)^2)-1/w)/(1-(1/w)),
      EK = (w-n)/(w-1),
      MCI = 1-n/(w + 0.1),
      PPC = ifelse(1-n/(w + 0.1) >= 0.66, 1, 0),
      MMCI= (1-n/(w + 0.1)) / (1-1/(w + 0.1)),
      INOP = 1/n
    )
  )
}

# Example for a patient with a care sequence like : ABABBCBD
# A, B and C being diffrent practionners
# Order from most to leat recent (A is the most recent visit)

seq <- "ABABBCBD"
pract <- c("A", "B", "C", "D")
vi <- rep(NA, 4)
for (i in 1:4) {
  vi[i] <- length(lapply(gregexpr(pract[i], seq), attributes)[[1]]$match.length)
}
vi

CoC(w = 8,
    n = 4, # A, B, C and D
    vi = vi,
    vc = 2, # 2 times A
    vu = 4, # 4 times B
    vs = 4) # Assuming every sites are different

CoC(8, 4, vc = 2)
