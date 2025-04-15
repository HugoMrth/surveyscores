\title{
  Mesures de dispersion dans la continuité des soins
}\name{ContinuityOfCare}
\alias{ContinuityOfCare}
\description{
  Compile plusieurs mesures de dispersion de continiuité des soins.
}\usage{
  ContinuityOfCare(
    w, 
    n = NA,
    vi = NA, 
    vc = NA, 
    vu = NA, 
    vs = NA)
}\arguments{
  \item{w}{
Nombre total de visites
}  \item{n}{
Nombre total de professionnel visités
}  \item{vi}{
Id des visites
}  \item{vc}{
Le dernier professionnel vu 
}  \item{vu}{
Le professionnel vu le plus fréquemment 
}  \item{vs}{
Site de soin le plus visité 
}
}\value{
  Une liste de d'indices
}
\references{
FROGS – indices de continuité des soins 

Dreiher (2012) The association between continuity of care in the community and health outcomes a population-based study
}
\author{
  Hugo Marthinet
}\examples{
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

ContinuityOfCare(w = 8,
    n = 4, # A, B, C and D
    vi = vi,
    vc = 2, # 2 times A
    vu = 4, # 4 times B
    vs = 4) # Assuming every sites are different

ContinuityOfCare(8, 4, vc = 2)
}
