\title{
  Proxy de l'EuroSCORE
}\name{EuroSCORE}
\alias{EuroSCORE}
\description{
  Calcul le proxy de l'EuroSCORE.
}\usage{
  EuroSCORE(x)
}\arguments{
  \item{x}{
    La matrice des 21 variables nécessaires à la construction du score. 
    La matrice doit être en facteurs et les questions et modalites dans le memes ordre que dans l'exemple ci-dessous.
}
}
\value{
  Un vecteur de scores.
}
\references{
 
}
\author{
  Hugo Marthinet
}\examples{
somme = -5.324537 + 
      0.6527205*top_chir_tho +
      0.0062118*top_poids_op1+
      0.5521478*top_poids_op2+
      0.9724533*top_poids_op3+
      0.3174673*top_inter_urg + 
      0.1528943*top_im + 
      0.3150652*top_FEVG + 
      1.086517*top_eta_preop + 
      0.6194522*top_endo + 
      0.6421508*top_ir_dialysis + 
      01.118599*top_chir_car + 
      0.2407181*top_mauv_mob + 
      0.1886564*top_MRC+
      0.5360268*top_AEC + 
      0.2196434*sexe_f+
      0.0285181*age_poids+ 
      0.3542749*top_diab + 
      0.2226147*top_angor + 
      0.1070545*top_cla_nyha_2 +
      0.2958358*top_cla_nyha_3 +
      0.5597929*top_cla_nyha_4
euroscore = 100*exp(somme)/(1+exp(somme))
}
