\title{
  ECI - Elixhauser Comorbidity Index
}
\name{CCI}
\alias{Elixhauser.CI}
\description{
  Compile le score de Charlson de comorbidité. Retourne la valeur du score ainsi que la probabilite de survie à 10 telle que déterminer par la methode de Charlson. Les colonnes du jeu de donnees en entree doivent respecter l'ordre preetabli par la grille de notation.
}\usage{
  Elixhauser.CI(x)
}\arguments{
  \item{x}{
    La matrice renseignant l'age et les comorbidites des patients. L'age doit etre la première colonne, des 19 comorbidites, codees en binaire, par ordre de gravite.
}
}
\value{
  Une liste de deux vecteurs, le premier avec les valeurs du score, le deuxieme les probabilites de survie a 10 ans associes.
}
\references{
Elixhauser, Anne, et al. "Comorbidity measures for use with administrative data." Medical care (1998)

van Walraven, Carl, et al. "A modification of the Elixhauser comorbidity measures into a point system for hospital death using administrative data." Medical care (2009)
}
\author{
  Hugo Marthinet
}
\examples{
x <- cbind(matrix(sample.int(2, 50*30, replace = TRUE, prob = c(0.9, 0.1))-1, ncol = 30))
Elixhauser.CI(x)
}
