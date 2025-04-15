\title{
  Charlson Comorbidity Index
}
\name{Charlson}
\alias{Charlson}
\description{
  Compile le score de Charlson de comorbidité (Charlson Comorbidity Index). Retourne la valeur du score ainsi que la probabilite de survie à 10 telle que déterminer par la methode de Charlson. Les colonnes du jeu de donnees en entree doivent respecter l'ordre preetabli par la grille de notation.
}\usage{
  Charlson(x)
}\arguments{
  \item{x}{
    La matrice renseignant l'age et les comorbidites des patients. L'age doit etre la première colonne, des 19 comorbidites, codees en binaire, par ordre de gravite.
}
}
\value{
  Une liste de deux vecteurs, le premier avec les valeurs du score, le deuxieme les probabilites de survie a 10 ans associes.
}
\references{
https://www.maeker.fr/fr/geriatrie/evaluations/charlson

Charlson ME, Pompei P, Ales KL, MacKenzie CR. A new method of classifying prognostic comorbidity in longitudinal studies: development and validation. J Chronic Dis. 1987

Fried L, Bernardini J, Piraino B. Charlson comorbidity index as a predictor of outcomes in incident peritoneal dialysis patients. Am J Kidney Dis. 2001
}
\author{
  Hugo Marthinet
}
\examples{
x <- cbind(35:84, matrix(sample.int(2, 50*19, replace = TRUE, prob = c(0.9, 0.1))-1, ncol = 19))
Charlson(x)
}
