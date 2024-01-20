\title{
  HSI - Heavy Smoker Index
}
\name{HSI}
\alias{HSI}
\description{
  Compile le score HSI (Heavy Smoker Index). Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature. Les questions utilisees pour la construction du score sont : le nombre de cigarettes fumees, et dans quelle delai apres le reveil.
}\usage{
  HSI(x, ind.NA = NULL)
}\arguments{
  \item{x}{
    La matrice des réponses aux 2 questions nécessaires à la construction du score.
    La matrice peut etre en numerique ou facteur.
    Le calcul du score assume que la modalite correspondant a l'addiciton la plus basse correspond au facteur le plus bas (1 pour un non fumeur, 2 pour la modalites de fumeur la plus basse). L'echelle va donc de 1 (non fumeurs) a 5.
}  \item{ind.NA}{
    Un vecteur de booleen renseignant si besoin les individus non concernes par le score : soit les individus ne fumant pas actuellement. Par defaut \code{ind.NA = NULL} et tous les individus sont inclus.
}
}
\value{
  Une liste de deux vecteurs, le premier avec les valeurs du score, le deuxieme avec les classes associees.
}
\references{

}
\author{
  Hugo Marthinet
}
\examples{
X <- cbind(sample.int(5, size = 50, replace = TRUE),
           sample.int(5, size = 50, replace = TRUE))

non_fumeurs <- sample.int(100, size = 50, replace = TRUE) > 80

HSI(X)
HSI(X, ind.NA = non_fumeurs)
}
