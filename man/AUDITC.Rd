\title{
  Alcohol Use Disorders Identification Test
}\name{AUDITC}
\alias{AUDITC}
\description{
  Compile le score AUDIT-c (Alcohol Use Disorders Identification Test). Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature. Les questions utilisees pour la construction du score sont : la frequence de consommation (en 5 niveaux), le nombre de verres consommes dans un jour typique (en 6 niveaux) et la frequence de consommation de 6 verres d'alcool ou plus (en 5 niveaux).
}\usage{
  AUDITC(x, sexe, label.femme = 0)
}\arguments{
  \item{x}{
    La matrice des réponses aux 3 questions nécessaires à la construction du score.
    La matrice peut etre en numerique ou facteur.
    Le calcul du score assume que la fréquence la plus basse correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (le moins frequent) a 5 ou 6 (le plus frequent).
}  \item{sexe}{
    Le vecteur correspondant au sexe des individus (une penalite est attribuee aux femmes).
}  \item{label.femme}{
    La modalite correspondant aux femmes dans le vecteur renseignant les sexes. Par défaut \code{label.femme = 0}.
}
}\value{
  Une liste de deux vecteurs, le premier avec les valeurs du score, le deuxieme avec les classes associees.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- cbind(sample.int(5, size = 50, replace = TRUE),
           sample.int(6, size = 50, replace = TRUE),
           sample.int(5, size = 50, replace = TRUE))

sexe <- sample.int(2, size = 50, replace = TRUE)

AUDITC(X, sexe, label.femme = 1)
}
