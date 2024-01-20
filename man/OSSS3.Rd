\title{
  Score OSSS-3 (Oslo Social Support Scale) de soutien social
}\name{OSSS3}
\alias{OSSS3}
\description{
  Compile le score OSSS (Oslo Social Support Scale) de soutien social en 3 questions. Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature. Les questions utilisees pour la construction du score concernent : le nombre de personnes proches en 4 classes, et l'interêt de l'entourage et la facilite d'obtenir de l'aide par des voisins en 5 classes.
}\usage{
  OSSS3(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 3 questions nécessaires à la construction des scores. La matrice peut etre en numerique ou facteur, et doit respecter l'ordre des questions du questionnaire de reference.
    Le calcul du score assume que la modalite traduisant le soutien social le plus bas correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (le moins de soutien) a 4 ou 5 (le plus de soutien).
}
\item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du plus de soutien (1) au moins de soutien (4 ou 5). Par defaut, \code{inv.scale = FALSE}.
}
}
\value{
  Une liste de deux vecteurs, le premier avec les valeurs du score, le deuxieme avec les classes associees.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- cbind(sample.int(4, size = 50, replace = TRUE),
           sample.int(5, size = 50, replace = TRUE),
           sample.int(5, size = 50, replace = TRUE))

OSSS3(X)
table(OSSS3(X)$Classe)
}
