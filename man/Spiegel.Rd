\title{
  Score Spiegel de Qualité du sommeil
}\name{Spiegel}
\alias{Spiegel}
\description{
  Compile le score Spiegel de Qualité du sommeil. Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature. Les questions utilisees pour la construction du score concernent : le delai d'endormissement, la qualite du sommeil, la duree du sommeil, la frequence des reveil nocturnes, la frequence des reves et l'etat de forme au reveil.
}\usage{
  Spiegel(x, scoring = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 6 questions nécessaires à la construction du score. La matrice peut etre en numerique ou facteur, sans ordre precis dans les questions.
    Le calcul du score assume que la modalite la plus positive correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (plus positif) a 5 (le plus negatif), avec 6 qui est associe a la modalite "Ne sait pas".
    Si le scoring des questions est deja effectue, soit la modalite "Ne sait pas" a 0 et une echelle de 1 (plus negatif) a 5 (le plus positif), l'inversion de l'echelle peut se faire en renseignant \code{scoring = TRUE}.
}
  \item{scoring}{
    Par defaut, \code{FALSE} et le scoring des questions est compile. Si le scoring est deja effectue, \code{scoring = FALSE} inversera l'echelle.
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
X <- matrix(data = sample.int(5, size = 50*6, replace = TRUE),
            ncol = 6)

Spiegel(X)
table(Spiegel(X)$Classe)
}
