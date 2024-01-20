\title{
  Echelle de Glasgow
}\name{Glasgow}
\alias{Glasgow}
\description{
  Compile le score de Glasgow d'etat de conscience. Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature. Les questions utilisees pour la construction du score sont : l'ouverture des yeux, la reponse verbale et la reponse motrice.
}\usage{
  Glasgow(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 3 questions nécessaires à la construction du score.
    La matrice peut etre en numerique ou facteur, mais les questions doivent etre dans l'ordre precise ci dessus.
    Le calcul du score assume que la modalite de conscience la plus basse correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (le moins de conscience) a 4, 5 ou 6 (le plus de conscience) suivant la question.
}\item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du plus de conscience (1) au moins de conscience (4, 5 ou 6). Par defaut, \code{inv.scale = FALSE}.
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
X <- cbind(sample.int(4, size = 50, replace = TRUE),
           sample.int(5, size = 50, replace = TRUE),
           sample.int(6, size = 50, replace = TRUE))

Glasgow(X)
}
