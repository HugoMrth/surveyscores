\title{
  MBI - Maslach Burnout Inventory
}\name{MBI}
\alias{MBI}
\description{
  Compile le score MBI (Maslach Burnout Inventory) a travers les trois sous scores d'Epuisement Professionnel, de Depersonnalisation et d'Accomplissement Personnel. Retourne les valeurs numériques et les classes associées selon les seuils de la literature.
}\usage{
  MBI(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 22 questions nécessaires à la construction du score HAD.
    La matrice peut etre en numerique ou facteur, mais les questions et leur facteurs doivent etre dans le bon ordre (cf. literature).
    Le calcul du score assume que la modalite correspondant a la frequence la plus basse correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 ("Jamais") a 7 ("Tous les jours").
}  \item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du "Tous les jours (1) au "Jamais" (7). Par defaut, \code{inv.scale = FALSE}.
}
}\value{
  Une liste de deux data.frames, le premier avec les scores numeriques, le deuxieme avec les classes associees.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- matrix(data = sample.int(7, size = 50*22, replace = TRUE),
            ncol = 22)

MBI(X)
lapply(MBI(X)$Class, table)
}
