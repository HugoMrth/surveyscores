\title{
  WCC - Way of Coping Checklist
}\name{WCC}
\alias{WCC}
\description{
  Compile le score WCC (Way of Coping Checklist) a travers les trois sous scores de problèmes de coping, d'emotion et de recherche de soutien. Retourne les valeurs numériques et les classes associées selon les valeurs observees par la norme francaise.
}\usage{
  WCC(x,
      sexe,
      label.femme,
      inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 27 questions nécessaires à la construction du score HAD.
    La matrice peut etre en numerique ou facteur, mais les questions et leur facteurs doivent etre dans le bon ordre (cf. literature).
    Le calcul du score assume que la modalite correspondant a "Non" correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 ("Non") a 4 ("Oui").
}  \item{sexe}{
    Vecteur renseignant le sexe des individus.
}  \item{label.femme}{
    Libellé associé à la modalité "Femme" dans \code{sexe}.
}   \item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du "Oui (1) au "Non" (4). Par defaut, \code{inv.scale = FALSE}.
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
X <- matrix(data = sample.int(4, size = 50*27, replace = TRUE),
            ncol = 27)

sexe <- sample.int(2, size = 50, replace = TRUE)

WCC(X, sexe = sexe, label.femme = 2)
lapply(WCC(X, sexe = sexe, label.femme = 2)$Class, table)
}
