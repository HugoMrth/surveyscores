\title{
  SF-12 Health Survey
}\name{SF12}
\alias{SF12}
\description{
  Compile les sous scores MCS (Mental Component Summary) et PCS (Physical Component Summary) du questionnaire SF-12 (Physical and Mental Health Summary Scales). Retourne les valeurs du score. Aucun seuil absolu de reference n'existe pour ce score et il est plutot pertinent lors de la comparaison de plusieurs populations. Les seuils utilises pour etablir les classes sont bases sur un echantillon representatif de la population des Etats-Unis (Ware et al).
}\usage{
  WEMWBS(x)
}\arguments{
  \item{x}{
    La matrice des réponses aux 12 questions nécessaires à la construction du score. La matrice doit être en facteurs et les questions et modalites dans le memes ordre que dans le questionnaire de reference (http://orthotoolkit.com/sf-12/).
}
}
\value{
  Une liste de deux data.frames, le premier avec les scores numeriques, le deuxieme avec les classes associees.
}
\references{
 http://orthotoolkit.com/sf-12/
}
\author{
  Hugo Marthinet
}\examples{

}
