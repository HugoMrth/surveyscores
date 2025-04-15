\title{
  Mesures d'adhésion
}\name{Adherence}
\alias{Adherence}
\description{
  Compile plusieurs mesures d'adhesion medicamenteuse.
}\usage{
  Adherence(
    n.days.supplied = NA,
    n.days.gaps = NA,
    n.days.period = NA,
    n.days.last.supply = NA,
    surplus = NA,
    diff.first.last.supply = NA,
    quantity.supplied = NA,
    quantity.per.day = NA)
}\arguments{
  \item{n.days.supplied}{
     Days’ supply of medication obtained
}\item{n.days.gaps}{
    Days of treatment gaps
}\item{n.days.period}{
     Days to next fill or end observation period
}\item{n.days.last.supply}{
    Last days’ supply
}\item{surplus}{
    Days of treatment surplus
}\item{diff.first.last.supply}{
    Last claim date – First claim date
}\item{quantity.supplied}{
    Sum of quantity dispensed over interval
}\item{quantity.per.day}{
    Quantity to be taken  per day
}
}\value{
  Une liste de d'indices
}
\references{
Hess (2006) Measurement of Adherence in Pharmacy Administrative Databases A Proposal for Standard Definitions and Preffered Measures
}
\author{
  Hugo Marthinet
}\examples{

}
