\title{
  Mortality and Expenditure Related Morbidity Indexes
}
\name{MRMI}
\alias{MRMI}
\description{
  Compile les score MRMI (Mortality-Related Morbidity Index) et ERMI (Expenditure-Related Morbidity Index).
}\usage{
  MRMI(x)
}\arguments{
  \item{x}{
    La matrice renseignant l'age et les comorbidites des patients. Les 22 colonnes de la matrice doivent etre dans l'ordre.
}
  \item{genre}{Sexe}
  \item{age}{Age en années}
  \item{isch_heart}{Ischemic heart disease}
  \item{cerebro}{Cerebrovascular disease}
  \item{heart_fail}{Heart failure or arrhythmias or valve disease}
  \item{periph_vasc}{Peripheral vascular disease}
  \item{diabetes}{Diabetes}
  \item{cancer}{Cancer}
  \item{cancer_hist}{History of cancer}
  \item{schiz}{Schizophrenia and delusional diseases}
  \item{depression}{Depression and mood disorders}
  \item{substance_abuse}{Substance abuse disorders}
  \item{dementia}{Dementia (including Alzheimer’s disease)}
  \item{parkinson}{Parkinson disease}
  \item{sclerosis}{Multiple sclerosis or paraplegia or tetraplegia}
  \item{epilepsy}{Epilepsy}
  \item{chronic_resp}{Chronic respiratory diseases (including asthma and COPD)}
  \item{infl_bowel}{Inflammatory bowel diseases}
  \item{arthritis}{Rheumatoid arthritis or systemic and connective tissue diseases}
  \item{AIDS}{HIV infection or AIDS}
  \item{end_renal}{End-stage renal disease}
  \item{liver_pancreas}{Liver and pancreas diseases (including failures)}
}
\value{
  Une liste de deux vecteurs, le premier avec les valeurs du score MRMI le deuxieme le score ERMI.
}
\references{
Constantinou (2018) Two morbidity indices developed in a nationwide population permitted performant outcome-specific severity adjustment
}
\author{
  Hugo Marthinet
}
\examples{

}
