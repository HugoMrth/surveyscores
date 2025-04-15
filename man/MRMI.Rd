\title{
  Mortality and Expenditure Related Morbidity Indexes
}
\name{MRMI}
\alias{MRMI}
\description{
  Compile les score MRMI (Mortality-Related Morbidity Index) et ERMI (Expenditure-Related Morbidity Index).
}\usage{
  MRMI(...)
}\arguments{
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
 MRMI(gender = "M",
                       age = 75,
                       isch.heart = 0,
                       cerebro.vasc = 0,
                       heart.fail = 0,
                       periph.vasc = 0,
                       diabetes = 1,
                       cancer = 0,
                       cancer.hist = 0,
                       schizo = 0,
                       depression = 0,
                       subst.abuse = 0,
                       dementia = 0,
                       parkinson = 0,
                       mult.sclero = 0,
                       epilespy = 0,
                       chornic.resp = 1,
                       infl.bowel = 0,
                       rheum.arthr = 0,
                       HIV = 0,
                       es.renal = 0,
                       liver.pancr = 0)


 MRMI(gender = "M",
           age = sample(50:90, 100, replace = TRUE),
           isch.heart = sample(c(0,1), 100, replace = TRUE),
           cerebro.vasc = sample(c(0,1), 100, replace = TRUE),
           heart.fail = sample(c(0,1), 100, replace = TRUE),
           periph.vasc = sample(c(0,1), 100, replace = TRUE),
           diabetes = sample(c(0,1), 100, replace = TRUE),
           cancer = sample(c(0,1), 100, replace = TRUE),
           cancer.hist = sample(c(0,1), 100, replace = TRUE),
           schizo = sample(c(0,1), 100, replace = TRUE),
           depression = sample(c(0,1), 100, replace = TRUE),
           subst.abuse = sample(c(0,1), 100, replace = TRUE),
           dementia = sample(c(0,1), 100, replace = TRUE),
           parkinson = sample(c(0,1), 100, replace = TRUE),
           mult.sclero = sample(c(0,1), 100, replace = TRUE),
           epilespy = sample(c(0,1), 100, replace = TRUE),
           chornic.resp = sample(c(0,1), 100, replace = TRUE),
           infl.bowel = sample(c(0,1), 100, replace = TRUE),
           rheum.arthr = sample(c(0,1), 100, replace = TRUE),
           HIV = sample(c(0,1), 100, replace = TRUE),
          es.renal = sample(c(0,1), 100, replace = TRUE),
           liver.pancr = sample(c(0,1), 100, replace = TRUE))

}
