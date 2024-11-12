<p align="center">
<img src="inst/logo.png" height="200"/> 
</p>

## Calculation of score from litterature surveys (FRENCH VERSION)
  
  
***

 ### 1) Install

```
#Install from CRAN 
#Not currently on CRAN

  
#Install the development version from GitHub  
install.packages("devtools")

#Install package from github
devtools::install_github("HugoMrth/surveyscores", build_vignettes = TRUE)
#Or download the zip file
devtools::install(build_vignettes = TRUE)

#R may recquire a session restart in order to properly run the App : Ctrl + Shift + F10
```

### 2) Disclaimer

The reference surveys this package implement have different versions in every language. This one is based on the French versions and WILL NOT be adequate to treat the results of a foreign version of the same survey.
Therefore, all of the documentation for this package is in french and will remain that way. This is intentionnal.

  
### 3) Purpose

Assuming the questions are phrased the same, and ordered in the same way as the reference survey, all the functions available will compute the numerical score and associated group using the thresholds as precribed
in the reference paper for the given survey.




