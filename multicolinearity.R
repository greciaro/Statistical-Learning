#MULTICOLINEARITY 
install.packages("rattle")
install.packages("RGtk2")
library(rattle)
rattle()
av <- available.packages(filters=list())
av[av[, "Package"] =="RGtk2", ]
options(repos = "http://cran.stat.auckland.ac.nz")
install.packages(c("RGtk2"))
install.packages("RGtk2", depen=T, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/RGtk2/RGtk2_2.20.36.3.tar.gz ",repos = NULL)
