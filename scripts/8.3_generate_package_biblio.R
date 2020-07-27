# From https://github.com/rstudio/renv/issues/340

renvLock <- jsonlite::read_json("renv.lock")
sink("r_packages.bib")
knitr::write_bib(names(renvLock$Packages))
sink()
