# From https://github.com/rstudio/renv/issues/340

renvLock <- jsonlite::read_json("renv.lock")
sink("r_packges.bib")
knitr::write_bib(names(renvLock$Packages))
sink()
