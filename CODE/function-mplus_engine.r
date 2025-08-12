# Mplus Engine for knitr
#
# Rich Jones
# Nov 25 2024
#
knitr::knit_engines$set(mplus = function(options) {    code <- paste(options$code, collapse = "\n")    fileConn<-file("formplus.inp")    writeLines(code, fileConn)    close(fileConn)    out  <- system2("/Applications/Mplus/mplus", "formplus.inp")    fileConnOutput <- file("formplus.out")    mplusOutput <- readLines(fileConnOutput)    knitr::engine_output(options, code, mplusOutput)})