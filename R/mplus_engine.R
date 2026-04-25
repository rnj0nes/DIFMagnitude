#' Mplus Engine for knitr
#'
#' Sets up a custom knitr engine for running Mplus code from Quarto/Rmarkdown documents.
#'
#' @details
#' When used in an mplus engine code block, this engine:
#' 1. Writes the code to formplus.inp
#' 2. Executes Mplus on that file
#' 3. Returns the output in the document
#'
#' @return Used for side effects; registers knitr engine
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Call mplus_engine() before knitting if you want to enable the engine.
#' }
#'
mplus_engine <- function() {
   knitr::knit_engines$set(mplus = function(options) {
      code <- paste(options$code, collapse = "\n")
      fileConn <- file("formplus.inp")
      writeLines(code, fileConn)
      close(fileConn)
      out <- system2("/Applications/Mplus/mplus", "formplus.inp")
      fileConnOutput <- file("formplus.out")
      mplusOutput <- readLines(fileConnOutput)
      knitr::engine_output(options, code, mplusOutput)
   })
}
