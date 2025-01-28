my_packages <- c("shiny", 
                 "shinydashboard", 
                 "RSQLite", 
                 "sodium", 
                 "DT", 
                 "blastula", 
                 "glue", 
                 "bslib", 
                 "shinyjs", 
                 "shinyBS", 
                 "DBI", 
                 "gmailr", 
                 "dplyr")
install_if_missing <- function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))