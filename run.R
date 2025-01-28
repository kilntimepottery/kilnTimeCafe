library(shiny)
port <- Sys.getenv('PORT')
runApp(appDir = getwd(), host = '0.0.0.0', port = as.numeric(port))