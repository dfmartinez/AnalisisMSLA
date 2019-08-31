#
# Paquetes Necesarios para correr la aplicación
# 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(httr)
library(jsonlite)
library(reactlog)


options(shiny.reactlog = TRUE)

# Interfaz de Usuario
ui <- dashboardPage(
  # Título de la Página
  dashboardHeader(title = ("ANÁLISIS DE RESULTADOS MSLA")),
  # Controles Comunes a toda la aplicación
  dashboardSidebar(
    helpText("Ingrese Usuario y contraseña de MobilServ para iniciar"),
    textInput("user", "Usuario"),
    passwordInput("pass", "Contraseña"),
    actionButton("login", "Iniciar"),
    uiOutput("filtros")
    # dateRangeInput("fechas", "Rango de Fechas Activas", end = Sys.Date()),
    # selectizeInput("aplicacion", "Seleccione la Aplicación", choices = NULL)
  ),
  dashboardBody(
    verbatimTextOutput("txt1"),
    verbatimTextOutput("txt2")
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # options(shiny.maxRequestSize=30*1024^2)
  
  ## Pruebas inciales conexion API MobilServ
  
  SolAPI <- function(path){
    url_base <- "https://api.ucld.us/"
    url_mod <- modify_url(url_base, path = paste0("env/prd/",path))
    
    return(url_mod)
  }
  
  observeEvent(input$login,{
   clave <- POST(SolAPI("authentication"), 
                 body = list(input$user, input$pass),
                 encode = "json",
                 add_headers(Accept = "*/*",
                            ContentType = "application/json")
                 ) %>%
           content(as = "text") 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

