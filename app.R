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
    actionButton("login", "Iniciar")
    # dateRangeInput("fechas", "Rango de Fechas Activas", end = Sys.Date()),
    # selectizeInput("aplicacion", "Seleccione la Aplicación", choices = NULL)
  ),
  dashboardBody(
    uiOutput("filtros"),
    verbatimTextOutput("txt1"),
    verbatimTextOutput("txt2")
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Función para interactuar con API
  SolAPI <- function(path){
    url_base <- "https://api.ucld.us/"
    url_mod <- modify_url(url_base, path = paste0("env/prd/",path))
    
    return(url_mod)
  }
  
  observeEvent(input$login,{
   clave <- POST(SolAPI("authentication"), 
                 body = list(paste0(input$user,"@terpel.com"), input$pass),
                 encode = "json",
                 add_headers(Accept = "*/*",
                            ContentType = "application/json")
                 ) %>%
     stop_for_status() %>%
           content(as = "text") 
  })
  
  output$filtros <- renderUI({
    cuentas <- GET(SolAPI("account/getaccounts"),
                   add_headers(Accept = "*/*",
                               ContentType = "application/json", 
                               Authorization = clave)) %>% 
      stop_for_status()
    
    selectizeInput("cuentas", "Seleccionar las Cuentas", choices = cuentas)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

