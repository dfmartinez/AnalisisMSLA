#
# Paquetes Necesarios para correr la aplicación
# 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(httr)
library(jsonlite)

# Pruebas inciales conexion API MobilServ
# url_base <- "https://api.ucld.us/"
# url_final <- modify_url(url_base, path = "env/prd/authentication")
# clave <- POST(url_final, body = list(UserName = "diego.martinez@terpel.com", Password = "Tomate80"),
#               encode = "json",
#               add_headers(ContentType = "application/json",
#                           Accept = "*/*"
#                           ), verbose()) %>% 
#   content("text")

# Interfaz de Usuario
ui <- dashboardPage(
  # Título de la Página
  dashboardHeader(title = ("ANÁLISIS DE RESULTADOS MSLA")),
  # Controles Comunes a toda la aplicación
  dashboardSidebar(
    fileInput("archivomsla", "Buscar Archivo descargado MSLA", multiple = FALSE,
              accept = c(".xls", ".xlsx", ".csv"),
              placeholder = "Ningún Archivo Seleccionado"),
    dateRangeInput("fechas", "Rango de Fechas Activas", end = Sys.Date()),
    selectizeInput("aplicacion", "Seleccione la Aplicación", choices = NULL)
  ),
  dashboardBody(
    uiOutput("salida")
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  
  datos <- reactive({
    req(input$archivomsla)
    archivos <- input$archivomsla
    
    if(endsWith(archivos$name, ".csv")){
      withProgress(message = "Importando Archivo",{
        datos <- read.csv(archivos$datapath)
      })
    } else{
      withProgress(message = "Importando Datos", {
        datos <- readxl::read_excel(archivos$datapath)
      })
    }
  })
  output$salida <- renderUI({
    fluidRow(
      box(
        varSelectizeInput("var1", "Escojer Variable 1", datos()),
        
      )
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

