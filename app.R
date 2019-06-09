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
    fluidRow(
       box(which = "plot",
        selectizeInput("prop1", "Seleccionar 1", choices = NULL),
        selectizeInput("prop2", "Seleccionar 2", choices = NULL),
        plotOutput("puntos")
        ),
       box(title = "Histograma",
           plotOutput("hist"))
          ),
    fluidRow(
      dataTableOutput("tabla")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  
  datos <- reactive({
    archivos <- input$archivomsla
    if(is.null(archivos)){ 
      return(NULL)
    } else if(archivos$type == "text/csv"){
      withProgress(message = "Importando Archivo",{
        datos <- read.csv(archivos$datapath)
      })
    } else{
      withProgress(message = "Importando Datos", {
        datos <- readxl::read_excel(archivos$datapath)
      })
    }
  })
      
  output$tabla <- renderDataTable({
    datos()
  })
  
  output$puntos <- renderPlot({
    
  })
  observe({
    updateSelectizeInput(session, "aplicacion", choices = as_factor(datos()$`Asset Class`))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

