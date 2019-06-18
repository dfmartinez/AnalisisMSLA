#
# Paquetes Necesarios para correr la aplicación
# 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(httr)
library(jsonlite)
library(reactlog)
library(ggvis)

options(shiny.reactlog = TRUE)

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
    dateRangeInput("fechas", "Rango de Fechas Muestreo", start = Sys.Date() - 30, end = Sys.Date()),
    selectizeInput("cliente", "Seleccionar Cliente", choices = NULL),
    selectizeInput("aplicacion", "Seleccione la Aplicación", choices = NULL) 
    
  ),
  dashboardBody(
    uiOutput("salida"),
    dataTableOutput("dat")
    # verbatimTextOutput("text")
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
        datos <- readxl::read_excel(archivos$datapath,1)
      })
    }
    return(datos)
  })
  
  filtro <- reactive({ datos() %>% 
    select(-starts_with("RESULT"),
           -(1:78))
  })
  
## Actulización de las listas desplegables con valores del archivo importado  
  observe({
    updateSelectizeInput(session, "aplicacion", choices = datos()$`Asset Class`, selected = NULL)
    updateSelectizeInput(session, "cliente", choices = datos()$`Account Name`, selected = NULL)
  })
  
  output$salida <- renderUI({
    fluidRow(
      box( title = "Gráfico Dispersión",
        # selectizeInput("var1", "Variable 1", choices = colnames(datos())),
        varSelectizeInput("var1", "Escojer Variable 1", filtro(), multiple = FALSE),
        varSelectizeInput("var2", "Escojer Variable 2", filtro(), multiple = FALSE),
        plotOutput("graf1")
      ),
      box( title = "Gráfico Histograma",
        varSelectizeInput("var3", "Escojer variable", filtro(), multiple = FALSE), 
        plotOutput("graf2")
      )
    )
  })
  
  graficas <- reactive({
    datos() %>% 
      filter(`Date Sampled` >= input$fechas[1] & `Date Sampled` <= input$fechas[2] & 
               `Account Name` == input$cliente & `Asset Class` == input$aplicacion)
  })
  
  output$dat <- renderDataTable(
    graficas(),{
    options = list(scrollX = TRUE)
  })
  
  output$graf1 <- renderPlot({
    graficas() %>% 
      ggplot() +
      geom_point(aes(!!input$var1, !!input$var2))
  })
  
  output$graf2 <- renderPlot({
    graficas() %>% 
    ggplot() +
      geom_histogram(aes(as.numeric(!!input$var3)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

