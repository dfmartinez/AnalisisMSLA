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
    # fileInput("archivomsla", "Buscar Archivo descargado MSLA", multiple = FALSE,
    #           accept = c(".xls", ".xlsx", ".csv"),
    #           placeholder = "Ningún Archivo Seleccionado"),
    helpText("Ingrese Usuario y contraseña de MobilServ para iniciar"),
    textInput("user", "Usuario"),
    passwordInput("pass", "Contraseña"),
    actionButton("login", "Iniciar"),
    uiOutput("filtros")
    # dateRangeInput("fechas", "Rango de Fechas Activas", end = Sys.Date()),
    # selectizeInput("aplicacion", "Seleccione la Aplicación", choices = NULL)
  ),
  dashboardBody(
    # uiOutput("salida"),
    # dataTableOutput("dat")
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
  
  observeEvent(input$login, {
    # url_clave <- modify_url(url_base, path = "env/prd/authentication")
    withProgress(message = "Conectando a MSLA",{
    clave <- reactive({POST(SolAPI("authentication"), body = list(UserName = input$user, Password = input$pass),
                  encode = "json",
                  add_headers(ContentType = "application/json",
                              Accept = "*/*"
                              )) %>%
      content("text")
      })
    })
  })
  
  cuentas <- reactive({
    req(input$login)
    GET(SolAPI("account/getaccounts"),
        add_headers(Accept = "*/*",
                    ContentType = "application/json",
                    Authorization = clave())) %>%
      stop_for_status() %>%
      content("text")
  })  

  # output$filtros <- renderUI(
  #   # req(input$login)
  #   selectizeInput("cuentas", choices = cuentas)
  # )
  
  # output$txt1 <- renderText({
  #   clave()
  #   })
  # output$txt2 <- renderText({
  #   if(input$login == 0){
  #     return(0)
  #   } else{cuentas}
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

