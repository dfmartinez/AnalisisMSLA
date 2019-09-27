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
  dashboardHeader(title = ("ANÁLISIS DE RESULTADOS MSLA"), titleWidth = 250),
  # Controles Comunes a toda la aplicación
  dashboardSidebar(
    helpText("Ingrese Usuario y contraseña de MobilServ para iniciar"),
    textInput("user", "Usuario"),
    passwordInput("pass", "Contraseña"),
    actionButton("login", "Iniciar"),
    br(),
    uiOutput("filtros")
  ),
  # Diseño de la página principal
  dashboardBody(
    verbatimTextOutput("txt1"),
    verbatimTextOutput("txt2")
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Función para interactuar con API
  SLOAPI <- function(path, mode = c("get", "post"), token = NULL, param = NULL, body = NULL){
    url_base <- "https://api.ucld.us/"
    url_mod <- modify_url(url_base, path = paste0("env/prd/",path), params = param)
    # browser()
    if(mode == "get"){
      resp <- GET(url_mod,
                  add_headers(Accept = "*/*",
                              ContentType = "application/json",
                              Authorization = token)) 
    } else{
      resp <- POST(url_mod,
                   add_headers(Accept = "*/*",
                               ContentType = "application/json",
                               Authorization = token),
                   body = body,
                   encode = "json")
    }
    
    if(status_code(resp) != 200){
      stop(
        glue("Error en la conexión API\n", "Código Error {status_code(resp)}\n","Error: {message_for_status(resp)}"),
        call. = FALSE
      )
    }
    
    resp %>% 
      content(as = "text")
  }
  
  cuentas <- reactiveVal(value = NULL, "cuenta")
  
  observeEvent(input$login,{
   token <- SLOAPI("authentication", "post", body = list(UserName = paste0(input$user,"@terpel.com"), Password = input$pass))
     
     # POST(SolAPI("authentication"), 
     #             body = list(UserName = paste0(input$user,"@terpel.com"), Password = input$pass),
     #             encode = "json",
     #             add_headers(Accept = "*/*",
     #                        ContentType = "application/json")
     #             ) %>%
     # stop_for_status() %>%
     #       content(as = "text") 
   
   cuentasmsla <- SLOAPI("account/getaccounts", "get", token = token) %>% 
     fromJSON() %>%
     filter(!row.names(.) %in% c("1","2")) %>%
     split(.$name) %>%
     map(~.$id)
     
     # GET(SolAPI("account/getaccounts"),
     #              add_headers(Accept = "*/*",
     #                          ContentType = "application/json",
     #                          Authorization = token)) %>%
     # stop_for_status() %>% 
     # content(as = "text") %>% 
     
   
   cuentas(cuentasmsla)
  })
  
  # cuentas <- reactive({
  #   GET(SolAPI("account/getaccounts"),
  #       add_headers(Accept = "*/*",
  #                   ContentType = "application/json",
  #                   Authorization = token)) %>% 
  #     stop_for_status() %>% 
  #     content(as = "text") %>% 
  #     fromJSON()
  # })
  
  output$filtros <- renderUI({
    if(input$login == 0)
      return()
    
    selectizeInput("cuentas", "Seleccionar Cuenta", choices = cuentas(), options = list())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

