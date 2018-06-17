#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(leaflet)
#require(DT)

source('Scripts/parametros-ui.R')
shinyUI(

dashboardPage(
  dashboardHeader(title = titulo),
  dashboardSidebar(
    selectInput("year",label = HTML('<p style="color:black">Ejercicio Fiscal <p>') ,choices = c(1998:format(Sys.Date(), "%Y")), selected = format(Sys.Date(), "%Y")  ),
    sidebarMenu(
    id = "tabs",
    menuItem("Información principal", tabName = "dashboard", icon = icon("dashboard") ),
    menuItem("Principal", tabName = "widgets", icon = icon("th"),
             menuSubItem("Tabla y gráfica", tabName = "anio", icon = icon("table") ),
             menuSubItem("Comparador", tabName= "comparador-exp", icon = icon("table") )  )
  )),
  dashboardBody(

    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                if( tipo == "gasto")
                  infoBoxOutput("progressBox") ,
                infoBoxOutput("devengadoBox"),
                uiOutput("detalle")
              ),
              fluidRow(
                box(title = HTML( paste0('<p style=color:',color_texto_caja,'>', titulo_caja, '</p>') ),
                    status = "primary", solidHeader = T , collapsible = TRUE,width = 12,
                    plotlyOutput("grafica-inicio", height = 400)
                )
                
              )
      ),
      
      tabItem(tabName = "anio",
              fluidRow(
                column(12, align= "center",offset = 0,
                       tags$h1(titulo_main)
                )
              ),
              
              fluidRow(
                column(12, align= "center",offset = 0,
                       tags$h3(titulo_sector)
                )
              ),
              
              
              if( tipo == "gasto")
              fluidRow(
                column(3, align = "left", radioButtons("metrica_grafica", "¿Qué desea graficar?", choiceValues = c("Ejecutado", "Devengado"), choiceNames = c("Porcentaje de ejecución", "Devengado"), inline = T ))
              ),
              
              
              fluidRow(
                column(12, align ="center",
                       box(title = HTML( paste0('<p style=color:',color_texto_caja,'>', titulo_grafica, '</p>') ),
                           status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                           plotlyOutput("grafica", height = 400, width = "100%")
                       )
                )
                
              )
              , 
              
              fluidRow(
                column(3),
                column(3),
                column(3, align = "right",
                       downloadButton("descarga","Descargar") ),
                column(3, align = "right",
                       uiOutput("Atras"))
              ), 
              
              fluidRow(
                column(12, align = "left", 
                       DT::dataTableOutput("tabla")
                ) )
              ),
      tabItem(tabName = "comparador-exp",
              fluidRow(
                column(2,uiOutput("comparadorAño")),
                column(3,uiOutput("dimension")),
                column(3,uiOutput("valor-dimension")),
                column(2, uiOutput("boton-comparar") ),
                column(2, uiOutput("boton-comparar-tabla") )
              ),
              fluidRow(
                column(12, align ="center",
                       box(title = HTML( paste0('<p style=color:',color_texto_caja,'>', "Gráfica de comparación", '</p>') ),
                           status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                           plotlyOutput("grafCon", height = 400, width = "100%")
                       )
                )
              ),
              fluidRow(
                column(12, align ="center",
                       box(title = HTML( paste0('<p style=color:',color_texto_caja,'>', "Tabla de comparación de devengados", '</p>') ),
                           status = "primary", solidHeader = TRUE, collapsible = TRUE,width = 12,
                           DT::dataTableOutput("tabla-comparacion")
                       )
                )
              )
              )
    ),
    

    
    conditionalPanel(
      condition =  "output.condition == 100",
      dropdownButton(label = "Hola", icon = icon("gear"))
    ),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css" ) ),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "box.css" ) ),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "cajas.css" ) ),
    
    tags$head(tags$style(".shiny-notification {position: fixed; top: 60% ;left: 50%"))
    
#     tags$style(HTML("
# 
# 
# .box.box-solid.box-primary>.box-header {
#   color:#000000;
#   background: #605ca8
#                     }
# 
# .box.box-solid.box-primary{
# border-bottom-color: #605ca8;
# border-left-color: #605ca8;
# border-right-color: #605ca8;
# border-top-color: #605ca8;
# }
# .sidebar-menu .treeview-menu{     padding: 0 0 0 50px;
# color:#000000}
#                                     "))
     
    
    
    
  )
  
)

)