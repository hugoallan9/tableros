#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#





# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # Acá se carga el script con las variables globales de cada tablero
  # Para hacer los cambios en cada tablero debe acceder a este archivo
  # y modificarlo
  source('Scripts/parametros-server.R')

  # Variables para uso en esta sesión ---------------------------------------

  mapaJson <-  NULL
  tabla_temporal <- NULL
  tabla_dinamica <- NULL

  
  
  # Setting up the environmet for global variables --------------------------
  app.env <- new.env()
  
  app.env$nivelActivo <- ""
  
  jerarquia_institucional_ida = lifo()
  push(jerarquia_institucional_ida, "Unidad.Ejecutora" )
  push(jerarquia_institucional_ida, "Entidad" )
  
  jerarquia_institucional_regreso = lifo()
  
  valores_institucional = list()
  
  jerarquia_finalidad_ida = lifo()
  push(jerarquia_finalidad_ida, "División")
  push(jerarquia_finalidad_ida, "Función")
  push(jerarquia_finalidad_ida, "Finalidad")
  
  
  jerarquia_finalidad_regreso = lifo()
  
  jerarquia_geografico_ida = lifo()
  push(jerarquia_geografico_ida, "Municipio")
  push(jerarquia_geografico_ida, "Departamento")
  push(jerarquia_geografico_ida, "Región")
  
  jerarquia_geografico_regreso = lifo()
  
  jerarquia_objeto_gasto_ida = lifo()
  push(jerarquia_objeto_gasto_ida, "Renglón")
  push(jerarquia_objeto_gasto_ida, "Sub.Grupo.Gasto")
  push(jerarquia_objeto_gasto_ida, "Grupo.Gasto")
  
  jerarquia_objeto_gasto_regreso = lifo()
  
  jerarquia_economico_ida = lifo()
  push(jerarquia_economico_ida, "Clasificación.Económica.Gasto")
  push(jerarquia_economico_ida, "Económico.Nivel.Operativo")
  push(jerarquia_economico_ida, "Económico.Nivel.4")
  push(jerarquia_economico_ida, "Económico.Nivel.3")
  push(jerarquia_economico_ida, "Económico.Nivel.2")
  #push(jerarquia_economico_ida, "Económico.Nivel.1")
  
  
  jerarquia_economico_regreso = lifo()
  
  
  dimension_ida = NULL # candidato a ser eliminado
  valor_dimension = NULL #candidato a ser eliminado
  
  jerarquia_dimension_regreso = list()  #cambio de estructura de datos
  jerarquia_valor_dimension_regreso = list() #antes era lifo, se pasa a lista
  
  
  filaSeleccionada <- 0
  datos_economico <- read.csv(paste0('Data','/Jerarquia_Economico.csv'), sep=';')
  
  ejecucionMes <- read.csv(paste0('Data','/EjecucionMensual.csv'))
  datos_intitucional <- read.csv( paste0('Data','/Jerarquia_Entidad.csv'), sep=";")
  datos_finalidad <- read.csv(  paste0( 'Data','/Jerarquia_Finalidad.csv'), sep= ';')
  datos_geografico <- read.csv(paste0('Data','/Jerarquia_Geografico.csv'), sep=';')
  datos_objeto_gasto <- read.csv( paste0('Data','/Jerarquia_ObjetoGasto.csv'), sep=';')
  datos_economico <- read.csv(  paste0('Data', '/Jerarquia_Economico.csv'), sep=';')
  
  

# REACTIVE VALUES ---------------------------------------------------------

  
# Definición de los valores que son reactivos. Se usarán através de toda la aplicación
  devengado <- reactiveValues(d = 0,p = 0)
  
  # Para mapa, no se está seguro del uso, candidato a eliminación
  mapaAnterior <- reactiveValues(mapa = NULL, variable = NULL)
  
  # Variable tempo, sin explicación, candidata a eliminación
  tempo <- reactiveValues(temp=NULL)
  
  # Reactive values que permiten hacer click en el treemap
  treemap_clicked <- reactiveValues(
    center = NULL,
    for_condition=NULL
  )
  
 #Valores reactivos para registrar los eventos
  numeroReactivos <- reactiveValues(x = 0, y = 0)
  
  # Valores reactivos para la comparación de datos
  datos_comparativos <- reactiveValues(data=NULL)
  
  # Valores para el árbol
  network <- reactiveValues()
  
  #valores reactivos para los datos de comparación
  
# CANDIDATA A ELMINACION --------------------------------------------------

  
 
  # Acá comienza la sección es candidata a irse, ya que la parte de los paneles condicionales
  # será finalmente eliminada para dar paso a los tabs del dashboard
   
  output$condition <- renderText({
    condition()
  })
  
  output$tipoVisualizacion <- renderText({
    visualizacion()
  })
  

  
  
  condition <- reactive({
    refresh = detalleGasto()
    retornoVisualizacion = input$visualizacion
    input$visualizacion
    result = 0
    if( !is.null(refresh)){
      if( is.null(retornoVisualizacion) || retornoVisualizacion != "Código.Departamento" ){
        if( refresh%%2 == 0 ){
          result = 0
        }else if( refresh%%2 == 1 && retornoVisualizacion == "treemap"  )
          result =1
        else if( refresh%%2 == 1 && retornoVisualizacion == "tabla")
          result = 3
        #gasto_tabla()
      }
      else if( retornoVisualizacion == "Código.Departamento")
        if( refresh%%2 == 0 ){
          result = 0
        }else if( retornoVisualizacion == "tabla" ){
          result = 3
          #gasto_tabla()
        }
      else
        result = 2
    }
    return(result)
  })
  
  
  visualizacion <- reactive({
    clasificacion = input$filtro
    result = 0
    if( !is.null(clasificacion) && clasificacion == "Código.Departamento"){
      result = 1
    }
    return(result)
  })
  
  
  outputOptions(output, 'condition', suspendWhenHidden=FALSE)
  
  detalleGasto=reactive({
    input$detalleGasto
  }) 
  
  
  # Finaliza condidato a eliminación
  

# FUNCIONES REACTIVE ------------------------------------------------------

# Función reactive que permite calcular el porcentaje de ejecución para el tablero principal
  
  
  porcentajeEjecucion <- reactive({
    ejecucionMes %>%
      select(Ejercicio, Devengado, Vigente ) %>%
      filter(Ejercicio == input$year ) %>%
      mutate(PorcentajeEje =  Devengado /   Vigente  * 100  ) 
  })
  
  # Función reactive que calcula el gasto para treemap
  
  gasto <- reactive({
    temp <- data.frame()
    if(!is.null(input$filtro)){
      temp <- entidad %>%
        select(Devengado, input$filtro)%>%
        group_by_(input$filtro) %>%
        summarise(devengado = sum(Devengado))
    }
    return(temp)
  })
  
  # Función reactive que maneja los clicks en el treemap, candidata a desaparecer o a ser reestructurada
  getRecordFromTreeMap <- reactive({
    x <- treemap_clicked$center[1]
    y <- treemap_clicked$center[2]
    
    x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
    y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
    
    l <- tmLocate(list(x=x, y=y), .tm)
    z=l[, 1:(ncol(l)-5)]
    
    
    if(is.na(z[,1]))
      return(NULL)
    
    col=as.character(z[,1])
    return(col)
    #filter(pop_data,Country==col)
  })
  
  
  
  

  

# ELEMENTOS RENDER --------------------------------------------------------


# Definición de Render para detalle del gasto -----------------------------

  output$detalle <- renderUI({
    actionButton("detalleGasto",
                 label = HTML( paste('<span style= "font-family:OpenSans;font-size:150%;color:white;bottom: 100px;">Ver
                              </span> <span style= "font-family:Montserrat;font-size:150%;color:white;"> detalle
                              </span> </br> <span style= "font-family:OpenSans;font-size:150%;color:white;">  &nbsp&nbsp del
                              </span> <span style= "font-family:Montserrat;font-size:150%;color:white;">',  aplicacion,
                                     '</span>') ), icon = icon("search-plus","fa-3x"),
                 style= paste0( 'color: #fff; background-color:', color_fuerte_boton,'; border-color:', color_fuerte_boton) )
  })
  

# Render que hace el treemap, debe ser revisada para cambiar la forma en que se hace, probablemente 
  # con la biblioteca dtree3r
  
  output$treemap1 <- renderPlot({
    gastoTreeMap()
    temp = tempo$temp
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    variable <- names( temp )[1]
    if( !is.null(temp)  ){
      if( temp != -1 ){
        .tm <<- treemap(temp,
                        index= variable,
                        vSize="Devengado",
                        vColor="Devengado",
                        type="value",
                        title = "",
                        palette="Purples",
                        border.col ="white",
                        position.legend="right",
                        fontsize.labels = 16,
                        title.legend="Escala de colores")
        mapaAnterior$mapa <- temp
        mapaAnterior$variable <- variable
      }else{
        t <- mapaAnterior$mapa
        .tm <<- treemap(t,
                        mapaAnterior$variable,
                        vSize="Devengado",
                        vColor="Devengado",
                        type="value",
                        title = "",
                        palette="Purples",
                        border.col ="white",
                        position.legend="right",
                        fontsize.labels = 16,
                        title.legend="Escala de colores")
        if( !is.null(temp) ){
          showModal(modalDialog(
            title = "Seleccione...",
            "Por favor seleccione una fila de la tabla para continuar", footer = modalButton("Continuar"), easyClose = T
          ))  
        }
      }
      
    }else{
      t <- mapaAnterior$mapa
      .tm <<- treemap(t,
                      mapaAnterior$variable,
                      vSize="Devengado",
                      vColor="Devengado",
                      type="value",
                      title = "",
                      palette="Purples",
                      border.col ="white",
                      position.legend="right",
                      fontsize.labels = 16,
                      title.legend="Escala de colores")
      if( !is.null(temp) ){
        showModal(modalDialog(
          title = "Seleccione...",
          "Por favor seleccione una fila de la tabla para continuar", footer = modalButton("Continuar"), easyClose = T
        ))  
      }    
    }
    
    
  })
  
  
  
  # Render para el mapa por departamentos
  
  output$mapa <- renderLeaflet({
    if(is.null(mapaJson)){
      withProgress(message = "Está cargando el mapa", value = 0, {
        mapaJson <<- rgdal::readOGR(dsn ="guatemala.geojson")
      })
    }
    datos <- gasto()
    if( nrow(datos) > 0  ){
      informacionMapa <- sp::merge(mapaJson, datos, by.x="Código.Departamento" )
      qpal <- colorQuantile("YlGn", informacionMapa$devengado, n = 5, na.color = "#bdbdbd")
      
      
      lat <- 16
      lng <- -89.5
      zoom <- 7
      
      mapa <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        #addTiles()%>%
        setView(lat = lat, lng = lng, zoom = zoom)%>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = informacionMapa, fillColor = ~qpal(devengado), fillOpacity = 0.7,
                    color = "white", weight = 2)
    }
    
    
    
    
  })
  
  # Render para el botón atrás que sirve para retroceder en las tablas
  
  output$Atras <- renderUI({
    actionButton("retroceder_tabla", icon = icon("glyphicon glyphicon-arrow-up", lib= "glyphicon"), label = "Nivel anterior")
  })
  
  # Render para el botón de comparador de años, el combobox
  output$comparadorAño <- renderUI({
    selectInput("yearCom",label = HTML('<p style="color:black">Ejercicio Fiscal<p>'),choices = c(1998:format(Sys.Date(), "%Y")), selected = as.numeric( input$year ) -1 )  
  })
  
  # Render para el botón que elige la dimensión de comparación en las tablas
  output$dimension <- renderUI({
    selectInput("dimension",label = 'Escoja la categoría',choices = NULL, selected = NULL )  
  })
  
  # Render para el selectInput de valor de dimensión escogida para la dimensión de comparación
  
  output$`valor-dimension` <- renderUI({
    selectInput("valor_dimension",label = 'Escoja el valor de la categoría',choices = NULL, selected = NULL)  
  })
  
  # Render para botón de comparación
  
  output$`boton-comparar` <- renderUI({
    actionButton("comparador","Comparar Ejercicios")
  })
  
  # Render para el botón de comparar la tabla trabajada para el año principal con cualquier
  # otro año en particular
  
  output$`boton-comparar-tabla` <- renderUI({
    actionButton("comparador_tabla",HTML("Comparar resultados del </br>  módulo Tabla y gráfica") )
  })
  
  # Render que dibuja el devengado en el tablero principal
  output$devengadoBox <- renderInfoBox({
    infoBox(
      HTML( paste( '<p style="font-family:Montserrat;">',titulo_box2 , '</p>' ) ), tags$p(style = paste0("font-family:Montserrat;color:", color_fuerte, ";font-size: 120%;"), paste("Q" ,formatC( devengado$d[[1]], format = "f", big.mark = ",", digits = 1) ) ) , icon = icon("money"),
      color = color_caja, width = 4
    )
  })
  
  # Render que dibuja el porcentaje de ejecución en el tablero principal
  output$progressBox <- renderInfoBox({
    infoBox(
      HTML( paste('<p style="font-family:Montserrat;">', titulo_box1 ,'</p>' ) ), tags$p(style = paste0("font-family:Montserrat;color:", color_fuerte, ";font-size: 120%;"), paste( round( devengado$p[[1]]  ,2) , "%" ) ), icon = icon("percent"),
      color = color_caja, width = 2
    )
  })
  
  
  # Render que hace botón para regresar de nivel
  output$Avanzar <- renderUI({
    actionButton("avanzar_tabla", "Siguiente nivel")
  })

  #Render para el árbol
  d3treeOutput("d3")
  
  # Encargado de mostrar el árbol finalmente.
  output$d3 <- renderD3tree({
    if( length(jerarquia_dimension_regreso) == 0) {
      p= datos_tabla %>% group_by(Entidad) %>% summarize(Devengado = sum(Devengado))
    }else{
      p=tabla_dinamica
    }
    d3tree(data = list(root = df2tree(struct = p,rootname = 'Gasto Total'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 58)
  })
  
  #Render para hacer la gráfica que acompaña a la tabla
  output$grafica <- renderPlotly({
    numeroReactivos$y
    s <- input$tabla_rows_selected
    tipo = ""
    if(nrow(tabla_temporal) < 20 ){
      tipo = 'bar'
    }else{
      tipo = 'scatter'
    }
    
    tabla_temporal$Concepto <- factor(tabla_temporal$Concepto, levels = tabla_temporal[["Concepto"]])
    
    if (is.null(s)) {
      
      p <- tabla_temporal %>%
        plot_ly(x = ~Concepto, y = ~Devengado, mode = "markers", color = I(color_fuerte), name = 'Concepto', type = tipo , text = paste("Q",formatC(tabla_temporal$Devengado,format = "f", big.mark = ",", digits = 1) ) ) %>%
        layout(xaxis = list(showticklabels = FALSE) ,showlegend = T) %>% 
        highlight("plotly_selected", color = I(color_fuerte), selected = attrs_selected(name = 'Sel'))
    } else{
      pp <- tabla_temporal %>%
        plot_ly() %>%
        add_trace(x = ~Concepto, y = ~Devengado, mode = "markers", color = I(color_fuerte), name = 'Concepto', type = tipo ) 
      # selected data
      print(s)
      # selected data
      pp <- add_trace(pp, data = tabla_temporal[s, , drop = F], x = ~Concepto, y = ~Devengado, mode = "markers",
                      color = I(color_debil), name = 'Selección', text = paste("Q",formatC(tabla_temporal[s, , drop = F]$Devengado,format = "f", big.mark = ",", digits = 1) ) )%>%
        layout( xaxis = list(showticklabels = FALSE), showlegend = T, barmode = "overlay")
      
    }
    #   datos <- tabla_temporal
    #   datos[-s, "Devengado"] =0
    #   print(datos)
    #   tabla_temporal$Devengado1 = datos$Devengado
    #   print(tabla_temporal)
    #   pp <- plot_ly(data = tabla_temporal, x = ~Concepto,  y = ~Devengado,
    #                   color = I('#c6c3ff'), name = 'Concepto') %>%
    #     add_trace( y = ~Devengado1, name = "Selección") %>%
    #     layout(yaxis = list(title = 'Devengado'),barmode = 'stack', showlegend = T )
    # }
    
  })
  
  
  #Render para hacer la gráfica comparativa
  output$grafCon <- renderPlotly({
    temp <- datos_comparativos$data
    
    tipo = ""
    if(nrow(tabla_temporal) < 20 ){
      tipo = 'bar'
    }else{
      tipo = 'scatter'
    }
    
    
    
    
    if (is.null(temp)) {
      
      p <- tabla_temporal %>%
        plot_ly(x = ~Concepto, y = ~Devengado, mode = "markers", color = I(color_debil), name = 'Concepto', type = tipo, text = paste("Q",formatC(tabla_temporal$Devengado,format = "f", big.mark = ",", digits = 1) ) ) %>%
        layout(xaxis = list(showticklabels = FALSE) ,showlegend = T) %>% 
        highlight("plotly_selected", color = I(color_fuerte), selected = attrs_selected(name = 'Filtered'))
    } else{
      if(nrow(temp) < 20 ){
        tipo = 'bar'
      }else{
        tipo = 'scatter'
      }
      
      p <- plot_ly(temp, x = ~Concepto, y = ~Devengado.y, name = paste("Devengado", input$yearCom), color = I(color_debil), type = tipo ) %>%
        add_trace(y = ~Devengado.x, name = paste("Devengado", input$year), color = I(color_fuerte)) %>%
        layout(yaxis = list(title = 'Count'))
      
      
    }
    
  })
  
  
  #Render para tabla de comparación
  output$`tabla-comparacion` <- DT::renderDataTable({
    if( !is.null(comparativo_tabla$data) ){
      DT::datatable(comparativo_tabla$data,options = list(
        # dom = 't',
        # deferRender = TRUE,
        searching = TRUE,
        # autoWidth = TRUE,
        # scrollCollapse = TRUE,
        rownames = FALSE,
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = "500px",
        fixedHeader = TRUE,
        class = 'cell-border stripe',
        fixedColumns = list(
          leftColumns = 3,
          heightMatch = 'none'
        ), language =  list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json') ) ) %>%
        DT::formatCurrency(c(2:4),currency = "Q")
    }
  }
  )
  
  
# OBSERVE EVENTS ----------------------------------------------------------

# En esta sección van todos los observe events 
  
  # Observe Event  para el botón del detalle del gasto
  
  
  observeEvent(input$detalleGasto,{
    updateTabItems(session = session, "tabs", selected = "anio")
  }
               )
  
  
  # Implementación para el botón de atrás de los treemaps, sujeta a revisiones
  
  observeEvent(input$atrasTM,{ 
    print("Hola")
    print(app.env$nivelActivo)
    print(mapaAnterior$filtro)
    switch ( input$filtro,
             'Entidad' = {
               temp = valores_institucional[ length(valores_institucional) -1 ]
               temp = as.data.frame(temp[[1]][[2]])
               filtro = pop(jerarquia_institucional_regreso)
               push(jerarquia_institucional_ida, paste0("'",filtro, "'") )
               jerarquia_institucional_ida <<- jerarquia_institucional_ida
               jerarquia_institucional_regreso <<- jerarquia_institucional_regreso
               # if ( !is.empty(jerarquia_institucional_regreso) ){
               #   filtro = pop(jerarquia_institucional_regreso)
               #   push(jerarquia_institucional_ida, paste0("'",filtro, "'") )  
               # }else{
               #   return(-1)
               # }
               # nivel = valores_institucional[length(valores_institucional)]
               # if( is.null(nivel) || trimws(nivel) == "" ){
               #   shinyjs::hide("atrasTM")
               #   temp <- datos_intitucional %>%
               #     select_(filtro, "Devengado") %>%
               #     group_by_(filtro) %>%
               #     summarise(Devengado = sum(Devengado)) 
               # }else{
               #   shinyjs::show("atrasTM")
               #   va <- app.env$nivelActivo
               #   temp <- datos_intitucional %>%
               #     select_(app.env$nivelActivo,  filtro, "Devengado") %>%
               #     filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
               #     group_by_(filtro) %>%
               #     summarise(Devengado = sum(Devengado))
               # }
               # 
               # if(nrow(temp) >1 ){
               #   jerarquia_institucional_ida <<- jerarquia_institucional_ida
               #   jerarquia_institucional_regreso <<- jerarquia_institucional_regreso
               #   app.env$nivelActivo = filtro  
               # }else{
               #   return(NULL)
               # }
               
             }, 
             'Finalidad' = {
               filtro = pop(jerarquia_finalidad_ida)
               push(jerarquia_finalidad_regreso, paste0("'",filtro, "'") )
               if( is.null(nivel) || trimws(nivel) == "" ){
                 temp <- datos_finalidad %>%
                   select_(filtro, "Devengado") %>%
                   group_by_(filtro) %>%
                   summarise(Devengado = sum(Devengado)) 
               }else{
                 va <- app.env$nivelActivo
                 temp <- datos_finalidad %>%
                   select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                   filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                   group_by_(filtro) %>%
                   summarise(Devengado = sum(Devengado))
               }
               jerarquia_finalidad_ida <<- jerarquia_finalidad_ida
               jerarquia_finalidad_regreso <<- jerarquia_finalidad_regreso
               app.env$nivelActivo = filtro
             }, 
             'Código.Departamento' = {
               filtro = pop(jerarquia_geografico_ida)
               push(jerarquia_geografico_regreso, paste0("'",filtro, "'") )
               if( is.null(nivel) || trimws(nivel) == "" ){
                 temp <- datos_geografico %>%
                   select_(filtro, "Devengado") %>%
                   group_by_(filtro) %>%
                   summarise(Devengado = sum(Devengado)) 
               }else{
                 va <- app.env$nivelActivo
                 temp <- datos_geografico %>%
                   select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                   filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                   group_by_(filtro) %>%
                   summarise(Devengado = sum(Devengado))
               }
               jerarquia_geografico_ida <<- jerarquia_geografico_ida
               jerarquia_geografico_regreso <<- jerarquia_geografico_regreso
               app.env$nivelActivo = filtro
             }, 
             'Objeto del gasto' = {
               filtro = pop(jerarquia_objeto_gasto_ida)
               push(jerarquia_objeto_gasto_regreso, paste0("'",filtro, "'") )
               if( is.null(nivel) || trimws(nivel) == "" ){
                 temp <- datos_objeto_gasto %>%
                   select_(filtro, "Devengado") %>%
                   group_by_(filtro) %>%
                   summarise(Devengado = sum(Devengado)) 
               }else{
                 va <- app.env$nivelActivo
                 temp <- datos_objeto_gasto %>%
                   select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                   filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                   group_by_(filtro) %>%
                   summarise(Devengado = sum(Devengado))
               }
               jerarquia_objeto_gasto_ida <<- jerarquia_objeto_gasto_ida
               jerarquia_objeto_gasto_regreso <<- jerarquia_objeto_gasto_regreso
               app.env$nivelActivo = filtro
             }, 
             'Economico' = {
               filtro = pop(jerarquia_economico_ida)
               push(jerarquia_economico_regreso, paste0("'",filtro, "'") )
               if( is.null(nivel) || trimws(nivel) == "" ){
                 temp <- datos_economico %>%
                   select_(filtro, "Devengado") %>%
                   group_by_(filtro) %>%
                   summarise(Devengado = sum(Devengado)) 
               }else{
                 va <- app.env$nivelActivo
                 temp <- datos_economico %>%
                   select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                   filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                   group_by_(filtro) %>%
                   summarise(Devengado = sum(Devengado))
               }
               jerarquia_economico_ida <<- jerarquia_economico_ida
               jerarquia_economico_regreso <<- jerarquia_economico_regreso
               app.env$nivelActivo = filtro
             }
    )
    tempo$temp = temp
    a = 5
    
  }
  )
  
  # Fin de atrasTM

  # Manejador de los clicks, candidato a eliminación o restructuración
  observeEvent(input$click_treemap, {
    x <- input$click_treemap$x
    y <- input$click_treemap$y
    treemap_clicked$center <- c(x,y)
    
    if(is.null(treemap_clicked$for_condition)){
      treemap_clicked$for_condition=c(x,y)
    }
    else{treemap_clicked$for_condition=NULL}
    
  })
  
  # Observe Event que se encarga de cargar la base y hacer la primera tablas, 
  # se debe considerar su restructuración, ya que los métodos de momento son muy ineficientes
  
  observeEvent(input$year,{
    req(input$year)
    tryCatch({
      
      withProgress(message ='Leyendo la información', value = 0, {
        datos_tabla <<- as.data.frame(fread(paste0("Data/",nombre_tablas, input$year,'.csv'), sep = ';'))
        jerarquia_dimension_regreso = list()  #cambio de estructura de datos
        jerarquia_valor_dimension_regreso = list() #antes era lifo, se pasa a lista
        tabla_temporal <<- NULL
        tabla_dinamica <<- NULL
        
        if(input$year %in% c('2004','2005','2006','2007') && aplicacion == 'gasto'){
          opciones_filtro_inicio <<- list( 
            "Entidad",
            "Finalidad",
            "Región",
            "Grupo",
            "Económico Nivel 1" 
          )
          
          mascara_filtro_inicio <<- list(
            "Institución",
            "Finalidad",
            "Clasificación Geográfica",
            "Objeto del gasto" ,
            "Económico del gasto" 
          )
        }else{
          if( aplicacion == "gasto" ){
            opciones_filtro_inicio <- list("Sub Grupo", 
                                           "Entidad",
                                           "Finalidad",
                                           "Región",
                                           "Grupo",
                                           "Económico Nivel 1" 
            )
            
            mascara_filtro_inicio <- list("Sub grupo (Institucional)",
                                          "Institución",
                                          "Finalidad",
                                          "Clasificación Geográfica",
                                          "Objeto del gasto" ,
                                          "Económico del gasto" 
            )
          }
          
        }
        
      }) 
      
      
      
      datos = gasto_tabla() %>%
        mutate(
          Acciones =  as.character(dropdownButton( icon = icon("gear"), right = T, checkboxGroupButtons(
            inputId = paste0("btFiltroN",1+numeroReactivos$y),
            choiceNames =  mascara_filtro_inicio, choiceValues = opciones_filtro_inicio, direction = "vertical") ) )  
        )
      
      temp <- isolate( numeroReactivos$y )
      numeroReactivos$y = temp + 1
      numeroReactivos$x <- temp
      
      output$tabla <- DT::renderDataTable({
        a <- DT::datatable( datos, escape = F, 
                            options = list(orderClasses = TRUE,
                                           dom = 'Bfrtip',
                                           searching = TRUE,
                                           # scrollCollapse = TRUE,
                                           rownames = FALSE,
                                           scroller = TRUE,
                                           scrollX = TRUE,
                                           scrollY = "500px",
                                           fixedHeader = TRUE,
                                           class = 'cell-border stripe',
                                           dom = 'tB',
                                           buttons = c('copy', 'csv', 'excel', 'pdf'),
                                           fixedColumns = list(
                                             leftColumns = 3,
                                             heightMatch = 'none'),
                                           language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
                                           ),
                                           preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                           drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')   ), class = "display" )
        if( length( calculos ) > 0 ){
          a <- a %>% 
            DT::formatCurrency(metricas_currency, currency = "Q") %>%
            DT::formatPercentage(ncol(datos) - 1, digits = 2 )
        }else{
          a <-  a %>%
            DT::formatCurrency(metricas_currency, currency = "Q") 
          
        }
        tabla_inutil <<- a
      })
      if(isolate(numeroReactivos$x) == 0){
        escribirReactivos( lon = isolate(numeroReactivos$y) )
      }
      
      output$`grafica-inicio` <- renderPlotly({
        if(aplicacion == 'gasto'){
          temp <- datos_tabla %>%
            select(Entidad,Devengado)%>%
            group_by(Entidad) %>%
            summarise(Devengado = sum(Devengado)) %>%
            rename("Concepto" = Entidad)
          
        }else{
          temp <- datos_tabla %>%
            select(Clase,Devengado)%>%
            group_by(Clase) %>%
            summarise(Devengado = sum(Devengado)) %>%
            rename("Concepto" = Clase)
          
        }
        
        
        
        temp <- temp[order(-temp$Devengado),]
        temp$Concepto <- factor(temp$Concepto, levels = temp[["Concepto"]])
        tabla_temporal$Concepto <- factor(tabla_temporal$Concepto, levels = tabla_temporal[["Concepto"]])
        
        
        
        
        
        
        p <- temp %>%
          plot_ly(x = ~Concepto, y = ~Devengado, mode = "markers", color = I(color_debil), name = 'Entidad', type = 'bar', text = paste("Q",formatC(temp$Devengado,format = "f", big.mark = ",", digits = 1) ) 
          ) %>%
          layout(xaxis = list(showticklabels = FALSE) ,showlegend = T) 
        
      })
      
      
      
      
    }, error = function(e){ 
      showModal(modalDialog(
        title = "Error",
        "No se pudo cargar el conjunto de datos, intente más tarde", footer = modalButton("Continuar"), easyClose = T
      ))
    }
    )
    
  })
  
  #Fin de input$year 
  
  #Función que permite la descarga en CSV, se necesita mejorar
  output$descarga <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(fname){
      write.csv2(tabla_temporal,fname)
    }
  )
  
  #Observe event que se encarga de capturar que fila fue seleccionada
  
  observeEvent(input$tabla_rows_selected,{
    if(  !is.null(input$tabla_rows_selected)  )
    {
      filaSeleccionada <<- input$tabla_rows_selected
    }
  }
  )  

  
  # Función para retroceder la tabla
  
  observeEvent(input$retroceder_tabla, {
    lista <- construirTablaDinamica()
    tablita <- lista[[1]]
    filtro <- as.character(lista[[2]])
    if(length(jerarquia_dimension_regreso) == 0){
      val_filtros = list(opciones_filtro_inicio, mascara_filtro_inicio)   
    }else{
      val_filtros = actualizarFiltro(filtro)
    }
    
    inputs <- character( nrow(tablita) )
    for( i in seq_len(nrow(tablita)) ){
      print(  paste("Haciendo el botón:", paste0("btFiltroN",i+numeroReactivos$y)) )
      inputs[i] <- as.character(dropdownButton( icon = icon("gear"), right = T, checkboxGroupButtons(
        inputId = paste0("btFiltroN",i+numeroReactivos$y), 
        choiceNames = val_filtros[[2]], choiceValues = val_filtros[[1]], direction = "vertical") ) )  
    }
    temp <- isolate( numeroReactivos$y )
    numeroReactivos$y = temp + nrow(tablita)
    numeroReactivos$x <- temp
    datos <- as.data.frame( tablita ) %>%
      mutate( 
        Acciones =  inputs
      )
    
    
    output$tabla <- DT::renderDataTable({
      a <- DT::datatable( datos, escape = F, 
                          list(orderClasses = TRUE,
                               searching = TRUE,
                               dom = 'Bfrtip',
                               # scrollCollapse = TRUE,
                               rownames = FALSE,
                               scroller = TRUE,
                               scrollX = TRUE,
                               scrollY = "500px",
                               fixedHeader = TRUE,
                               class = 'cell-border stripe',
                               buttons = c('copy', 'csv', 'excel', 'pdf'),
                               fixedColumns = list(
                                 leftColumns = 3,
                                 heightMatch = 'none'),
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                               preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                               drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')   ) )%>%
        DT::formatCurrency(metricas_currency, currency = "Q") 
      if( length( calculos ) > 0 ){
        a <- a %>%
          DT::formatCurrency(metricas_currency, currency = "Q") %>%
          DT::formatPercentage(ncol(datos) - 1, digits = 2 )
      }else{
        a <- a %>%
          DT::formatCurrency(metricas_currency, currency = "Q") 
        
      }
      tabla_inutil <<- a
    })
    #obtenerArbol()
    
    
    
    
  })
  #Fin atras tabla
  
  
  #Observer para el select input que carga la información de un año para comparar
  observeEvent(input$yearCom,{print(input$yearCom)
    tryCatch({
      withProgress(message ='Leyendo la información', value = 0, {
        datos_tabla_con <<-  as.data.frame(data.table::fread(paste0('Data/',nombre_tablas,input$yearCom,'.csv'), sep =';'))
      })
      dimension <- datos_tabla_con %>%
        select_if(Negate(is.numeric) ) %>%
        top_n(2)
      dimension <- names(dimension)
      print( as.list(dimension)[[1]] )
      updateSelectInput(session = session,"dimension", choices = as.list(dimension), selected = as.list(dimension)[[1]] ) 
      
      
    }, error = function(e){
      showModal(modalDialog(
        title = "",
        "No se pudo cargar el conjunto de datos, intente más tarde", footer = modalButton("Continuar"), easyClose = T
      ))
    })
    
    
    
  })
  
  
  #Observer para el select input de la dimensión
  
  observeEvent(input$dimension,{
    
    print(input$dimension)
    filtros = datos_tabla_con %>%
      select_( as.name(req(input$dimension) ) ) %>%
      unique()
    
    
    if( length(filtros[[1]]) > 1 ){
      filtros <- c( "Todas" ,filtros[[1]] )
    }else{
      filtros <- filtros[[1]]
    }
    
    
    updateSelectInput(session = session, "valor_dimension", choices = filtros )
  }
  )
  
  #Observe event para actualizar el arbol
  
  observeEvent(input$d3_update,{
    network$nodes <- unlist(input$d3_update$.nodesData)
    activeNode<-input$d3_update$.activeNode
    if(!is.null(activeNode)) network$click <- jsonlite::fromJSON(activeNode)
  })
  
  
  #Observe Evente con relación al árbol, función desconocida y candidata a eliminación
  
  observeEvent(network$nodes,{
    output$results <- renderPrint({
      str.out=''
      if(!is.null(network$nodes)) str.out=tree.filter(network$nodes,m)
      return(str.out)
    })    
  })
  
  
  #Observe Event que compara los datos de un año con otro
  observeEvent(input$comparador,{
    req(input$comparador)
    
    tryCatch( {
      
      if( input$valor_dimension == "Todas"){
        
        data1 <- datos_tabla %>%
          group_by_( as.name(input$dimension) ) %>%
          summarise_at(metricas, sum)
        
        if(length(calculos) > 0){
          data1 <- data1 %>%
            mutate_(.dots = setNames(.dots1,"Ejecutado"))
        }
        
        
        # .dots <- list(interp(~ x == y , .values = list(x = input$dimension, y = input$valor_dimension ) ) )
        
        
        data2 <- datos_tabla_con %>%
          group_by_( input$dimension ) %>%
          summarise_at(metricas, sum)
        
        if(length(calculos) > 0){
          data2 <- data2 %>%
            mutate_(.dots = setNames(.dots1,"Ejecutado"))
        }
      }else{
        .dots <- list(interp(~ x == y , .values = list(x = as.name(input$dimension), y = input$valor_dimension ) ) )
        
        data1 <- datos_tabla %>%
          filter_( .dots = .dots) %>%
          group_by_( as.name(input$dimension) ) %>%
          summarise_at(metricas, sum)
        
        if(length(calculos) > 0){
          data1 <- data1 %>%
            mutate_(.dots = setNames(.dots1,"Ejecutado"))
        }
        
        
        # .dots <- list(interp(~ x == y , .values = list(x = input$dimension, y = input$valor_dimension ) ) )
        
        
        data2 <- datos_tabla_con %>%
          filter_( .dots = .dots) %>%
          group_by_( input$dimension ) %>%
          summarise_at(metricas, sum)
        
        if(length(calculos) > 0){
          data2 <- data2 %>%
            mutate_(.dots = setNames(.dots1,"Ejecutado"))
        }
      }
      
      
      
      data1 <- data1 %>%
        rename_(Concepto = input$dimension )
      
      data2 <- data2  %>%
        rename_(Concepto = input$dimension )
      
      data1 <- data1 %>%
        select(Concepto,Devengado)
      
      data2 <- data2 %>%
        select(Concepto,Devengado)
      
      tabla_merge <- merge(data1, data2, by = "Concepto")
      print(tabla_merge)
      
      if( nrow(tabla_merge) == 0 ){
        showModal(modalDialog(
          title = "",
          "La comparación no se pudo hacer", footer = modalButton("Continuar"), easyClose = T
        ))
      }else{
        datos_comparativos$data <- tabla_merge
      }
    }
    
    
    , error = function(e){
      showModal(modalDialog(
        title = "Error",
        "No se pudo realizar la comparación", footer = modalButton("Continuar"), easyClose = T
      )) 
    })
    
    
    
    
    
  })
  
  
  #Observe event que compara los datos de la tabla generada en módulo principal con otro año
  #en particular
  observeEvent(input$comparador_tabla, {
    tabla_temp <- NULL
    tryCatch({
      
      
      
      if( length(jerarquia_dimension_regreso  )  == 0 ){
        tabla_temp <- datos_tabla_con %>%
          summarise( Devengado = sum(Devengado) )%>%
          mutate(Concepto = Concepto[[1]])
      }else{
        jerarquia_dimension_regreso <- c(jerarquia_dimension_regreso,dimension_ida)
        jerarquia_valor_dimension_regreso <- c(jerarquia_valor_dimension_regreso, "PlaceHolder")
        contador <- 2:length(jerarquia_dimension_regreso)
        for(z in  contador ){
          a <- jerarquia_dimension_regreso[[z]]
          b <- jerarquia_valor_dimension_regreso[[z]]
          .dots <- list(interp( ~y==x, .values = list( y = as.name(jerarquia_dimension_regreso[[z]] ), x = jerarquia_valor_dimension_regreso[[z]] ) ) )
          if( z == length(jerarquia_dimension_regreso) ) {
            if( z == 2 ){
              tabla_temp <- datos_tabla_con %>%
                group_by_( as.name(jerarquia_dimension_regreso[[z]]) ) %>%
                summarise( Devengado = sum(Devengado) ) %>%
                rename_(Concepto = as.name(jerarquia_dimension_regreso[[z]]) )
              dimension_ida <<- jerarquia_dimension_regreso[[z]]
            }else{
              tabla_temp <- tabla_temp %>%
                group_by_( as.name(jerarquia_dimension_regreso[[z]]) ) %>%
                summarise( Devengado = sum(Devengado) ) %>%
                rename_(Concepto = as.name(jerarquia_dimension_regreso[[z]])  )
              dimension_ida <<- jerarquia_dimension_regreso[[z]]
            }
          }else if( z == 2 ){
            tabla_temp <- datos_tabla_con %>%
              filter_(.dots = .dots)
          }else{
            tabla_temp <- tabla_temp %>%
              filter_(.dots = .dots)
          }
        }
      }
      # names(tabla_temp) = sub('Devengado', paste("Devengado", input$yearCom), names(tabla_temp))
      # 
      tabla_temporal <- tabla_temporal[,c("Concepto","Devengado")]
      
      tabla_temp <- tabla_temp[,c("Concepto","Devengado")]
      #   
      # 
      # names(tabla_temporal) = sub('Devengado', paste("Devengado", input$year), names(tabla_temporal))
      tabla_merge <- merge( tabla_temp,tabla_temporal, by = "Concepto")
      print(tabla_merge)
      datos_comparativos$data <- tabla_merge
      
    }
    , error = function(e) {
      print(e)
      showModal(modalDialog(
        title = "Error",
        "No se pudo realizar la comparación", footer = modalButton("Continuar"), easyClose = T
      )) 
    }  )
    
  }
  )
  
# FUNCIONES, NO REACTIVE --------------------------------------------------

# Función de utilidad para hacer el Treemap, no está implementada como reactive  

  
  gastoTreeMap <- function()({
    
    
    temp <- NULL
    nivel <- getRecordFromTreeMap()
    # if( !is.null(isolate(mapaAnterior$variable)) && !is.null(nivel) ){
    #   if (nivel == isolate(mapaAnterior$variable) ){
    #     return(NULL)
    #   }
    # }
    #    
    
      
    if( !is.null(input$filtro) ){
      switch ( input$filtro,
              'Entidad' = {
                if ( !is.empty(jerarquia_institucional_ida) ){
                  filtro = pop(jerarquia_institucional_ida)
                  push(jerarquia_institucional_regreso, paste0("'",filtro, "'") )  
                }else{
                  return(-1)
                }
                
                if( is.null(nivel) || trimws(nivel) == "" ){
                  shinyjs::hide("atrasTM")
                  temp <- datos_intitucional %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  shinyjs::show("atrasTM")
                  va <- app.env$nivelActivo
                  temp <- datos_intitucional %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                valores_institucional <- list(valores_institucional, temp)
                valores_institucional <<- valores_institucional
                if(nrow(temp) >1 ){
                  jerarquia_institucional_ida <<- jerarquia_institucional_ida
                  jerarquia_institucional_regreso <<- jerarquia_institucional_regreso
                  app.env$nivelActivo = filtro  
                }else{
                  return(NULL)
                }
                
              }, 
              'Finalidad' = {
                filtro = pop(jerarquia_finalidad_ida)
                push(jerarquia_finalidad_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_finalidad %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_finalidad %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_finalidad_ida <<- jerarquia_finalidad_ida
                jerarquia_finalidad_regreso <<- jerarquia_finalidad_regreso
                app.env$nivelActivo = filtro
              }, 
              'Código.Departamento' = {
                filtro = pop(jerarquia_geografico_ida)
                push(jerarquia_geografico_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_geografico %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_geografico %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_geografico_ida <<- jerarquia_geografico_ida
                jerarquia_geografico_regreso <<- jerarquia_geografico_regreso
                app.env$nivelActivo = filtron
              }, 
              'Objeto del gasto' = {
                filtro = pop(jerarquia_objeto_gasto_ida)
                push(jerarquia_objeto_gasto_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_objeto_gasto %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_objeto_gasto %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_objeto_gasto_ida <<- jerarquia_objeto_gasto_ida
                jerarquia_objeto_gasto_regreso <<- jerarquia_objeto_gasto_regreso
                app.env$nivelActivo = filtro
              }, 
              'Economico' = {
                filtro = pop(jerarquia_economico_ida)
                push(jerarquia_economico_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_economico %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_economico %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_economico_ida <<- jerarquia_economico_ida
                jerarquia_economico_regreso <<- jerarquia_economico_regreso
                app.env$nivelActivo = filtro
              }
      )
    }
    tempo$temp <- temp
    return(temp)
  })
  

  # Función útil para la actulización de filtros
  
  actualizarFiltro <-  function(filtro="") {
    if( !is.null( tabla_dinamica )  ){
      
      valores_filtros <- sapply(colnames(tabla_dinamica),function( x ){
        y <- tabla_dinamica[,x] 
        if( !is.numeric(y) ){
          y <- as.factor( as.character(y) )
          if( nlevels(y) >  1  ){
            return(x)
          }
        }
      })
      
      
      
      valores_filtros <- plyr::compact(valores_filtros)
      
      if (exists("filtro")) {
        valores_filtros <- valores_filtros[valores_filtros != filtro]
      }else{
        valores_filtros <- valores_filtros[valores_filtros != dimension_ida]
      }
      
      lista_filtros <- obtenerListaFiltros(valores_filtros)
      
      #View(valores_filtros)
      
      
      if(length(length(lista_filtros[[1]])) > 0 )
        updateRadioButtons(session,"opcionTabla", choiceNames  = lista_filtros[2], choiceValues = lista_filtros[1], inline = T) 
      return(lista_filtros)
    }else{
      updateRadioButtons(session,"opcionTabla", choiceNames = mascara_filtro_inicio, choiceValues =  opciones_filtro_inicio, inline = T) 
      
      return(list(opciones_filtro_inicio,mascara_filtro_inicio))
    }
    
  }  
  
  # Función útil para crear elementos de entrada en las tablas, usando DataTables
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  

  #Función que hace la tabla de ingreso/gasto según filtros
  # dados por el usuario, requiere readecuación a valores reactivos
  
  gasto_tabla <- function(filtro = '', variable = ''){
    
    var <-  variable
    temporal <- NULL
    if( is.null(var) || var == '' ){
      temp <- datos_tabla %>%
        summarise_at(metricas, sum)
      
      if(length(calculos) > 0){
        temp <- temp %>%
          mutate_(.dots = setNames(.dots1,"Ejecutado"))
      }
      
      
      
      temporal <- cbind.data.frame(setNames(Concepto,"Concepto" ), temp)
      
      
      
    }else{
      resultado <- tabla_temporal[var, "Concepto"] 
      if(resultado == Concepto[[1]]){
        codigo = as.character( tabla_parejamientos[tabla_parejamientos$nombres_reales == filtro,][[3]] )
        temporal <- datos_tabla %>%
          group_by_( as.name(codigo) , as.name(filtro)) %>%
          summarise_at(metricas, sum)
        
        if(length(calculos) > 0){
          temporal <- temporal %>%
            mutate_(.dots = setNames(.dots1,"Ejecutado"))
        }
        
        
        tabla_dinamica <<- datos_tabla
        
        if( filtro == codigo ){
          temporal <- temporal %>%
            rename_(Concepto = filtro)
        }else{
          temporal <- temporal %>%
            rename_(Concepto = as.name(filtro), Código = as.name(codigo) )  
        }
        
        
        
        dimension_ida <<- filtro
        #push(jerarquia_dimension_regreso,filtro)
        jerarquia_dimension_regreso <- c(jerarquia_dimension_regreso,filtro)
        #push(jerarquia_valor_dimension_regreso,"")
        jerarquia_valor_dimension_regreso <- "NADA"
        
        
        
        
      }else{
        col <- as.character(tabla_temporal$Concepto[variable] )
        dimension_actual <- dimension_ida
        .dots <- list(interp(~y==x, .values = list( y = as.name(dimension_actual), x = col ) ))
        tabla_dinamica <<- tabla_dinamica %>%
          filter_( .dots = .dots )
        
        codigo = as.character( tabla_parejamientos[tabla_parejamientos$nombres_reales == filtro,][[3]] )
        temporal <- tabla_dinamica %>%
          group_by_( as.name( codigo ) , as.name(filtro) ) %>%
          summarise_at(metricas, sum)
        
        if(length(calculos) > 0){
          temporal <- temporal %>%
            mutate_(.dots = setNames(.dots1,"Ejecutado"))
        } 
        
        if( filtro == codigo){
          
          temporal <- temporal %>%
            rename_( Concepto = as.name( filtro ) )
        }else{
          
          temporal <- temporal %>%
            rename_( Concepto = as.name( filtro ), Código = as.name(codigo ) )
        }
        
        dimension_ida <<- filtro # quizás lo correcto es filtro actualmente es dimension_actual
        #push(jerarquia_dimension_regreso, dimension_actual)
        jerarquia_dimension_regreso <- c(jerarquia_dimension_regreso, dimension_actual)
        valor_dimension <<- col
        #push(jerarquia_valor_dimension_regreso, col)
        jerarquia_valor_dimension_regreso <- c(jerarquia_valor_dimension_regreso, col)
      }
      
      
    }
    
    
    print(temporal)
    
    names(temporal) <- make.names(names(temporal),unique = T)
    
    
    
    
    
    
    
    #actualizarFiltro(filtro)
    
    jerarquia_valor_dimension_regreso <<- jerarquia_valor_dimension_regreso
    jerarquia_dimension_regreso <<- jerarquia_dimension_regreso
    
    #Ordenando
    
    if( filtro == "Nombre Mes"){
      temporal <- temporal[order(temporal$Código),]  
    }else{
      temporal <- temporal[order(-temporal$Devengado),]  
    }
    
    tabla_temporal <<- temporal
    
    return( temporal )
  }

  
  #Fin de gasto tabla 


  
  # Función para reconstruir la tabla dinámica
  # Requiere restructuración
  
  construirTablaDinamica <- function(){
    tabla_temp <- NULL
    if( length(jerarquia_dimension_regreso  )  == 1 ){
      
      temporal <- datos_tabla %>%
        summarise_at(metricas, sum)
      
      if(length(calculos) > 0){
        temporal <- temporal %>%
          mutate_(.dots = setNames(.dots1,"Ejecutado"))
      }
      
      
      
      temporal <- cbind.data.frame(setNames(Concepto,"Concepto" ), temporal)
      
      
      tabla_temp <- temporal
      
    }else{
      contador <- 2:length(jerarquia_dimension_regreso)
      for(z in  contador ){
        a <- jerarquia_dimension_regreso[[z]]
        b <- jerarquia_valor_dimension_regreso[[z]]
        .dots <- list(interp( ~y==x, .values = list( y = as.name(jerarquia_dimension_regreso[[z]] ), x = jerarquia_valor_dimension_regreso[[z]] ) ) )                      
        if( z == length(jerarquia_dimension_regreso) ) {
          codigo = as.character( tabla_parejamientos[tabla_parejamientos$nombres_reales == jerarquia_dimension_regreso[[z]],][[3]] )
          if( z == 2 ){
            tabla_dinamica <<- datos_tabla 
            tabla_temp <- datos_tabla %>%
              group_by_( as.name(codigo) , as.name(jerarquia_dimension_regreso[[z]]) ) %>%
              summarise_at(metricas, sum)
            
            if(length(calculos) > 0){
              tabla_temp <- tabla_temp %>%
                mutate_(.dots = setNames(.dots1,"Ejecutado"))
            }
            if( jerarquia_dimension_regreso[[z]] == codigo ){
              tabla_temp <- tabla_temp %>%
                rename_(Concepto =  as.name(jerarquia_dimension_regreso[[z]]))
            }else{
              tabla_temp <- tabla_temp %>%
                rename_(Concepto = as.name(jerarquia_dimension_regreso[[z]]), Código = as.name(codigo)  )
            }
            
            
            dimension_ida <<- jerarquia_dimension_regreso[[z]]
          }else{
            tabla_dinamica <<- tabla_temp
            tabla_temp <- tabla_temp %>%
              group_by_( as.name(codigo), as.name(jerarquia_dimension_regreso[[z]]) ) %>%
              summarise_at(metricas, sum)
            
            if(length(calculos) > 0){
              tabla_temp <- tabla_temp %>%
                mutate_(.dots = setNames(.dots1,"Ejecutado"))
            }
            
            if( jerarquia_dimension_regreso[[z]] == codigo ){
              tabla_temp <- tabla_temp %>%
                rename_(Concepto = as.name(jerarquia_dimension_regreso[[z]]) )
            }else{
              tabla_temp <- tabla_temp %>%
                rename_(Concepto = as.name(jerarquia_dimension_regreso[[z]]), Código = as.name(codigo) )
            }
            
            # 
            # tabla_temp <- tabla_temp %>%
            #   rename_(Concepto = jerarquia_dimension_regreso[[z]], Código = codigo)
            dimension_ida <<- jerarquia_dimension_regreso[[z]]
          }
        }else if( z == 2 ){
          tabla_temp <- datos_tabla %>%
            filter_(.dots = .dots)
        }else{
          tabla_temp <- tabla_temp %>%
            filter_(.dots = .dots)
        }
      }
    }
    #tomando el valor del último elemento de dimension
    last_filt = jerarquia_dimension_regreso[length(jerarquia_dimension_regreso)]
    
    #Quitando el ultimo elemento
    jerarquia_dimension_regreso <<- jerarquia_dimension_regreso[c(1:length(jerarquia_dimension_regreso) -1 )]
    jerarquia_valor_dimension_regreso <<- jerarquia_valor_dimension_regreso[c(1:length(jerarquia_valor_dimension_regreso) -1 )]
    #View(tabla_temp)
    tabla_temp <- tabla_temp[order(-tabla_temp$Devengado),]
    tabla_temporal <<- tabla_temp
    return(list(tabla_temp,last_filt))
    #View(tabla_temporal)
    #View(tabla_dinamica)
  }
  
  #Fin de contruir tabla dinámica

  
  
  # Función que obtiene los filtros basándose en las jeraquias definidas 
  
  obtenerListaFiltros <- function( valores ){
    listaExclusion <- NULL
    nombres <- names(valores)
    #View(nombres)
    
    a <- lapply(jerarquias, function(x,y){
      a <- length(x) - 1
      lapply( c(1:a), function(z){
        if(x[z] %in% y){
          listaExclusion <<- c(listaExclusion, x[ c(z+1:length(x)) ] )
        }
      })
    }, y = nombres)
    
    
    
    
    nombres <- names(valores)[-which( names(valores) %in% listaExclusion )]
    tempo_bonitos <- lapply(nombres, function(x){
      return( as.character( tabla_parejamientos[tabla_parejamientos$nombres_reales == x, ][[2]]))
    })
    return(list(nombres, tempo_bonitos))
    
  }  
  
# Función que hace el árbol, desactulizada, necesita cambios
  obtenerArbol <- function(){
    arbol <- NULL
    if( length(jerarquia_dimension_regreso) == 0) {
      p= datos_tabla %>% group_by(Entidad) %>% summarize(Devengado = sum(Devengado)) %>% distinct
    }else{
      valores <- sapply(jerarquia_dimension_regreso, as.character)
      p= datos_tabla %>% select_(.dots = valores) %>% mutate(NEWCOL = NA)  %>% distinct
    }
    try( {arbol <- df2tree(struct = p,rootname = 'Gasto Total')
    output$d3 <- renderD3tree({
      d3tree(data = list(root = arbol , layout = 'collapse') , height = 500, width = 500)
    }) })
    
  }
  

  # Función para hacer la tabla para hacer el render
  # Necesita restructurar
  
  hacerTabla <- function( filtro,variable ){
    tablita <- gasto_tabla(filtro, variable)
    val_filtros = actualizarFiltro(filtro)
    inputs <- character( nrow(tablita) )
    for( i in seq_len(nrow(tablita)) ){
      print(  paste("Haciendo el botón:", paste0("btFiltroN",i+numeroReactivos$y)) )
      inputs[i] <- as.character(dropdownButton( icon = icon("gear"), right = T, checkboxGroupButtons(
        inputId = paste0("btFiltroN",i+numeroReactivos$y), 
        choiceNames  =  val_filtros[[2]], choiceValues = val_filtros[[1]] , direction = "vertical") ) )  
    }
    temp <- isolate( numeroReactivos$y )
    numeroReactivos$y = temp + nrow(tablita)
    numeroReactivos$x <- temp
    datos <- as.data.frame( tablita ) %>%
      mutate( 
        Acciones =  inputs
      )
    output$tabla <- DT::renderDataTable({ 
      tabla_inutil <<- a <- DT::datatable( datos, escape = F, 
                                           options = list(orderClasses = TRUE,
                                                          dom = 'Bfrtip',
                                                          searching = TRUE,
                                                          # scrollCollapse = TRUE,
                                                          rownames = FALSE,
                                                          scroller = TRUE,
                                                          scrollX = TRUE,
                                                          scrollY = "500px",
                                                          fixedHeader = TRUE,
                                                          buttons = c('copy', 'csv', 'excel', 'pdf'),
                                                          class = 'cell-border stripe',
                                                          fixedColumns = list(
                                                            leftColumns = 3,
                                                            heightMatch = 'none'),
                                                          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                          preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                          drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')   ) )
      if( length( calculos ) > 0 ){
        a <- a %>%
          DT::formatCurrency(metricas_currency, currency = "Q") %>%
          DT::formatPercentage(ncol(datos) - 1, digits = 2 )
      }else{
        a <- a %>%
          DT::formatCurrency(metricas_currency, currency = "Q") 
      }
      tabla_inutil <<- a
    })
    
    
    
  }
  
  # 
  # Función que escribe el reactivo
  
  escribirReactivos <- function(ini = 1,lon){
    
    res <- lapply(c(ini:lon),function(x){
      print(paste("Haciendo el observer para el id", "btFiltroN",x))
      observeEvent(input[[paste0("btFiltroN",x)]], {
        print(paste("En el reactivo de", x))
        filtro <- input[[paste0("btFiltroN",x)]]
        variable <- filaSeleccionada
        hacerTabla(filtro,variable)
        #obtenerArbol()
      }, once = T)
    } )
    return(res)
  }
  
  




 

# OBSERVERS ---------------------------------------------------------------

# Observer que  se encarga de actalizar la información para el tablero de devengado al cambiar el año
#   principal
  
  observe({
    input$year
    devengado$d <- datos_tabla %>%
      summarise(Devengado = sum(Devengado))
    
    if( aplicacion == "gasto" ){
      devengado$p <- datos_tabla %>%
        summarise( Ejecutado = sum(Devengado) / sum(Vigente) * 100 )
    }
  })
  
  # observe que se encarga de escribir los reactivos una vez estos se actualizan
  # Si se implementan los módulos probablemente dejen de ser necesarios
  observe({
    print(numeroReactivos$x)
    print(numeroReactivos$y)
    if( numeroReactivos$x > 0){
      print("Se hacen los reactivos")
      escribirReactivos(numeroReactivos$x+1, numeroReactivos$y)
    }
  })
  
  #Observe que se encarga de cambiar los nombres en la tabla de comparativos
  # de Devengado.x a Devengado año de comparación
  observe({
    datos <- datos_comparativos$data
    if( !is.null(datos) ){
      names(datos) = sub('Devengado.x', paste("Devengado", input$yearCom), names(datos))
      names(datos) = sub('Devengado.y', paste("Devengado", input$year), names(datos))
      comparativo_tabla$data <- datos  
    }
    
  })

  
 
  

  

  
  
  

  
 
  
  

  
  

  
  
  
  
  
  
  
  
  
  


  

  

  

  
  

  
  
  

  
  

  
  
  
  
  
  

  

  



  
  
  
  

  
  


  
  
  
  
  
  # Data Tree implementation ------------------------------------------------

  


# Event Reactive ----------------------------------------------------------

# Event Reactive que se escarga de preparar los datos para el árbol  
  TreeStruct=eventReactive(network$nodes,{
    df=datos_tabla
    if(is.null(network$nodes)){
      df=datos_tabla
    }else{
      
      x.filter=tree.filter(network$nodes,m)
      df=ddply(x.filter,.(ID),function(a.x){m%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  


  

  
  

  

  

  
  

  

  
 

  
  
  
  
})


