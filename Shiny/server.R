# server.R
library(shiny)
library(ggplot2)
library(DT)

server <- function(input, output, session) {
  # Estado inicial: todos los puntos sin color
  vals <- reactiveValues(
    df = mtcars,
    color = rep("black", nrow(mtcars)),
    selected_brush = rep(FALSE, nrow(mtcars)),
    selected_click = rep(FALSE, nrow(mtcars)),
    original_color = rep("black", nrow(mtcars))  # Guardar colores originales
  )
  
  # Variable reactiva para mantener los puntos actualmente en hover
  hovered_points <- reactiveVal(rep(FALSE, nrow(mtcars)))  # Usamos nrow(mtcars) directamente
  
  # Gráfico ggplot
  output$plot <- renderPlot({
    ggplot(vals$df, aes(x = wt, y = mpg)) +
      geom_point(aes(color = vals$color), size = 4) +
      theme_minimal() +
      scale_color_identity()
  })
  
  # Hover: Cambiar color a gris temporalmente y restaurar cuando se deja de hacer hover
  observeEvent(input$plot_hover, {
    if (is.null(input$plot_hover)) {
      # Restaurar colores de los puntos previamente en hover
      previamente_hovereados <- hovered_points()
      vals$color[previamente_hovereados] <- vals$original_color[previamente_hovereados]
      hovered_points(rep(FALSE, nrow(vals$df)))
    } else {
      hover_index <- nearPoints(vals$df, input$plot_hover, allRows = TRUE)$selected_
      
      # Restaurar colores de los puntos previamente en hover
      previamente_hovereados <- hovered_points()
      vals$color[previamente_hovereados] <- vals$original_color[previamente_hovereados]
      
      # Guardar los colores originales de los nuevos puntos en hover
      vals$original_color[hover_index] <- vals$color[hover_index]
      
      # Cambiar el color de los puntos en hover a gris
      vals$color[hover_index] <- "grey"
      
      # Actualizar hovered_points
      hovered_points(hover_index)
    }
  })
  
  # Click: Cambiar color a verde y mantenerlo hasta que se haga doble click
  observeEvent(input$plot_click, {
    click_index <- nearPoints(vals$df, input$plot_click, allRows = TRUE)$selected_
    vals$selected_click[click_index] <- !vals$selected_click[click_index]
    
    # Actualizar colores y colores originales
    vals$color[vals$selected_click] <- "green"
    vals$original_color[vals$selected_click] <- "green"
    
    # Restablecer color para puntos no seleccionados
    isolate({
      no_seleccionados <- !vals$selected_brush & !vals$selected_click
      vals$color[no_seleccionados] <- "black"
      vals$original_color[no_seleccionados] <- "black"
    })
  })
  
  # Doble click: Quitar color solo del punto seleccionado (volver al estado inicial para ese punto)
  observeEvent(input$plot_dblclick, {
    dblclick_index <- nearPoints(vals$df, input$plot_dblclick, allRows = TRUE)$selected_
    vals$selected_brush[dblclick_index] <- FALSE
    vals$selected_click[dblclick_index] <- FALSE
    vals$color[dblclick_index] <- "black"
    vals$original_color[dblclick_index] <- "black"
  })
  
  # Brush: Cambiar color de los puntos seleccionados a azul y mantenerlo (selecciones acumulativas)
  observeEvent(input$plot_brush, {
    if (!is.null(input$plot_brush)) {
      brush_index <- brushedPoints(vals$df, input$plot_brush, allRows = TRUE)$selected_
      
      # Actualizar la selección acumulativamente
      vals$selected_brush <- vals$selected_brush | brush_index
      
      # Actualizar colores y colores originales
      vals$color[vals$selected_brush] <- "blue"
      vals$original_color[vals$selected_brush] <- "blue"
      
      # Restablecer color para puntos no seleccionados
      isolate({
        no_seleccionados <- !vals$selected_brush & !vals$selected_click
        vals$color[no_seleccionados] <- "black"
        vals$original_color[no_seleccionados] <- "black"
      })
    }
    # No reseteamos la selección al terminar el brush
  })
  
  # Mostrar tabla filtrada con los puntos seleccionados
  output$mtcars_table <- renderDT({
    selected <- vals$selected_click | vals$selected_brush  # Filtra los seleccionados por click o brush
    datatable(vals$df[selected, , drop = FALSE])  # Muestra solo las filas seleccionadas
  })
}
