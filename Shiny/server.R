library(shiny)
library(ggplot2)
library(DT)

shinyServer(function(input, output, session) {
  
  # Almacenamos los colores iniciales (negro) en un reactiveValues
  vals <- reactiveValues(df = mtcars, color = rep("black", nrow(mtcars)))
  
  # Renderizamos el gráfico con ggplot
  output$plot <- renderPlot({
    ggplot(vals$df, aes(x = wt, y = mpg)) +
      geom_point(aes(color = vals$color), size = 5, alpha = 0.8) +
      theme_minimal() +
      labs(x = "Peso (1000 lbs)", y = "Millas por galón (mpg)", 
           title = "Interacción con los puntos de mtcars") +
      scale_color_identity()
  })
  
  # Hover: Cambia temporalmente el color a gris
  observeEvent(input$plot_hover, {
    hover_index <- nearPoints(vals$df, input$plot_hover, allRows = TRUE)$selected_
    vals$color[hover_index] <- "grey"
    isolate(vals$color[!hover_index] <- "black")  # Restaura los colores no seleccionados
  })
  
  # Click: Cambia el color a verde
  observeEvent(input$plot_click, {
    click_index <- nearPoints(vals$df, input$plot_click, allRows = TRUE)$selected_
    vals$color[click_index] <- "green"
  })
  
  # Doble click: Restablece todos los colores a negro
  observeEvent(input$plot_dblclick, {
    vals$color <- rep("black", nrow(vals$df))
  })
  
  # Brush: Cambia el color de los puntos seleccionados por el brush a azul
  observeEvent(input$plot_brush, {
    brush_index <- brushedPoints(vals$df, input$plot_brush, allRows = TRUE)$selected_
    vals$color[brush_index] <- "blue"
    isolate(vals$color[!brush_index] <- "black")  # Restablece los puntos no seleccionados
  })
  
  # Renderizamos la tabla interactiva con DT
  output$mtcars_table <- renderDT({
    datatable(mtcars, options = list(pageLength = 10, autoWidth = TRUE), 
              rownames = TRUE)
  })
})
