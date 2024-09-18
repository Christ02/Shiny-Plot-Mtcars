library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("Interacción con ggplot y mtcars"),
  
  # Gráfico interactivo
  plotOutput("plot", 
             hover = hoverOpts("plot_hover"),
             click = clickOpts("plot_click"),
             dblclick = dblclickOpts("plot_dblclick"),
             brush = brushOpts("plot_brush", resetOnNew = TRUE)),
  
  # Tabla interactiva
  DTOutput("mtcars_table")
)

server <- function(input, output, session) {
  # Estado inicial: todos los puntos sin color
  vals <- reactiveValues(df = mtcars, color = rep("black", nrow(mtcars)))
  
  # Grafico ggplot
  output$plot <- renderPlot({
    ggplot(vals$df, aes(x = wt, y = mpg)) +
      geom_point(aes(color = vals$color), size = 4) +
      theme_minimal() +
      scale_color_identity()
  })
  
  # On hover cambie el color a gris
  observeEvent(input$plot_hover, {
    hover_index <- nearPoints(vals$df, input$plot_hover, allRows = TRUE)$selected_
    vals$color[hover_index] <- "grey"
    isolate(vals$color[!hover_index] <- "black")
  })
  
  # On click cambie el color a verde
  observeEvent(input$plot_click, {
    click_index <- nearPoints(vals$df, input$plot_click, allRows = TRUE)$selected_
    vals$color[click_index] <- "green"
  })
  
  # On doble click quite el color
  observeEvent(input$plot_dblclick, {
    vals$color <- rep("white", nrow(vals$df))
  })
  
  # On brush cambie el color a los puntos que están dentro del rectángulo.
  observeEvent(input$plot_brush, {
    brush_index <- brushedPoints(vals$df, input$plot_brush, allRows = TRUE)$selected_
    vals$color[brush_index] <- "blue"
    isolate(vals$color[!brush_index] <- "black")
  })
  
  output$mtcars_table <- renderDT({
    datatable(mtcars)
  })
}

# Ejecutar la app Shiny
shinyApp(ui = ui, server = server)

