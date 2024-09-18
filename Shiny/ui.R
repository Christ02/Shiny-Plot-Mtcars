library(shiny)
library(ggplot2)
library(DT)

# interfaz de usuario
shinyUI(
  fluidPage(
    titlePanel("Interacciones avanzadas con ggplot y mtcars"),
    
    # grafico y la tabla en filas separadas
    fluidRow(
      column(12, 
             plotOutput("plot", 
                        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                        click = clickOpts("plot_click"),
                        dblclick = dblclickOpts("plot_dblclick"),
                        brush = brushOpts("plot_brush", resetOnNew = TRUE)
             )
      )
    ),
    
    fluidRow(
      column(12, 
             DTOutput("mtcars_table")
      )
    )
  )
)
