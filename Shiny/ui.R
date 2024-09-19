# ui.R
library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("Interacción con ggplot y mtcars"),
  
  plotOutput("plot", 
             hover = hoverOpts("plot_hover"),
             click = clickOpts("plot_click"),
             dblclick = dblclickOpts("plot_dblclick"),
             brush = brushOpts("plot_brush", resetOnNew = TRUE)),
  
  DTOutput("mtcars_table")
)
