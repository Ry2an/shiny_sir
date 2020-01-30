###sir shiny####
library(shiny)
library(ggplot2)
drawplot <- function(sus_ini, inf_ini, rec_ini, beta1, beta2) {
  days <- c(1:5000)
  suscept <- rep(0, 5000)
  infect <- rep(0, 5000)
  recover <- rep(0, 5000)
  suscept[1] <- sus_ini*10000
  infect[1] <- inf_ini*10000
  recover[1] <- rec_ini*10000
  beta1 <- beta1/1000000
  beta2 <- beta2/100
  for (i in 2:5000) {
    suscept[i] <- suscept[i - 1] - beta1 * suscept[i - 1] * infect[i - 1]
    infect[i] <- infect[i - 1] + beta1 * suscept[i - 1] * infect[i - 1] - beta2 * infect[i - 1]
    recover[i] <- recover[i - 1] + beta2 * infect[i - 1]
  }
  plotdf <- data.frame(
    rep(days, 3),
    c(suscept, infect, recover),
    c(rep("suscept", 5000), rep("infect", 5000), rep("recover", 5000))
  )
  names(plotdf) <- c("day", "value", "type")
  return(ggplot(plotdf, aes(x = day, y = value, colour = type)) + geom_line())
}

ui <- fluidPage(
  titlePanel("Shiny SIR"),
  sidebarPanel(
    sliderInput("suspect_ini", h3("Initial Suscept Number (10^)"),
                min = 0.1, max = 1, value = 0.454),
    sliderInput("infect_ini", h3("Initial Infected Number (10^)"),
                min = 0.1, max = 1, value = 0.21),
    sliderInput("recover_ini", h3("Initial Recovered Number (10^)"),
                min = 0.1, max = 1, value = 0.25),
    sliderInput("beta1", h3("beta1"),
                min = 0.1, max = 1, value = 0.1),
    sliderInput("beta2", h3("beta2"),
                min = 0.1, max = 1, value = 0.1)
  ),
  mainPanel(
    fluidRow(
      column(width = 5, offset = 7,
             h3("Made by Ruoyan", style = "color:#163883")
      )),
    plotOutput("main_plot")
  )
)

server <- function(input, output){
  output$main_plot <- renderPlot({drawplot(
    sus_ini = input$suspect_ini,
    inf_ini = input$infect_ini,
    rec_ini = input$recover_ini,
    beta1 = input$beta1,
    beta2 <- input$beta2
  )})
}
shinyApp(ui, server)