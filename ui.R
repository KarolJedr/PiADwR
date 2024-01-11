ui = fluidPage(
  tabsetPanel(
      tabPanel("Wygrane",
               selectInput("team",
                           label = "Wybierz drużynę",
                           choices = teams),
               plotOutput("wins_plot"))
    )
)