ui = fluidPage(
  tabsetPanel(
      tabPanel("Wygrane",
               selectInput("team",
                           label = "Wybierz drużynę",
                           choices = teams),
               selectInput("statistic",
                           label = "Wybierz statystykę",
                           choices = stats),
               plotOutput("stats_plot"))
    )
)