ui = fluidPage(
  tags$style(".nav-tabs {
  background-color: #9fe0c1;
    }

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: transparent;
border-color: transparent;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #4dbf88;
}"),
  titlePanel("Siatkówka Męska - PlusLiga - 2008-2023"), 
  tabsetPanel(
      tabPanel("Statystyki",
               selectInput("team",
                           label = "Wybierz drużynę",
                           choices = teams),
               selectInput("statistic",
                           label = "Wybierz statystykę",
                           choices = stats),
               plotOutput("stats_plot")),
      tabPanel("Błędy",
               selectInput("team_2",
                           label = "Wybierz drużynę",
                           choices = teams),
               selectInput("error",
                           label = "Wybierz wykres",
                           choices = errors),
               plotOutput("rec_error_plot")),
      tabPanel("Wygrane zależności",
               selectInput("team_3",
                           label = "Wybierz drużynę",
                           choices = teams),
               selectInput("point",
                           label = "Wybierz wykres",
                           choices = points),
               plotOutput("points_plot")),
      tabPanel("Tabela końcowa",
               selectInput("season",
                           label = "Wybierz sezon",
                           choices = sezon),
               DTOutput("final_table")))
      
)
