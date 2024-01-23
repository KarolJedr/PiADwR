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
      tabPanel("Zależności",
               fluidRow(
                 column(
                   width = 4,
                   selectInput("team_2",
                               label = "Wybierz drużynę",
                               choices = teams)),
                 column(
                   width = 4,
                   radioButtons("zaleznosc",
                               label = "Wybierz zależność",
                               choices = list("Wygrane" = 1,
                                              "Porażki" = 2))),
                 column(
                   width = 4,
                   radioButtons("error",
                                label = "Wybierz wykres",
                                choices = list("Suma błędów",
                                               "Błędy w przyjęciu")))),
               plotOutput("rec_error_plot")),
      tabPanel("Tabela wyników",
               selectInput("season_2",
                           label = "Wybierz sezon",
                           choices = sezon),
               selectInput("statistic_2",
                           label = "Wybierz statystykę",
                           choices = stats_2),
               DTOutput("score_table")),
      tabPanel("Mecze bezpośrednie",
               fluidRow(
                       column(
                         width = 4,
                         selectInput("first_team",
                                     label = "Wybierz pierwszą drużynę",
                                     choices = teams),
                         selectInput("second_team",
                                     label = "Wybierz drugą drużynę",
                                     choices = teams)),
                       column(
                         width = 4,
                         checkboxGroupInput("seasons_checkbox_1", 
                                            label = "Wybierz sezony", 
                                            choices = list("2008/9" = 1, 
                                                           "2009/10" = 2, 
                                                           "2010/11" = 3,
                                                           "2011/12" = 4, 
                                                           "2012/13" = 5, 
                                                           "2013/14" = 6,
                                                           "2014/15" = 7))),
                       column(
                         width = 4,
                         checkboxGroupInput("seasons_checkbox_2", 
                                            label = "", 
                                            choices = list("2015/16" = 8, 
                                                           "2016/17" = 9,
                                                           "2017/18" = 10, 
                                                           "2018/19" = 11, 
                                                           "2020/21" = 12,
                                                           "2021/22" = 13, 
                                                           "2022/23" = 14)))),
               DTOutput("matches")))
      
)
