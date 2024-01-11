server = function(input, output) {
  output[["wins_plot"]] = renderPlot({
    team_matches_home <- list()
    team_matches_away <- list()
    for (j in 1:14){
      team_matches_home[[j]] <- seasons[[j]][seasons[[j]]$Team_1 == input[["team"]],]
      team_matches_away[[j]] <- seasons[[j]][seasons[[j]]$Team_2 == input[["team"]],]
    }

    wins_home <- sapply(team_matches_home, function(x) sum(x$Winner))
    loses_home <- sapply(team_matches_home, function(x) nrow(x)) - wins_home
    loses_away <- sapply(team_matches_away, function(x) sum(x$Winner))
    wins_away <- sapply(team_matches_away, function(x) nrow(x)) - loses_away
    
    wins_away_percentage <- mapply(function(x,y) x/(0.0000001+nrow(y)), loses_away, team_matches_away)
    loses_away_percentage <- mapply(function(x,y) x/(0.0000001+nrow(y)), wins_away, team_matches_away)
    wins_home_percentage <- mapply(function(x,y) x/(0.0000001+nrow(y)), loses_home, team_matches_home)
    loses_home_percentage <- mapply(function(x,y) x/(0.0000001+nrow(y)), wins_home, team_matches_home)
    
    wins_plot = plot(wins_away_percentage, type='l', ylim=c(0,1), col='red')
    lines(wins_home_percentage, col='green')
  })
}