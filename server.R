server = function(input, output) {
  output[["stats_plot"]] = renderPlot({
    team_matches_home <- list()
    team_matches_away <- list()
    for (j in 1:14){
      team_matches_home[[j]] <- seasons[[j]][seasons[[j]]$Team_1 == input[["team"]],]
      team_matches_away[[j]] <- seasons[[j]][seasons[[j]]$Team_2 == input[["team"]],]
    }

    if (input[["statistic"]] == "Wygrane sety"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Score))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Score))
    } else if (input[["statistic"]] == "Zdobyte punkty"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Sum))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Sum))
    } else if (input[["statistic"]] == "Błędy serwisowe"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Srv_Err))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Srv_Err))
    } else if (input[["statistic"]] == "Asy serwisowe"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Srv_Ace))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Srv_Ace))
    } else if (input[["statistic"]] == "Punkty stracone"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Sum - x$T1_Ratio))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Sum - x$T2_Ratio))
    } else if (input[["statistic"]] == "Efektywność zagrywek"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Srv_Eff))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Srv_Eff))
    } else if (input[["statistic"]] == "Błędy w ataku"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Att_Err))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Att_Err))
    } else if (input[["statistic"]] == "Punkty z ataku"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Att_Kill))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Att_Kill))
    } else if (input[["statistic"]] == "Efektywność ataków"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Att_Eff))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Att_Eff))
    } else if (input[["statistic"]] == "Punkty z bloku"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Blk_Sum))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Blk_Sum))
    } else if (input[["statistic"]] == "Wygrane"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$Winner))
      stat_away <- sapply(team_matches_away, function(x) nrow(x)) - sapply(team_matches_away, function(x) sum(x$Winner))
    } else if (input[["statistic"]] == "Porażki"){
      stat_home <- sapply(team_matches_home, function(x) nrow(x)) - sapply(team_matches_home, function(x) sum(x$Winner))
      stat_away <- sapply(team_matches_away, function(x) sum(x$Winner))
    }
    
    stat_away_mean <- mapply(function(x,y) x/(0.0000001+nrow(y)), stat_away, team_matches_away)
    stat_home_mean <- mapply(function(x,y) x/(0.0000001+nrow(y)), stat_home, team_matches_home)

    stats_plot = plot(stat_away_mean, type='l', col='red')
    lines(stat_home_mean, col='green')
  })
}