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
  output[["rec_error_plot"]] = renderPlot({
    team_matches <- list()
    for (j in 1:14){
      team_matches[[j]] <- seasons[[j]][seasons[[j]]$Team_1 == input[["team_2"]],]
    }

    loses <- sapply(team_matches, function(x) sum(x$Winner==0))
    if (input[["error"]] == "Suma błędów"){
      sum_error <- sapply(team_matches, function(x) sum(as.numeric(gsub(",", ".", x$T1_Srv_Err))+as.numeric(gsub(",", ".", x$T1_Rec_Err))+as.numeric(gsub(",", ".", x$T1_Att_Err))))
      rec_error_percentage <- mapply(function(x,y) x/(0.0000001+nrow(y)), sum_error/200, team_matches)
      ylb = "Średnia liczba błędów popełnionych na mecz"                                                            
    } else{
      rec_error <- sapply(team_matches, function(x) sum(as.numeric(gsub(",", ".", x$T1_Srv_Err))))
      rec_error_percentage <- mapply(function(x,y) x/(0.0000001+nrow(y)), rec_error/17, team_matches)
      ylb = "Średnia liczba błędów w przyjęciu popełnionych na mecz"                               
    }
    
    loses_percentage <- mapply(function(x,y) x/(0.0000001+nrow(y)), loses, team_matches)
    
    rec_error_plot = plot(rec_error_percentage, type='l', ylim=c(0,1), col='purple', lwd = 3,  xlab = "Numer sezonu", ylab = ylb)
    lines(loses_percentage, col='pink', lwd = 3)
  })
  output[["points_plot"]] = renderPlot({
    team_matches <- list()
    for (j in 1:14){
      team_matches[[j]] <- seasons[[j]][seasons[[j]]$Team_1 == input[["team_3"]],]
    }
    
    wins <- sapply(team_matches, function(x) sum(x$Winner==1))
    if (input[["point"]] == "Asy serwisowe"){
      sum_aces <- sapply(team_matches, function(x) sum(x$T1_Srv_Ace))
      points_percentage <- mapply(function(x,y) x/(0.0000001+nrow(y)), sum_aces/80, team_matches)
    }
    
    wins_percentage <- mapply(function(x,y) x/(0.0000001+nrow(y)), wins, team_matches)
    
    points_plot = plot(points_percentage, type='l', ylim=c(0,1), col='purple', lwd = 2)
    lines(wins_percentage, col='pink', lwd = 2)
  })
  output[["final_table"]] = renderDT({
    data.table( ID = c("b","b","b","a","a","c"),  a = 1:6,  b = 7:12,  c = 13:18)
  })
}
