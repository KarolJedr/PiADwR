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
      y_lim <- c(0, 3)
    } else if (input[["statistic"]] == "Zdobyte punkty"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Srv_Sum))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Srv_Sum))
      y_lim <- c(50, 100)
    } else if (input[["statistic"]] == "Błędy serwisowe"){
      stat_home <- sapply(team_matches_home, function(x) sum(as.integer(x$T1_Srv_Err)))
      stat_away <- sapply(team_matches_away, function(x) sum(as.integer(x$T2_Srv_Err)))
      y_lim <- c(0, 20)
    } else if (input[["statistic"]] == "Asy serwisowe"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Srv_Ace))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Srv_Ace))
      y_lim <- c(0, 10)
    } else if (input[["statistic"]] == "Punkty stracone"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T2_Srv_Sum))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T1_Srv_Sum))
      y_lim <- c(50, 100)
    } else if (input[["statistic"]] == "Skuteczność ataków"){
      stat_home <- sapply(team_matches_home, function(y) sum(as.integer(gsub(pattern="%",x=y$T1_Att_Kill_Perc,replacement="")))/100)
      stat_away <- sapply(team_matches_away, function(y) sum(as.integer(gsub(pattern="%",x=y$T2_Att_Kill_Perc,replacement="")))/100)
      y_lim <- c(0, 1)
    } else if (input[["statistic"]] == "Błędy w ataku"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Att_Err))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Att_Err))
      y_lim <- c(0, 15)
    } else if (input[["statistic"]] == "Punkty z ataku"){
      stat_home <- sapply(team_matches_home, function(x) sum(as.integer(x$T1_Att_Kill)))
      stat_away <- sapply(team_matches_away, function(x) sum(as.integer(x$T2_Att_Kill)))
      y_lim <- c(30, 80)
    } else if (input[["statistic"]] == "Punkty z bloku"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$T1_Blk_Sum))
      stat_away <- sapply(team_matches_away, function(x) sum(x$T2_Blk_Sum))
      y_lim <- c(0, 18)
    } else if (input[["statistic"]] == "Wygrane"){
      stat_home <- sapply(team_matches_home, function(x) sum(x$Winner))
      stat_away <- sapply(team_matches_away, function(x) nrow(x)) - sapply(team_matches_away, function(x) sum(x$Winner))
      y_lim <- c(0, 1)
    } else if (input[["statistic"]] == "Porażki"){
      stat_home <- sapply(team_matches_home, function(x) nrow(x)) - sapply(team_matches_home, function(x) sum(x$Winner))
      stat_away <- sapply(team_matches_away, function(x) sum(x$Winner))
      y_lim <- c(0, 1)
    }
    
    stat_away_mean <- mapply(function(x,y) x/(0.0000001+nrow(y)), stat_away, team_matches_away)
    stat_home_mean <- mapply(function(x,y) x/(0.0000001+nrow(y)), stat_home, team_matches_home)
    
    stats_plot = plot(stat_away_mean, type='l', col='red', ylim=y_lim)
    lines(stat_home_mean, col='green')
  })
  output[["rec_error_plot"]] = renderPlot({
    team_matches_home <- list()
    team_matches_away <- list()
    for (j in 1:14){
      team_matches_home[[j]] <- seasons[[j]][seasons[[j]]$Team_1 == input[["team_2"]],]
      team_matches_away[[j]] <- seasons[[j]][seasons[[j]]$Team_2 == input[["team_2"]],]
    }

    loses <- sapply(team_matches_home, function(x) sum(x$Winner==0)) + sapply(team_matches_away, function(x) sum(x$Winner==1))
    if (input[["error"]] == "Suma błędów"){
      sum_error <- sapply(team_matches_home, function(x) sum(as.numeric(gsub(",", ".", x$T1_Srv_Err))+as.numeric(gsub(",", ".", x$T1_Rec_Err))+as.numeric(gsub(",", ".", x$T1_Att_Err))))+sapply(team_matches_away, function(x) sum(as.numeric(gsub(",", ".", x$T2_Srv_Err))+as.numeric(gsub(",", ".", x$T2_Rec_Err))+as.numeric(gsub(",", ".", x$T2_Att_Err))))
      rec_error_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), sum_error/100, team_matches_home, team_matches_away)
      ylb = "Średnia liczba błędów popełnionych na mecz (1:100)"                                                            
    } else{
      rec_error <- sapply(team_matches_home, function(x) sum(as.numeric(gsub(",", ".", x$T1_Srv_Err)))) + sapply(team_matches_away, function(x) sum(as.numeric(gsub(",", ".", x$T2_Srv_Err))))
      rec_error_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), rec_error/20, team_matches_home, team_matches_away)
      ylb = "Średnia liczba błędów w przyjęciu popełnionych na mecz (1:20)"                               
    }
    
    loses_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), loses, team_matches_home, team_matches_away)
    
    rec_error_plot = plot(rec_error_percentage, type='l', ylim=c(0,1), col='purple', lwd = 3,  xlab = "Numer sezonu", ylab = ylb)
    lines(loses_percentage, col='pink', lwd = 3)
    legend("topleft", legend=c("Średnia liczba błędów", "Średnia liczba przegranych"),
           col=c("purple", "pink"), lwd = 4, cex=0.8)                           
  })
  output[["points_plot"]] = renderPlot({
    team_matches_home <- list()
    team_matches_away <- list()
    for (j in 1:14){
      team_matches_home[[j]] <- seasons[[j]][seasons[[j]]$Team_1 == input[["team_3"]],]
      team_matches_away[[j]] <- seasons[[j]][seasons[[j]]$Team_2 == input[["team_3"]],]
    }
    
    wins <- sapply(team_matches_home, function(x) sum(x$Winner==1)) + sapply(team_matches_away, function(x) sum(x$Winner==0))
    if (input[["point"]] == "Asy serwisowe"){
      sum_aces <- sapply(team_matches_home, function(x) sum(x$T1_Srv_Ace)) + sapply(team_matches_away, function(x) sum(x$T2_Srv_Ace))
      points_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), sum_aces/20, team_matches_home, team_matches_away)
    }
    
    wins_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), wins, team_matches_home, team_matches_away)
    
    points_plot = plot(points_percentage, type='l', ylim=c(0,1), col='purple', lwd = 2)
    lines(wins_percentage, col='pink', lwd = 2)
  })
  output[["final_table"]] = renderDT({
    data.table( ID = c("b","b","b","a","a","c"),  a = 1:6,  b = 7:12,  c = 13:18)
  })
}
