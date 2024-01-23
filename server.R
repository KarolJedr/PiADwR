server = function(input, output, session){
  observeEvent(input$zaleznosc,
               {
                 updateRadioButtons(session, input = "error", choices = cho[[as.integer(input$zaleznosc)]])
               })
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
    
    stats_plot = plot(stat_away_mean, type='l', col='red', ylim=y_lim, lwd = 3,  xlab = "Numer sezonu", ylab = input[["statistic"]])
    lines(stat_home_mean, col='green', lwd = 3)
    legend("topleft", legend=c("Mecze u siebie", "Mecze na wyjeździe"),
           col=c("green", "red"), lwd = 4, cex=0.8)
  })
  output[["rec_error_plot"]] = renderPlot({
    team_matches_home <- list()
    team_matches_away <- list()
    for (j in 1:14){
      team_matches_home[[j]] <- seasons[[j]][seasons[[j]]$Team_1 == input[["team_2"]],]
      team_matches_away[[j]] <- seasons[[j]][seasons[[j]]$Team_2 == input[["team_2"]],]
    }
    
    if (input[["zaleznosc"]] == 1){
      wins <- sapply(team_matches_home, function(x) sum(x$Winner==1)) + sapply(team_matches_away, function(x) sum(x$Winner==0))
      sum_aces <- sapply(team_matches_home, function(x) sum(x$T1_Srv_Ace)) + sapply(team_matches_away, function(x) sum(x$T2_Srv_Ace))
      stat_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), sum_aces/20, team_matches_home, team_matches_away)
      score_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), wins, team_matches_home, team_matches_away)
      leg <- c("Średnia liczba asów serwisowych", "Średnia liczba wygranych")
      ylb = "Średnia liczba asów serwisowych na mecz (1:20)"
    } else{
      loses <- sapply(team_matches_home, function(x) sum(x$Winner==0)) + sapply(team_matches_away, function(x) sum(x$Winner==1))
      if (input[["error"]] == "Suma błędów"){
        sum_error <- sapply(team_matches_home, function(x) sum(as.numeric(gsub(",", ".", x$T1_Srv_Err))+as.numeric(gsub(",", ".", x$T1_Rec_Err))+as.numeric(gsub(",", ".", x$T1_Att_Err))))+sapply(team_matches_away, function(x) sum(as.numeric(gsub(",", ".", x$T2_Srv_Err))+as.numeric(gsub(",", ".", x$T2_Rec_Err))+as.numeric(gsub(",", ".", x$T2_Att_Err))))
        stat_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), sum_error/40, team_matches_home, team_matches_away)
        ylb = "Średnia liczba błędów popełnionych na mecz (1:40)"                                                            
      } else{
        rec_error <- sapply(team_matches_home, function(x) sum(as.numeric(gsub(",", ".", x$T1_Srv_Err)))) + sapply(team_matches_away, function(x) sum(as.numeric(gsub(",", ".", x$T2_Srv_Err))))
        stat_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), rec_error/40, team_matches_home, team_matches_away)
        ylb = "Średnia liczba błędów w przyjęciu popełnionych na mecz (1:40)"                               
      }
      score_percentage <- mapply(function(x,y,z) x/(0.0000001+nrow(y)+nrow(z)), loses, team_matches_home, team_matches_away)
      leg <- c("Średnia liczba błędów", "Średnia liczba przegranych")
    }
    
    rec_error_plot = plot(stat_percentage, type='l', ylim=c(0,1), col='purple', lwd = 3,  xlab = "Numer sezonu", ylab = ylb)
    lines(score_percentage, col='pink', lwd = 3)
    legend("topleft", legend=leg,
           col=c("purple", "pink"), lwd = 4, cex=0.8)
  })
  
  output[["score_table"]] = renderDT({
    season_index <- which(sezon == input[["season_2"]])

    stat_sums <- c()
    if (input[["statistic_2"]] == "Skuteczność ataków"){
      for (tm in teams){
        stat_sum <- sum(as.integer(gsub(pattern="%",x=seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,]$T1_Att_Kill_Perc,replacement="")))/100
        stat_sum <- stat_sum + sum(as.integer(gsub(pattern="%",x=seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,]$T2_Att_Kill_Perc,replacement="")))/100
        stat_sums <- c(stat_sums, stat_sum/(nrow(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,])+nrow(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,])))
      }
    } else if (input[["statistic_2"]] == "Wygrane sety"){
      for (tm in teams){
        stat_sum <- sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,]$T1_Score))
        stat_sum <- stat_sum + sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,]$T2_Score))
        stat_sums <- c(stat_sums, stat_sum/(nrow(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,])+nrow(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,])))
      }
    } else if (input[["statistic_2"]] == "Zdobyte punkty"){
      for (tm in teams){
        stat_sum <- sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,]$T1_Srv_Sum))
        stat_sum <- stat_sum + sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,]$T2_Srv_Sum))
        stat_sums <- c(stat_sums, stat_sum/(nrow(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,])+nrow(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,])))
      }
    } else if (input[["statistic_2"]] == "Asy serwisowe"){
      for (tm in teams){
        stat_sum <- sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,]$T1_Srv_Ace))
        stat_sum <- stat_sum + sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,]$T2_Srv_Ace))
        stat_sums <- c(stat_sums, stat_sum/(nrow(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,])+nrow(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,])))
      }
    } else if (input[["statistic_2"]] == "Punkty z bloku"){
      for (tm in teams){
        stat_sum <- sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,]$T1_Blk_Sum))
        stat_sum <- stat_sum + sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,]$T2_Blk_Sum))
        stat_sums <- c(stat_sums, stat_sum/(nrow(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,])+nrow(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,])))
      }
    } else if (input[["statistic_2"]] == "Wygrane"){
      for (tm in teams){
        stat_sum <- sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,]$Winner))
        stat_sum <- stat_sum - sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,]$Winner)) + nrow(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,])
        stat_sums <- c(stat_sums, stat_sum/(nrow(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,])+nrow(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,])))
      }
    } else if (input[["statistic_2"]] == "Punkty stracone"){
      for (tm in teams){
        stat_sum <- sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,]$T2_Srv_Sum))
        stat_sum <- stat_sum + sum(as.integer(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,]$T1_Srv_Sum))
        stat_sums <- c(stat_sums, stat_sum/(nrow(seasons[[season_index]][seasons[[season_index]]$Team_1 == tm,])+nrow(seasons[[season_index]][seasons[[season_index]]$Team_2 == tm,])))
      }
    }
    score_table <- data.table(Drużyna = teams, Średnia = round(stat_sums, 2))
    setorder(score_table[!is.na(score_table$Średnia)], cols = - "Średnia")
  })
  
  output[["matches"]] = renderDT({
    selected_seasons <- c(input[["seasons_checkbox_1"]], input[["seasons_checkbox_2"]])
    all_matches <- data.table(rbindlist(seasons[as.integer(selected_seasons)]))
    selected_matches <- rbind(all_matches[all_matches$Team_1 == input[["first_team"]] & all_matches$Team_2 == input[["second_team"]],],
                             all_matches[all_matches$Team_2 == input[["first_team"]] & all_matches$Team_1 == input[["second_team"]],])
    if (nrow(selected_matches) > 0){
      matches <- selected_matches[, c("Date", "Hour", "Team_1", "Team_2", "T1_Score", "T2_Score")]
    } else{
      matches <- data.table()
    }
    
    
  })
}