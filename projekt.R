#setwd("C:/Studia/magisterka/3_semestr/PiADwR/Projekt")

data <- read.csv("Mens-Volleyball-PlusLiga-2008-2023.csv")

data2 <- cbind(data.frame(do.call('rbind', strsplit(as.character(data$Date),', ',fixed=TRUE))), data[,-1])
colnames(data2) <- c("Date", "Hour", colnames(data2)[3:length(colnames(data2))])
data2$Date <- as.Date(data2$Date, format="%d.%m.%Y")

season22_23 <- data2[data2$Date < "2023-08-01" & data2$Date > "2022-08-01",]

season21_22 <- data2[data2$Date < "2022-08-01" & data2$Date > "2021-08-01",]

season20_21 <- data2[data2$Date < "2021-08-01" & data2$Date > "2020-08-01",]

#season19_20 <- data2[data2$Date < "2020-08-01" & data2$Date > "2019-08-01",]

season18_19 <- data2[data2$Date < "2019-08-01" & data2$Date > "2018-08-01",]

season17_18 <- data2[data2$Date < "2018-08-01" & data2$Date > "2017-08-01",]

season16_17 <- data2[data2$Date < "2017-08-01" & data2$Date > "2016-08-01",]

season15_16 <- data2[data2$Date < "2016-08-01" & data2$Date > "2015-08-01",]

season14_15 <- data2[data2$Date < "2015-08-01" & data2$Date > "2014-08-01",]

season13_14 <- data2[data2$Date < "2014-08-01" & data2$Date > "2013-08-01",]

season12_13 <- data2[data2$Date < "2013-08-01" & data2$Date > "2012-08-01",]

season11_12 <- data2[data2$Date < "2012-08-01" & data2$Date > "2011-08-01",]

season10_11 <- data2[data2$Date < "2011-08-01" & data2$Date > "2010-08-01",]

season09_10 <- data2[data2$Date < "2010-08-01" & data2$Date > "2009-08-01",]

season08_09 <- data2[data2$Date < "2009-08-01" & data2$Date > "2008-08-01",]

seasons <- list(season08_09, season09_10, season10_11, season11_12,
                season12_13, season13_14, season14_15, season15_16,
                season16_17, season17_18, season18_19,
                season20_21, season21_22, season22_23)


teams <- unique(data2$Team_1)

#Wybierzmy drużynę i
i <- 16

team_matches_home <- list()
team_matches_away <- list()
for (j in 1:14){
  team_matches_home[[j]] <- seasons[[j]][seasons[[j]]$Team_1 == teams[i],]
  team_matches_away[[j]] <- seasons[[j]][seasons[[j]]$Team_2 == teams[i],]
}

wins_home <- c()
loses_home <- c()
for (j in 1:14){
  wins_home[j] <- sum(team_matches_home[[j]]$Winner)
  loses_home[j] <- nrow(team_matches_home[[j]]) - wins_home[j]
}

wins_away <- c()
loses_away <- c()
for (j in 1:14){
  loses_away[j] <- sum(team_matches_away[[j]]$Winner)
  wins_away[j] <- nrow(team_matches_away[[j]]) - loses_away[j]
}

x<-c()
y <-c()
for (j in 1:14){
  x <- c(x,(loses_away[j]/(0.0000001+nrow(team_matches_away[[j]]))))
  y <- c(y,(wins_away[j]/(0.0000001+nrow(team_matches_away[[j]]))))
}
plot(x, type='l', ylim=c(0,1), col='red')
lines(y, col='green')