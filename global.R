library(shiny)
library(DT)
library(data.table)

data <- read.csv("Mens-Volleyball-PlusLiga-2008-2023.csv")
data$Winner[data$Winner==0] <- 2
data$Winner<- data$Winner - 1

data2 <- cbind(data.frame(do.call('rbind', strsplit(as.character(data$Date),', ',fixed=TRUE))), data[,-1])
colnames(data2) <- c("Date", "Hour", colnames(data2)[3:length(colnames(data2))])
data2$Date <- as.Date(data2$Date, format="%d.%m.%Y")

season22_23 <- data2[data2$Date < "2023-08-01" & data2$Date > "2022-08-01",]

season21_22 <- data2[data2$Date < "2022-08-01" & data2$Date > "2021-08-01",]

season20_21 <- data2[data2$Date < "2021-08-01" & data2$Date > "2020-08-01",]

seasons_to_2019 <- data2[data2$Date < "2020-08-01",]
colnames(seasons_to_2019) <- c("Date", "Hour", "Team_1", "Team_2",
                               "T1_Score", "T2_Score", "T1_Sum",
                               "T1_Srv_Sum", "T1_Srv_Ace", "T1_Srv_Err",
                               "T1_Srv_Ace_per_set", "T1_Rec_Sum",
                               "T1_Rec_Err", "T1_Rec_Neg", "T1_Rec_Po",
                               "T1_Rec_Pos", "T1_Rec_Pe", "T1_Rec_Perf",
                               "T1_Att_Sum", "T1_Att_Err", "T1_Att_Blk",
                               "T1_Att_Kill", "T1_Att_Kill_Perc",
                               "T1_Blk_Sum", "T1_Blk_Sum_per_set", "T2_Sum",
                               "T2_Srv_Sum", "T2_Srv_Ace", "T2_Srv_Err",
                               "T2_Srv_Ace_per_set", "T2_Rec_Sum",
                               "T2_Rec_Err", "T2_Rec_Neg", "T2_Rec_Po",
                               "T2_Rec_Pos", "T2_Rec_Pe", "T2_Rec_Perf",
                               "T2_Att_Sum", "T2_Att_Err", "T2_Att_Blk",
                               "T2_Att_Kill", "T2_Att_Kill_Perc",
                               "T2_Blk_Sum", "T2_Blk_Sum_per_set", "Winner")

season18_19 <- seasons_to_2019[seasons_to_2019$Date < "2019-08-01" & seasons_to_2019$Date > "2018-08-01",]

season17_18 <- seasons_to_2019[seasons_to_2019$Date < "2018-08-01" & seasons_to_2019$Date > "2017-08-01",]

season16_17 <- seasons_to_2019[seasons_to_2019$Date < "2017-08-01" & seasons_to_2019$Date > "2016-08-01",]

season15_16 <- seasons_to_2019[seasons_to_2019$Date < "2016-08-01" & seasons_to_2019$Date > "2015-08-01",]

season14_15 <- seasons_to_2019[seasons_to_2019$Date < "2015-08-01" & seasons_to_2019$Date > "2014-08-01",]

season13_14 <- seasons_to_2019[seasons_to_2019$Date < "2014-08-01" & seasons_to_2019$Date > "2013-08-01",]

season12_13 <- seasons_to_2019[seasons_to_2019$Date < "2013-08-01" & seasons_to_2019$Date > "2012-08-01",]

season11_12 <- seasons_to_2019[seasons_to_2019$Date < "2012-08-01" & seasons_to_2019$Date > "2011-08-01",]

season10_11 <- seasons_to_2019[seasons_to_2019$Date < "2011-08-01" & seasons_to_2019$Date > "2010-08-01",]

season09_10 <- seasons_to_2019[seasons_to_2019$Date < "2010-08-01" & seasons_to_2019$Date > "2009-08-01",]

season08_09 <- seasons_to_2019[seasons_to_2019$Date < "2009-08-01" & seasons_to_2019$Date > "2008-08-01",]

seasons <- list(season08_09, season09_10, season10_11, season11_12,
                season12_13, season13_14, season14_15, season15_16,
                season16_17, season17_18, season18_19)

seasons <- append(seasons, list(season20_21, season21_22, season22_23))

teams <- unique(data2$Team_1)

stats <- c("Wygrane sety", "Zdobyte punkty", "Błędy serwisowe", "Asy serwisowe",
           "Punkty stracone", "Błędy w ataku", "Punkty z ataku",
           "Skuteczność ataków", "Punkty z bloku", "Wygrane", "Porażki")
stats_2 <- c("Wygrane sety", "Zdobyte punkty", "Asy serwisowe",
           "Punkty stracone", "Skuteczność ataków", "Punkty z bloku", "Wygrane")
sezon <- c("2008/9", "2009/10", "2010/11", "2011/12", "2012/13", "2013/14", "2014/15",
           "2015/16", "2016/17", "2017/18", "2018/19", "2020/21", "2021/22", "2022/23")
cho <- list(c("Asy serwisowe"), c("Suma błędów", "Błędy w przyjęciu"))