require(tidyr)
require(dplyr)
require(ggplot2)



ff_data <- read.csv('FFStats.csv')

pointsFor <- select(ff_data, Owner, ends_with("F"))
colnames(pointsFor) <- c("Owner", as.character(seq(1, 14)))

pointsFor <- pointsFor %>% gather(Week, Owner)
colnames(pointsFor) <- c("Owner", "Week", "PointsFor")


pointsAgainst <- select(ff_data, Owner, ends_with("A"))
colnames(pointsAgainst) <- c("Owner", as.character(seq(1, 14)))

pointsAgainst <- pointsAgainst %>% gather(Week, Owner)
colnames(pointsAgainst) <- c("Owner", "Week", "PointsAgainst")


points <- inner_join(pointsFor, pointsAgainst, by = c("Owner" = "Owner", "Week" = "Week"))
points$Week <- as.numeric(points$Week)

names <- as.character(ff_data$Owner)
Player <- list()
for (name in names) {
  Player <- c(Player, list(filter(points, Owner == name)))
}
names(Player) <- names


matchups <- list()
for (i in 1:length(Player)){
  for (j in 1:length(Player)){
    
    if (Player[[i]]$Owner[1] != Player[[j]]$Owner[1]) {
    matchup <- paste(Player[[i]]$Owner[1], "vs", Player[[j]]$Owner[1], "opponents", sep = "_")
    
    differences <- Player[[i]]$PointsFor - Player[[j]]$PointsAgainst
    
    output <- data.frame(differences)
    colnames(output) <- matchup
    
    matchups <- c(matchups, output)
    
    
    }
  }
}

matchup_scramble <- data.frame(Owner = character(), Matchup = character(), Wins = double(), Losses = double(), Ties = double(), WinPct= double())

for (matchup in names(matchups)) {
  wins <- sum(matchups[[matchup]] > 0, na.rm = T)
  losses <- sum(matchups[[matchup]] < 0, na.rm = T)
  ties <- sum(matchups[[matchup]] == 0, na.rm = T)
  
  winpct <- (wins + 0.5*ties)/(wins + losses + ties)
  
  winpct_output <- data.frame(Owner = strsplit(matchup, "_")[[1]][1], Matchup = matchup, Wins = wins, Losses = losses, Ties = ties, WinPct = winpct)
  
  matchup_scramble <- rbind(matchup_scramble, winpct_output)
  
  
}

schedule_compare <- group_by(matchup_scramble, Owner) %>% summarize(AvgScrambledWinPct = mean(WinPct))
schedule_compare <- full_join(schedule_compare, ff_data[, c("Owner", "WinPct")])
colnames(schedule_compare) <- c("Owner", "ScrambledPct", "ActualPct")
schedule_compare$ScramDifferential <- schedule_compare$ActualPct - schedule_compare$ScrambledPct
schedule_compare <- schedule_compare %>% select(Owner, ActualPct, ScrambledPct, ScramDifferential)



correlations <- data.frame(Owner = character(), Correlation = double())
for (i in 1:length(Player)){
  
  correlation <- data.frame( Owner = Player[[i]]$Owner[1], Correlation = cor.test(Player[[i]]$PointsFor, seq(1,14))$estimate)
  
  correlations <- rbind(correlations, correlation)  
  
}

correlation_metadata <- full_join(correlations, ff_data[, 1:13])

pythag_winpct <- data.frame(Owner = character(), PWinPct = double())
for ( i in 1:length(Player)){
  PF <- sum(Player[[i]]$PointsFor, na.rm = T)
  PA <- sum(Player[[i]]$PointsAgainst, na.rm = T)
  ppct <- data.frame(Owner = Player[[i]]$Owner[1], PWinPct = (PF^2.37)/(PF^2.37 + PA^2.37))
  pythag_winpct <- rbind(pythag_winpct, ppct)
}

schedule_compare <- left_join(schedule_compare, pythag_winpct, by = "Owner")
schedule_compare$PythDifferential <- schedule_compare$ActualPct - schedule_compare$PWinPct

pvalue_table <- data.frame(Owner = character(), Pvalue = double())
for (i in 1:length(Player)){
  Owner <- Player[[i]]$Owner[1]
  Pvalue <- t.test(Player[[i]]$PointsFor, do.call(rbind, Player[-i])$PointsFor)$p.value
  pval <- data.frame(Owner = Owner, Pvalue = Pvalue)
  pvalue_table <- rbind(pvalue_table, pval)
  p <- ggplot() + geom_density(data =  do.call(rbind, Player[-i]), aes(PointsFor), fill = 'blue', alpha = 0.2) + geom_density(data = Player[[i]], aes(PointsFor), fill = 'red', alpha = 0.2) + labs(title = paste(Owner, "(Red) vs Rest (Blue)", sep = " "), x = "Points") + theme_classic()
  print(p)
}


