require(shiny)
require(shinydashboard)
require(googlesheets)
require(tidyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(BiomassWorkflow)
options(digits = 4)

## Google Hookup
googlesheets::gs_auth(token = "googletoken.rds")
fantasy_data <- googlesheets::gs_title('FantasyData')
fantasy_schedules <- googlesheets::gs_title('FantasySchedules')

## Custom Functions
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[, nums] <- round(df[, nums], digits = digits)
  
  (df)
}

table_reorder_first <- function(table, value) {
  cols <- c(value, names(table[-which(names(table) ==
                                        value)]))
  output_table <- data.frame(table[cols], row.names = NULL)
  return(output_table)
}

matchup_maker <- function(player_list) {
  matchups <- list()
  for (i in 1:length(player_list)) {
    for (j in 1:length(player_list)) {
      if (player_list[[i]]$Owner[1] != player_list[[j]]$Owner[1]) {
        matchup <-
          paste(player_list[[i]]$Owner[1],
                "vs",
                player_list[[j]]$Owner[1],
                "opponents",
                sep = "_")
        
        differences <-
          player_list[[i]]$PointsFor - player_list[[j]]$PointsAgainst
        
        output <- data.frame(differences)
        colnames(output) <- matchup
        
        matchups <- c(matchups, output)
      }
    }
  }
  return(matchups)
}

matchup_scrambler <- function(matchups) {
  matchup_scramble <-
    data.frame(
      Owner = character(),
      Matchup = character(),
      Wins = double(),
      Losses = double(),
      Ties = double(),
      WinPct = double()
    )
  
  for (matchup in names(matchups)) {
    wins <- sum(matchups[[matchup]] > 0, na.rm = T)
    losses <- sum(matchups[[matchup]] < 0, na.rm = T)
    ties <- sum(matchups[[matchup]] == 0, na.rm = T)
    
    winpct <- (wins + 0.5 * ties) / (wins + losses + ties)
    
    winpct_output <-
      data.frame(
        Owner = strsplit(matchup, "_")[[1]][1],
        Matchup = matchup,
        Wins = wins,
        Losses = losses,
        Ties = ties,
        WinPct = winpct
      )
    
    matchup_scramble <- rbind(matchup_scramble, winpct_output)
    
    
  }
  output <- matchup_scramble
  return(output)
}

schedule_comparison <-
  function(matchup_scramble, player_list, ff_data) {
    schedule_compare <-
      group_by(matchup_scramble, Owner) %>% summarize(AvgScrambledWinPct = mean(WinPct))
    schedule_compare <-
      full_join(schedule_compare, ff_data[, c("Owner", "WinPct")])
    colnames(schedule_compare) <-
      c("Owner", "ScrambledPct", "ActualPct")
    schedule_compare$ScramDifferential <-
      schedule_compare$ActualPct - schedule_compare$ScrambledPct
    schedule_compare <-
      schedule_compare %>% select(Owner, ActualPct, ScrambledPct, ScramDifferential)
    
    pythag_winpct <-
      data.frame(Owner = character(), PWinPct = double())
    for (i in 1:length(player_list)) {
      PF <- sum(player_list[[i]]$PointsFor, na.rm = T)
      PA <- sum(player_list[[i]]$PointsAgainst, na.rm = T)
      ppct <-
        data.frame(Owner = player_list[[i]]$Owner[1],
                   PWinPct = (PF ^ 2.37) / (PF ^ 2.37 + PA ^ 2.37))
      pythag_winpct <- rbind(pythag_winpct, ppct)
    }
    
    schedule_compare <-
      left_join(schedule_compare, pythag_winpct, by = "Owner")
    schedule_compare$PythDifferential <-
      schedule_compare$ActualPct - schedule_compare$PWinPct
    
    
    return(schedule_compare)
  }

correlator <- function(player_list, raw_data) {
  correlations <-
    data.frame(Owner = character(), Correlation = double())
  for (i in 1:length(player_list)) {
    correlation <-
      data.frame(
        Owner = player_list[[i]]$Owner[1],
        Correlation = cor.test(player_list[[i]]$PointsFor, seq(1, 14))$estimate
      )
    
    correlations <- rbind(correlations, correlation)
    
  }
  
  correlation_metadata <- full_join(correlations, raw_data[, 1:13])
  
  return(correlation_metadata)
}


fantasy_data_cruncher <- function(ff_data, schedule) {
  
  pointsFor_tmp <-
    as.data.frame(dplyr::select(ff_data, Owner, ends_with("For")))
  colnames(pointsFor_tmp) <- c("Owner", as.character(seq(1, 14)))
  
  ## Points For
  pointsFor <-
    as.data.frame(
      reshape2::melt(
        data = pointsFor_tmp,
        id.vars = 'Owner',
        variable.name = 'Week',
        value.name = 'PointsFor'
      )
    )
  colnames(pointsFor) <- c("Owner", "Week", "PointsFor")
  
  
  pointsAgainst_tmp <-
    as.data.frame(dplyr::select(ff_data, Owner, ends_with("Against")))
  colnames(pointsAgainst_tmp) <- c("Owner", as.character(seq(1, 14)))
  
  ## Points Against
  pointsAgainst <-
    as.data.frame(
      reshape2::melt(
        data = pointsAgainst_tmp,
        id.vars = 'Owner',
        variable.name = 'Week',
        value.name = 'PointsAgainst'
      )
    )
  colnames(pointsAgainst) <- c("Owner", "Week", "PointsAgainst")
  
  ## Combined, all points
  points_all <-
    dplyr::inner_join(pointsFor, pointsAgainst, by = c("Owner", "Week"))
  points_all$Week <- as.numeric(points_all$Week)
  
  # Player Names
  player_names <- ff_data$Owner
  
  # Player list
  Players <- list()
  for (name in player_names) {
    Players <- c(Players, list(dplyr::full_join(dplyr::filter(points_all, Owner == name), dplyr::filter(ff_data, Owner == name)[,c('Owner', 'TeamName', 'WinPct')], by = "Owner")))
  }
  
  names(Players) <- player_names
  
  # matchups
  matchups <- matchup_maker(Players)
  
  for (name in player_names) {
    sched <- schedule[[name]]
    opponent_winpct = 0
    opponent_opponent_winpct = 0
    for (opp in sched) {
      opponent_winpct = opponent_winpct + Players[[opp]]$WinPct[1]/(length(sched))
      for (opp_opp in schedule[[opp]]) {
        opponent_opponent_winpct = opponent_opponent_winpct + Players[[opp_opp]]$WinPct[1]/(length(sched)^2)
      }
    }
    Players[[name]]$OpponentWinPct <- opponent_winpct
    Players[[name]]$OpponentOpponentWinPct <- opponent_opponent_winpct
    Players[[name]]$StrengthSchedule <- (2*Players[[name]]$OpponentWinPct + Players[[name]]$OpponentOpponentWinPct)/3
    Players[[name]]$RPI <- 0.25*Players[[name]]$WinPct + 0.5*Players[[name]]$OpponentWinPct + 0.25*Players[[name]]$OpponentOpponentWinPct
  }
  
  strength_summary <- data.frame(Owner = character(), Team = character(), WinPct = double(), OpponentWinPct = double(), OpponentOpponentWinPct = double(), StrengthSchedule = double(), RPI = double())
  
  for (name in player_names) {
    strength_summary <- rbind(strength_summary, Players[[name]][1, c(1,seq(5,10))])
  }
  
  # Plot Data
  plot_data <- dplyr::left_join(ff_data[, 1:14], strength_summary)
  
  # League Data
  league_summary_table <- dplyr::arrange(dplyr::full_join(strength_summary, ff_data[,1:14]), desc(RPI))
  
  league_summary_table_final <- dplyr::select(league_summary_table, RPI, TeamName, Owner, Division, Wins, Losses, Ties, WinPct, StrengthSchedule, PointsForTotal, PointsAgainstTotal, Trades, Acquisitions, Drops, Activations, IR, OpponentWinPct)
  
  output <- list('Plot_Data' = plot_data, 'Players' = Players, 'League_Summary' = league_summary_table_final, 'Player_Names' = player_names)
  
  return(output)
  
}
