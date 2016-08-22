#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$selectyear <- renderUI({
    fluidRow(column(4, offset = 1, strong(h2('Beejspottings Season Data/Stats'))), column(1, offset = 5, selectInput('year', 'Year', choices = rev(googlesheets::gs_ws_ls(fantasy_data)), selected = '2016')))
    
  })
  
  
  ff_data <- reactive({
    return(googlesheets::gs_read(fantasy_data, input$year))
  })
  
  schedules <- reactive({
    tmp <- googlesheets::gs_read(fantasy_schedules, input$year)
    tmp[] <- lapply(tmp, as.character)
    return(tmp)
  })
  
  crunched <- reactive({
    return(fantasy_data_cruncher(ff_data(), schedules()))
  })
  
  player_names <- reactive({return(crunched()$Player_Names)
    })
  
  Players <- reactive({
    return(crunched()$Players)
  })
  
  plot_data <- reactive({
    return(crunched()$Plot_Data)
  })
  output$plot_x <- renderUI({
    validate(need(!is.null(plot_data()), 'Please Wait'))
    selectInput('plot_x', 'X', names(plot_data()), "PointsFor")
  })
  output$plot_y <- renderUI({
    validate(need(!is.null(plot_data()), 'Please Wait'))
    selectInput('plot_y', 'Y', names(plot_data()), "WinPct")
  })
  output$plot_color_by <- renderUI({
    validate(need(!is.null(plot_data()), 'Please Wait'))
    selectInput('plot_color_by', 'Color By', c('None', names(plot_data())), selected = "Owner")
  })
  
  league_plot <- reactive({
    validate(need(
      !is.null(input$plot_x) &&
        !is.null(input$plot_y),
      "Select plot variables"
    ))
    p <-
      ggplot(data = plot_data(), aes_string(x = input$plot_x, y = input$plot_y)) + geom_point(size = 8) +
      EJC_theme() +
      theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.line = element_line(size = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_line(size = 1.5)
      )
    
    
    if (!is.null(input$plot_color_by)) {
      if (input$plot_color_by != 'None') {
        p <- p + aes_string(color = input$plot_color_by)
      }
    }
    return(p)
  })
  
  
  
  output$league_plot <- renderPlot(league_plot())
  
  
  
  output$league_summary <-
    renderDataTable({
      round_df(crunched()$League_Summary, 4)
    })
  
  
  
  
  
  ###### Individual Player #########
  
  output$select_player <- renderUI({
    selectInput("player", "Select Player", player_names()[order(player_names())])
  })
  
  pvalue_table <- data.frame(Owner = character(), Pvalue = double())
  
  selected_player <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    return(Players()[[input$player]])
  })
  
  other_players <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    return(Players()[!(names(Players()) %in% input$player)])
  })
  
  Pvalue_for <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    t.test(selected_player()$PointsFor,
           do.call(rbind, other_players())$PointsFor)$p.value
  })
  Pvalue_against <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    t.test(
      selected_player()$PointsAgainst,
      do.call(rbind, other_players())$PointsAgainst
    )$p.value
  })
  
  selected_player_average_for <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    return(mean(selected_player()$PointsFor))
  })
  selected_player_average_against <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    return(mean(selected_player()$PointsAgainst))
  })
  
  other_players_average_for <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    return(mean(do.call(rbind, other_players())$PointsFor))
  })
  
  other_players_average_against <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    return(mean(do.call(rbind, other_players())$PointsAgainst))
  })
  
  output$selected_avg_for <-
    renderText(selected_player_average_for())
  output$other_avg_for <- renderText(other_players_average_for())
  output$selected_avg_against <-
    renderText(selected_player_average_against())
  output$other_avg_against <-
    renderText(other_players_average_against())
  pval_for_text <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    return(paste("(p = ", round(Pvalue_for(), 4), ")", sep = ""))
  })
  output$pvalue_for <- renderText({
    pval_for_text()
  })
  pval_against_text <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    return(paste("(p = ", round(Pvalue_against(), 4), ")", sep = ""))
  })
  output$pvalue_against <- renderText(pval_against_text())
  output$select_win_pct <- renderText({
    selected_player()$WinPct[1]
  })
  output$select_opp_win_pct <-
    renderText({
      selected_player()$OpponentWinPct[1]
    })
  output$select_opp_opp_win_pct <-
    renderText({
      selected_player()$OpponentOpponentWinPct[1]
    })
  output$select_strength_schedule <-
    renderText({
      selected_player()$StrengthSchedule[1]
    })
  output$select_RPI <- renderText({
    selected_player()$RPI[1]
  })
  
  
  selection_histogram_for <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    p <-
      ggplot() + geom_density(
        data =  do.call(rbind, other_players()),
        aes(PointsFor),
        fill = 'black',
        alpha = 0.2
      ) + geom_density(
        data = selected_player(),
        aes(PointsFor),
        fill = 'green3',
        alpha = 0.5
      ) + labs(
        title = paste(
          input$player,
          "(Green) Compared to Rest (Gray): Points For",
          sep = " "
        ),
        x = "Points For"
      ) + theme_classic()
    return(p)
  })
  
  selection_histogram_against <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    p <-
      ggplot() + geom_density(
        data =  do.call(rbind, other_players()),
        aes(PointsAgainst),
        fill = 'black',
        alpha = 0.2
      ) + geom_density(
        data = selected_player(),
        aes(PointsAgainst),
        fill = 'green3',
        alpha = 0.5
      ) + labs(
        title = paste(
          input$player,
          "(Green) Compared to Rest (Gray): Points Against",
          sep = " "
        ),
        x = "Points Against"
      ) + theme_classic()
    return(p)
  })
  
  
  
  
  selection_timecourse <- reactive({
    validate(need(!is.null(input$player), "Select player"))
    p <-
      ggplot(data = selected_player(), aes(x = Week)) + labs(y = "Points") + EJC_theme() +  scale_colour_manual(
        name = 'Data',
        values = c('black' = 'black', 'firebrick3' = 'firebrick3'),
        labels = c('Points For', 'Points Against')
      )
    if (is.null(input$selection_timecourse_plot_y)) {
      return()
    }
    else if (length(input$selection_timecourse_plot_y) == 1) {
      if (input$selection_timecourse_plot_y == 'PointsFor') {
        p <-
          p + geom_point(size = 5, aes(y = PointsFor, color = 'black')) + geom_line(aes(y = PointsFor, color = 'black'))
      }
      else if (input$selection_timecourse_plot_y == 'PointsAgainst') {
        p <-
          p + geom_point(size = 5, aes(y = PointsAgainst, color = 'firebrick3')) + geom_line(aes(y = PointsAgainst, color = 'firebrick3'))
      }
    }
    else if (length(input$selection_timecourse_plot_y) == 2) {
      p <-
        p + geom_point(size = 5, aes(y = PointsFor, color = 'black')) + geom_line(aes(y = PointsFor, color = 'black')) + geom_point(size = 5, aes(y = PointsAgainst, color = 'firebrick3')) + geom_line(aes(y = PointsAgainst, color = 'firebrick3'))
    }
    
    return(p)
  })
  
  output$selection_timecourse <- renderPlot(selection_timecourse())
  
  
  output$selection_histogram_for <-
    renderPlot(selection_histogram_for())
  output$selection_histogram_against <-
    renderPlot(selection_histogram_against())
  output$table <-
    renderDataTable(selected_player()[, seq(1, 4)], options = list(pageLength = 14))
  
  
  output$test <- renderPrint(input$selection_timecourse_plot_y)
  
  
  
})
