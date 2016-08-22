#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  uiOutput('selectyear'),
  hr(),
  withMathJax(),
  navbarPage(theme = 'bootstrap.css',
             title = "",
    windowTitle = "Beejspottings Season Stats/Data",
    position = 'fixed-bottom',
    tabPanel(
      "Home",
      h3("Description"),
      p(
        "This is all a work in progress. But the idea is that you can take a more nuanced and semi-scientific look at the data from our fantasy football regular season here. Click around, explore, and let me know if you think there would be something useful to look at that I can help put together."
      )
    ),
    
    tabPanel(
      "Global League Analysis",
      h2("Global League Analysis"),
      fluidRow(
        column(
          width = 2,
          uiOutput('plot_x'),
          uiOutput('plot_y'),
          uiOutput('plot_color_by')
        ),
        column(width = 10,
               plotOutput('league_plot'))
      ),
      hr(),
      h2("End-of-Season Data"),
      helpText(
        'League Stats, sorted by RPI. For more information about RPI, ',
        a('see here', href = 'https://en.wikipedia.org/wiki/Rating_Percentage_Index')
      ),
      fluidRow(column(
        width = 10,
        dataTableOutput('league_summary')
      ))
      
      
    ),
    tabPanel(
      "Individual-Based Analysis",
      h2("Individual-Based Analysis"),
      
      fluidRow(
        column(
          width = 5,
          uiOutput("select_player"),
          hr(),
          h2("Retrospective Stats"),
          h4("Win Percentage"),
          helpText("WinPct = (Wins + 0.5*Ties)/(Total Games Played)"),
          textOutput('select_win_pct'),
          h4("Opponent's Win Percentage"),
          textOutput('select_opp_win_pct'),
          h4("Opponent's Opponent's Win Percentage"),
          textOutput('select_opp_opp_win_pct'),
          h4("Computed Strength of Schedule"),
          helpText(
            "Strength = 2/3*(Opponent's WinPct) + 1/3*(Opponent's Opponent's WinPct)"
          ),
          textOutput('select_strength_schedule'),
          h4("RPI (Rating Performance Index)"),
          helpText(
            "RPI = 0.25*(WinPct) + 0.5*(Opponent's WinPct) + 0.25*(Opponent's Opponent's WinPct)"
          ),
          textOutput('select_RPI')
        ),
        column(width = 5,
               dataTableOutput('table'))
      ),
      hr(),
      fluidRow(
        column(
          width = 5,
          h3("Points For"),
          plotOutput('selection_histogram_for'),
          hr(),
          fluidRow(
            column(width = 6,
                   h4("Player Average:"),
                   textOutput('selected_avg_for')),
            column(width = 6,
                   h4("Rest of League:"),
                   textOutput('other_avg_for'))
          ),
          fluidRow(column(
            width = 3,
            offset = 4,
            textOutput('pvalue_for')
          ))
        ),
        column(
          width = 5,
          h3("Points Against"),
          plotOutput('selection_histogram_against'),
          hr(),
          fluidRow(
            column(
              width = 6,
              h4("Player Average:"),
              textOutput('selected_avg_against')
            ),
            column(width = 6,
                   h4("Rest of League:"),
                   textOutput('other_avg_against'))
          ),
          fluidRow(column(
            width = 3,
            offset = 4,
            textOutput('pvalue_against')
          ))
        )
      ),
      
      hr(),
      fluidRow(
        h3("Season View"),
        column(
          width = 2,
          checkboxGroupInput(
            "selection_timecourse_plot_y",
            "Select Data to Plot:",
            c("Points For" = "PointsFor", "Points Against" = "PointsAgainst"),
            selected = "PointsFor"
          )
        ),
        column(width = 10,
               plotOutput('selection_timecourse'))
      )
      
      
      
      
    )
  )
  
  
  
))
