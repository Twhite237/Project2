library(shiny)
library(shinyalert)
library(tidyverse)
library(bslib)
library(gt)




#nflData = read.csv(file = "data/nfldata.csv")
# Remove any cols with prob:
nflDataClean <- readRDS("data/nflDataClean.rds")

nflDataClean = nflDataClean |> select(-contains("prob"))

## Select Desired Columns:
nflDataClean = nflDataClean |> select(
  Date,
  qtr,
  posteam,
  ydstogo,
  PassOutcome,
  YardsAfterCatch,
  InterceptionThrown,
  GameID,
  TimeSecs,
  yrdln,
  ydsnet,
  Touchdown,
  PassLength,
  AbsScoreDiff,
  Season,
  Drive,
  time,
  yrdline100,
  Yards.Gained,
  PlayType,
  PassAttempt,
  HomeTeam,
  AwayTeam,
  PosTeamScore,
  DefTeamScore
) |>
  filter(posteam != "")

factors = c("qtr",
            "posteam",
            "PassOutcome",
            "HomeTeam",
            "AwayTeam",
            "PlayType",
            "GameID",
            "Touchdown",
            "PassAttempt"
)


nflDataClean[factors] = lapply(nflDataClean[factors], as.factor)



ui <- page_sidebar(
  title = "Team Comparison Tool",
  sidebar = sidebar(title = "Chose your teams here",
                    
                    checkboxInput(inputId = "headToHeadCheck", label = "Check to Compare Head-To-Head"),
                    conditionalPanel(
                      condition = "input.headToHeadCheck == true",
                      selectizeInput(
                        "team1Sel",
                        "Team 1:",
                        (levels(nflDataClean$HomeTeam)),
                        "NYG"
                      ),
                      selectizeInput(
                        "team2Sel",
                        "Team 2:",
                        (levels(nflDataClean$HomeTeam)),
                        "CAR")
                    ),
                    
                    sliderInput(inputId = "dateSel",
                                label = "Filter by Season",
                                min = min(nflDataClean$Season),
                                max = max(nflDataClean$Season),
                                value = c(2009,2016),
                                step = 1,
                                round = TRUE),
                    # Partial example
                    checkboxInput(inputId = "numVar1Check", label = "Filter on Rush Yards Per Game?"),
                    conditionalPanel(
                      condition = "input.numVar1Check == true",
                      sliderInput(inputId = "rushYardsFilter", 
                                  label="Filter on Rush Yards Per Game",
                                  min = 0,
                                  max = 500,
                                  value = c(200, 250),
                                  step = 1,
                                  round = TRUE)
                      ),
                      checkboxInput(inputId = "numVar2Check", label = "Filter on Pass Yards Per Game?"),
                      conditionalPanel(
                        condition = "input.numVar2Check == true",
                        sliderInput(inputId = "passYardsFilter", 
                                    label="Filter on Pass Yards Per Game",
                                    min = 0,
                                    max = 500,
                                    value = c(200, 250),
                                    step = 1,
                                    round = TRUE)
                        ),
                    actionButton("filterButton","Filter Data")
                    ),
  navset_card_underline(
    title = NULL,
    nav_panel("About", card(
      img(src = "football.png"),
      h3("Introduction"),
      "The purpose of this application is to explore the NFL Play-By-Play Dataset. In this app, the user is able to exlore summary data from NFL play by play data from 2009-2016",
      tags$a(href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016?resource=download", "Download the Data Here"),
      h3("What this app can do"),
      "This app gives the user ability to look at summary statistics across every NFL team, across every season. The app also gives the user the ability to view the stats from two NFL teams side-by side"
      
    )),
    nav_panel("Discover NFL Data",
              selectizeInput(inputId = "summarySel",
                             label = "Select Summary Visualization",
                             choices = c("NumericSummaries",
                                         "PlaysRun",
                                         "Run vs Pass Attempts",
                                         "Histograms for Pass and Rush Yards per Attempt",
                                         "Box Plots for Pass and Rush Yards per Attempt",
                                         "Pass Yards vs Rush Yards Per Game Scatter Plot",
                                         "Evolution of Yards Gained Over Time"),
                             selected ="NumericSummaries"),
    
    conditionalPanel(
      condition = "input.summarySel == 'NumericSummaries'",
      tableOutput("numericSummaries")
    ),
    
    conditionalPanel(
      condition = "input.summarySel == 'PlaysRun'",
      tableOutput("playsRunTable")
    ),
    conditionalPanel(
      condition = "input.summarySel == 'Run vs Pass Attempts'",
      tableOutput("runAndPassAttempts")
    ),
    conditionalPanel(
      condition = "input.summarySel == 'Histograms for Pass and Rush Yards per Attempt'",
      card(card_header("Rush Yards Histogram"),
           plotOutput("rushYdsHist")),
      card(card_header("Pass Yards Histogram"),
           plotOutput("passYdsHist"))
    ),
    
    conditionalPanel(
      condition = "input.summarySel == 'Pass Yards vs Rush Yards Per Game Scatter Plot'",
      card(card_header("Graphical Summaries"),
           plotOutput("p"))
    ),
    conditionalPanel(
      condition = "input.summarySel == 'Evolution of Yards Gained Over Time'",
      card(card_header("Graphical Summaries"),
           plotOutput("timeSeries"))
    ),
    conditionalPanel(
      condition = "input.summarySel == 'Box Plots for Pass and Rush Yards per Attempt'",
      card(card_header("Box Plot of Pass and Rush Attempts"),
           plotOutput("boxPlot")))
              ),
    nav_panel("Download Data",downloadButton("downloadData", label = "Download Current Subset of Data", class = NULL))
  ))

server = function(input, output, session){
  #now, update the 'x' selections available
  observeEvent(input$team1Sel, {
    team1 <- input$team1Sel
    team2 <- input$team2Sel
    choices <- levels(nflDataClean$HomeTeam)
    if (team1 != team2){
      choices <- choices[-which(choices == team1)]
      updateSelectizeInput(session,
                           "team2Sel",
                           choices = choices,
                           selected = team2)
    }
  })
 
  observeEvent(input$team2Sel, {
    team1 <- input$team1Sel
    team2 <- input$team2Sel
    choices <- levels(nflDataClean$HomeTeam)
    if (team1 != team2){
      choices <- choices[-which(choices == team2)]
      updateSelectizeInput(session,
                           "team1Sel",
                           choices = choices,
                           selected = team1)
    }
  })
  
  observeEvent(input$filterButton, {
    
    ## Filter on Teams first (Home and away)
    if(input$headToHeadCheck){
      nflDataFiltered = nflDataClean |> filter((HomeTeam == input$team1Sel | AwayTeam == input$team1Sel) & (HomeTeam == input$team2Sel | AwayTeam == input$team2Sel)) |>
        filter(Season >= input$dateSel[1] & Season <= input$dateSel[2])
    }
    else{
      nflDataFiltered = nflDataClean |> filter(Season >= input$dateSel[1] & Season <= input$dateSel[2])
    }
    
    
    ## Then perform summaries for total rush and pass yds per game
    ydsYears = nflDataFiltered |> select(Season, GameID, PlayType, Yards.Gained, posteam) |>
      filter(PlayType == "Run" | PlayType == "Pass") |>
      drop_na(posteam) |>
      group_by(GameID, Season, posteam, PlayType) |>
      summarize(ydsPerGame = sum(Yards.Gained))
    
    summaryComp = ydsYears |> filter(PlayType == "Pass") |> select(GameID, Season, posteam, ydsPerGame)
    
    runs = ydsYears |> ungroup()|> filter(PlayType == "Run") |> select(ydsPerGame) |> rename(rushYdsPerGame = ydsPerGame)
    
    
    summaryComp$rushYdsPerGame = runs$rushYdsPerGame
    summaryComp = summaryComp |> rename(passYdsPerGame = ydsPerGame) |> 
      mutate(teamYr = paste(Season, posteam, sep = "-"))
    
    ## Conditions to filter, but only in the button is pressed
    if(input$numVar1Check){
      summaryComp = summaryComp |> filter(rushYdsPerGame >= input$rushYardsFilter[1] & rushYdsPerGame <= input$rushYardsFilter[2])
      filteredGames = distinct(summaryComp, GameID, .keep_all = TRUE)
      nflDataFiltered = nflDataFiltered |> filter(GameID %in% filteredGames$GameID)
    
      }
    
    if(input$numVar2Check){
      summaryComp = summaryComp |> filter(passYdsPerGame >= input$passYardsFilter[1] & passYdsPerGame <= input$passYardsFilter[2])
      filteredGames = distinct(summaryComp, GameID, .keep_all = TRUE)
      nflDataFiltered = nflDataFiltered |> filter(GameID %in% filteredGames$GameID)
    }
    
    
    ## Get Games that have been filtered by everything so that we can get the results
    
    
    myGames = nflDataFiltered
    ## Game Results:
    
    ## Get games involving both teams
    #head(myGames)
    #get vector of games
    
    games = unique(myGames$GameID)
    
    ## Get Final Scores
    myGames = myGames |> filter(GameID %in% games) |> group_by(GameID) |> 
      drop_na(PosTeamScore, DefTeamScore) |>
      mutate(homeTeamScore = 
               ifelse(HomeTeam == posteam, PosTeamScore, DefTeamScore),
             awayTeamScore = 
               ifelse(AwayTeam == posteam, PosTeamScore, DefTeamScore)) |>
      mutate(homeTeamFinalScore = max(homeTeamScore),
             awayTeamFinalScore = max(awayTeamScore)) |>
      distinct(GameID, .keep_all = TRUE) |> select(Date,
                                                   GameID,
                                                   HomeTeam,
                                                   homeTeamFinalScore,
                                                   AwayTeam,
                                                   awayTeamFinalScore
      ) |>
      mutate(winningTeam = factor(ifelse((homeTeamFinalScore > awayTeamFinalScore), as.character(HomeTeam), as.character(AwayTeam))))
    
    
    
    #output$table <- renderTable({
    #  gt(myGames |> ungroup()) |> tab_header("Matchup Results")
    #})
    
    ## numeric Variables:
    
    ##Histogram of Runs yds gained
    output$runYdsHist = renderPlot({
      g = ggplot(nflDataFiltered |> filter(PlayType == "Run"), aes(x = Yards.Gained))
      g + geom_density(aes(fill = posteam))
    })
    
    ## Histogram for Pass Yards Gained
    output$passYdsHist = renderPlot({
      g = ggplot(nflDataFiltered |> filter(PlayType == "Pass"), aes(x = Yards.Gained))
      g + geom_density(aes(fill = posteam))
    })
    
    output$rushYdsHist = renderPlot({
      g = ggplot(nflDataFiltered |> filter(PlayType == "Run"), aes(x = Yards.Gained))
      g + geom_density(aes(fill = posteam))
    })
    
    output$wins = renderPlot({
      wins = myGames |> mutate(
        homeTeamWon = ifelse(HomeTeam == winningTeam, 1, 0), 
        awayTeamWon = ifelse(AwayTeam == winningTeam, 1, 0)) |> group_by(HomeTeam)|> summarize(totalWins = sum(homeTeamWon + awayTeamWon))
      
      g = ggplot(wins, aes(x = HomeTeam, y = totalWins))
      g + geom_col(aes(fill = HomeTeam))
    })
    
    output$numericSummaries = renderTable({
      summaries = nflDataFiltered |> group_by(posteam) |>
        summarize(across(where(is.numeric), 
                         list("mean" = mean), 
                         .names = "{.fn}_{.col}"))
      
      gt(summaries)
    })
    
    
  output$playsRunTable = renderTable({
    plays = nflDataFiltered |> group_by(PlayType)|> select(PlayType) |> summarise(count = n()) |> arrange(desc(count))
    gt(plays) |> tab_header(title = "Types of Plays Run")
    
  })
  
  output$runAndPassAttempts = renderTable({
    passAndRunAttempts = nflDataFiltered |> 
      filter(PlayType == "Run" | PlayType == "Pass") |>
      group_by(posteam, PlayType) |> summarize(count = n())
    
    gt(passAndRunAttempts |> pivot_wider(names_from = posteam, values_from = count))
  })
  
  output$boxPlot = renderPlot({
    g = ggplot(nflDataFiltered |> filter(PlayType == "Pass" | PlayType == "Run"), aes(x = Yards.Gained))
    g + geom_boxplot(aes(fill = PlayType)) + facet_wrap(posteam ~.)
    })
    
    
  
  output$p <- renderPlot({
    
    
    g = ggplot(summaryComp, aes(x = rushYdsPerGame, y = passYdsPerGame))
    g + geom_point(aes(color = posteam))
    
  })
  
  output$timeSeries = renderPlot({
    ydsYears = nflDataFiltered |> select(Date, Season, GameID, PlayType, Yards.Gained, posteam) |>
      filter(PlayType == "Run" | PlayType == "Pass") |>
      drop_na(posteam) |>
      group_by(Date, GameID, Season, posteam, PlayType) |>
      summarize(ydsPerPlay = sum(Yards.Gained))
    
    summaryComp = ydsYears |> filter(PlayType == "Pass") |> select(Season, posteam, ydsPerPlay)
    
    runs = ydsYears |> ungroup()|> filter(PlayType == "Run") |> select(ydsPerPlay) |> rename(rushYdsPerPlay = ydsPerPlay)
    
    
    summaryComp$rushYdsPerPlay = runs$rushYdsPerPlay
    summaryComp = summaryComp |> rename(passYdsPerPlay = ydsPerPlay) |> 
      mutate(teamYr = paste(Season, posteam, sep = "-"))
    
    
    g = ggplot(summaryComp, aes(x = Date, y = passYdsPerPlay)) +geom_line(aes(group = posteam, color = posteam))
    
    g + facet_wrap(posteam ~ .)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
         paste('NFLdata-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(nflDataFiltered, con)
       }
    )
  })


  
} 

shinyApp(ui, server)

