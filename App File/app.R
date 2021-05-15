library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(data.table)
library(readxl)
library(reshape2)
library(rsq)


setwd("/Users/manu/appfinal")
matches <- as.data.frame(read_excel("Final_Matches.xlsx"))
deliveries <- as.data.frame(read_excel("Final_Deliveries.xlsx"))
players <- as.data.frame(read_excel("Final Players.xlsx"))

df <- deliveries %>% inner_join(matches, by = c('match_id' = 'id'))
season_filter <- sort(as.character(unique(df$season)), decreasing = T)

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = 'IPL Dashboard'
  ),
  dashboardSidebar( 
                    sidebarMenu(
                      menuItem('About', tabName = 'about', icon = icon('pen')),
                      menuItem('IPL - Overview', tabName = 'summary', icon = icon('trophy')),
                      menuItem('Top Performances - By Season', tabName = 'season', icon = icon('chart-bar')),
                      menuItem('Player Profile', tabName = 'player', icon = icon('id-card')),
                      menuItem("Regression Model",tabName = "regression",icon = icon("glyphicon glyphicon-link", lib = "glyphicon"))
                      
                    )
  ),
  dashboardBody(
    tabItems(
      
      # About -------------------------------------------------------------------
      tabItem(tabName = 'about',
              h2('INDIAN PREMIER LEAGUE', align = 'center'),
              tags$p('The Indian Premier League (IPL) is a professional Twenty20 cricket league in India contested during
              March or April and May of every year by eight teams representing eight different cities in India.
              The league was founded by the Board of Control for Cricket in India (BCCI) in 2008. 
              The league which is played by 8 teams represent 8 different cities in INDIA', 
                     style = 'font-size: 120%'),
              h5('Source: Wikipedia', align = 'right'),  
              h3('Teams of the League', align = 'center'),
              
              fluidRow(
                shinydashboard::box(width = 12, background = 'black',
                                    valueBox(tags$p('Chennai Super Kings (CSK)', style = 'font-size: 30%;text-align: center;'),
                                            
                                             div(img(src = 'csk.jpeg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Mumbai Indians (MI)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'mi.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Sunrises Hyderabad (SRH)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'srh.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Delhi Capitals (DC)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'dc.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Kolkata Knight Riders (KKR)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'kkr.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Kings XI  Punjab (KXIP)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'punjab.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Royal Challengers Bangalore (RCB)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'rcb.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Rajasthan Royals (RR)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'rr.jpg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Rising Pune Supergiant (RPS)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'rps.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Gujarat Lions (GL)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'gujarath.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Deccan Chargers (DC)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'decan.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('Delhi Daredevils (DD)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'delhidare.png', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black')
                )
              )
      ),
      
      # Overview -----------------------------------------------------------------
      
      tabItem(tabName = 'summary',
              
              shinydashboard::box(title = 'Overview',
                                  width = 12,
                                  solidHeader = T, 
                                  collapsible = F,
                                  background  = 'black',
                                  fluidRow(
                                    infoBoxOutput('matches'),tags$style('#matches {width:250px;}'),
                                    infoBoxOutput('teams'),tags$style('#teams {width:250px;}'),
                                    infoBoxOutput('runs'),tags$style('#runs {width:250px;}'),
                                    infoBoxOutput('wickets'),tags$style('#wickets {width:250px;}'),
                                    infoBoxOutput('seasons'),tags$style('#seasons {width:250px;}'),
                                    infoBoxOutput('mom'),tags$style('#mom {width:250px;}'),
                                    infoBoxOutput('sixes'),tags$style('#sixes {width:250px;}'),
                                    infoBoxOutput('fours'),tags$style('#fours {width:250px;}')
                                  )),
              tags$p('Expand to see details of below items', style = 'font-size: 120%;margin-left:2.5em;'),
              
              shinydashboard::box(title = 'IPL Season Winners List',
                                  width = 12,
                                  background  = 'black',
                                  solidHeader = T, 
                                  collapsible = T,
                                  collapsed = T,
                                  fluidRow(
                                    valueBox(tags$p('2008', style = 'font-size: 40%;text-align: center;'), div(img(src = 'rr.jpg', height = '80', width = '100'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2009', style = 'font-size: 40%;text-align: center;'), div(img(src = 'decan.png', height = '80', width = '100'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2010', style = 'font-size: 40%;text-align: center;'), div(img(src = 'CSK.jpeg', height = '80', width = '100'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2011', style = 'font-size: 40%;text-align: center;'), div(img(src = 'CSK.jpeg', height = '80', width = '100'),style='text-align: center;'),  
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2012', style = 'font-size: 40%;text-align: center;'), div(img(src = 'kkr.png', height = '80', width = '100'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2013', style = 'font-size: 40%;text-align: center;'), div(img(src = 'mi.png', height = '80', width = '100'),style='text-align: center;'),  
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2014', style = 'font-size: 40%;text-align: center;'), div(img(src = 'kkr.png', height = '80', width = '100'),style='text-align: center;'), 
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2015', style = 'font-size: 40%;text-align: center;'), div(img(src = 'mi.png', height = '80', width = '100'),style='text-align: center;'),  
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2016', style = 'font-size: 40%;text-align: center;'), div(img(src = 'srh.png', height = '80', width = '100'),style='text-align: center;'),  
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2017', style = 'font-size: 40%;text-align: center;'), div(img(src = 'mi.png', height = '80', width = '100'),style='text-align: center;'),  
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2018', style = 'font-size: 40%;text-align: center;'), div(img(src = 'CSK.jpeg', height = '80', width = '100'),style='text-align: center;'),  
                                             icon = NULL, width = 2, color = 'black'),
                                    valueBox(tags$p('2019', style = 'font-size: 40%;text-align: center;'), div(img(src = 'mi.png', height = '80', width = '100'),style='text-align: center;'),  
                                             icon = NULL, width = 2, color = 'black')
                                  )
              ),
              
              shinydashboard::box(title = 'Orange Cap Winners List',
                                  width = 12,
                                  background  = 'orange', 
                                  solidHeader = T, 
                                  collapsed = T,
                                  collapsible = T,
                                  shinydashboard::box( background  = 'black',width = 12,
                                                       valueBox(tags$p('2008 - Shaun Marsh (KXIP)', style = 'font-size: 30%;text-align: center;'), div(img(src = "Shaun.png", 
                                                                                                                                                           height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),
                                                       valueBox(tags$p('2009 - Matthew Hayden (CSK)',style = 'font-size: 30%;text-align: center;'), div(img(src = "hyden.png", 
                                                                                                                                                            height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),
                                                       valueBox(tags$p('2010 -  Sachin Tendulkar (MI)', style = 'font-size: 30%;text-align: center;'), div(img(src = "sachin.png", 
                                                                                                                                                               height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),
                                                       valueBox(tags$p('2011 - Chris Gayle (RCB)', style = 'font-size: 30%;text-align: center;'), div(img(src = "gayle.jpg", 
                                                                                                                                                          height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),
                                                       valueBox(tags$p('2012 - Chris Gayle (RCB)', style = 'font-size: 30%;text-align: center;'), div(img(src = "gayle.jpg", 
                                                                                                                                                          height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),                                                                                              
                                                       valueBox(tags$p('2013 - Michael Hussey (CSK)', style = 'font-size: 30%;text-align: center;'), div(img(src = "hussey.png",
                                                                                                                                                             height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),
                                                       valueBox(tags$p('2014 - Robin Uthappa (KKR)', style = 'font-size: 30%;text-align: center;'), div(img(src = "uttappa.jpg", 
                                                                                                                                                            height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),
                                                       valueBox(tags$p('2015 - David Warner (SRH)', style = 'font-size: 30%;text-align: center;'), div(img(src = "warner.jpeg", 
                                                                                                                                                           height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),
                                                       valueBox(tags$p('2016 - Virat Kohli (RCB)', style = 'font-size: 30%;text-align: center;'), div(img(src = "kohili.png", 
                                                                                                                                                          height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),
                                                       valueBox(tags$p('2017 - David Warner (SRH)', style = 'font-size: 30%;text-align: center;'), div(img(src = "warner.jpeg", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p('2018 - Kane Williamson (SRH)', style = 'font-size: 30%;text-align: center;'), div(img(src = "wiliamson.png", 
                                                                                                                                                              height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'),
                                                       valueBox(tags$p('2019 - David Warner (SRH)', style = 'font-size: 30%;text-align: center;'), div(img(src = "warner.jpeg", 
                                                                                                                                                           height = '80', width = '100'),style='ext-align: center;'), icon = NULL, width = 2, color = 'black'))
              ),
              
              shinydashboard::box(title = 'Purple Cap Winners List',
                                  width = 12,
                                  background  = 'purple', 
                                  solidHeader = T, 
                                  collapsible = T,
                                  collapsed = T,
                                  shinydashboard::box( background  = 'black',width = 12,
                                                       valueBox(tags$p("2008 - Sohail Tanvir (RR)", style = "font-size: 30%;text-align: center;"), div(img(src = "B1.png", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2009 - R. P. Singh (DC)",style = "font-size: 30%;text-align: center;"), div(img(src = "B2.png", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2010 -  Pragyan Ojha (DC)", style = "font-size: 30%;text-align: center;"), div(img(src = "B3.png", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2011 - Lasith Malinga (MI)", style = "font-size: 30%;text-align: center;"), div(img(src = "B4.png", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2012 - Morne Morkel (DD)", style = "font-size: 30%;text-align: center;"), div(img(src = "B5.png", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2013 - Dwayne Bravo (CSK)", style = "font-size: 30%;text-align: center;"), div(img(src = "bravo.jpg", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2014 - Mohit Sharma (CSK)", style = "font-size: 30%;text-align: center;"), div(img(src = "B7.png", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2015 - Dwayne Bravo (CSK)", style = "font-size: 30%;text-align: center;"), div(img(src = "bravo.jpg", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2016 - Bhuvneshwar Kumar (SRH)", style = "font-size: 30%;text-align: center;"), div(img(src = "B8.png", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2017 - Bhuvneshwar Kumar (SRH)", style = "font-size: 30%;text-align: center;"), div(img(src = "B8.png", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2018 - Andrew Tye (KXIP)", style = "font-size: 30%;text-align: center;"), div(img(src = "tye.jpg", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black"),
                                                       valueBox(tags$p("2019 - Imran Tahir (CSK)", style = "font-size: 30%;text-align: center;"), div(img(src = "B10.png", height = "80", width = "100"),style="text-align: center;"), 
                                                                icon = NULL, width = 2, color = "black")
                                  )
              ),
      ),
      
      # Season Analysis ---------------------------------------------------------
      
      tabItem(tabName = 'season',
              fluidPage(
                selectInput('season_filter', 'Select Season:',
                            season_filter),
                fluidRow(
                  shinydashboard::box(title = 'Batting Analysis',width = 8,solidHeader = T, background = 'black',
                                      tabBox(width = 12,
                                             title = NULL,
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = 'tabset1', height = '260px',
                                             tabPanel(tags$p('Most Runs', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_batsman', height = 200)),
                                             tabPanel(tags$p('Most Hundreds', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('most_100s', height = 200)),
                                             tabPanel(tags$p('Most Fifties', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('most_50s', height = 200)),
                                             tabPanel(tags$p('Most Sixes', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_6s', height = 200)),
                                             tabPanel(tags$p('Most Fours', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_4s', height = 200))
                                      )
                  ),
                  shinydashboard::box(title = 'Bowling Analysis',width = 8,solidHeader = T, background = 'black',
                                      tabBox(width = 12,
                                             title = NULL,
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = 'tabset2', height = '270px',
                                             tabPanel(tags$p('Most Wickets', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_bowlers', height = 210)),
                                             tabPanel(tags$p('Most Maidens', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('maiden', height = 210)),
                                             tabPanel(tags$p('Most Dot Balls', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('dot_balls', height = 210)),
                                             tabPanel(tags$p('Most 4 Wickets', style = 'color:black;font-weight:bold;'),
                                                      plotOutput('wickets_4', height = 210))
                                      )
                  )
                )
              )
      ),
      
      # Player profile ----------------------------------------------------------
      tabItem(tabName = 'player',
              fluidPage(
                selectInput('player', 'Select Player:',
                            unique(df$batsman)),
                fluidRow(
                  shinydashboard::box( title = 'Teams Represented', 
                                       width = 4, solidHeader = TRUE, background = 'black',
                                       uiOutput('teamsplayedui')),
                  
                  shinydashboard::box(
                    title = 'Batting', 
                    width = 4, solidHeader = TRUE, background = 'black',
                    uiOutput('batting_ui')
                    
                  ),
                  shinydashboard::box(
                    title = 'Bowling', width = 4, solidHeader = TRUE, background = 'black',
                    uiOutput('bowling_ui')
                    
                  )
                )
              )
      ),
      # Regression Plot ------------------------------------------------------
      
      tabItem(
        tabName = "regression",
        tabsetPanel(
          
          
          
          tabPanel("Regression Plot",
                   fluidRow(column(5, selectInput("v1", "Select the dependent variable:"," ")),
                            column(5, selectInput("v2","Select the independent variable:"," ")),
                            column(9, plotOutput("rplot",height = 500)),
                            column(7, "Slope"),
                            column(7, textOutput("pred1slope")),
                            column(7, "R Squared"),
                            column(7,textOutput("Rsquared"))
                   ) 
          )
        )
      )
      
      
    )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output,session) { 
  teams <-reactive(read.csv("teamwise_home_and_away.csv"))
  
  # Overview Outputs --------------------------------------------------------
  
  output$matches <- renderInfoBox({
    infoBox('# Matches', df %>% summarise(matches = n_distinct(match_id)),
            icon = icon('handshake'), color = 'aqua', fill = T, width = 1)
  })
  output$seasons <- renderInfoBox({
    infoBox('# Season', df %>% summarise(matches = n_distinct(season)),
            icon = icon('trophy'), color = 'aqua', fill = T, width = 1)
  })
  output$runs <- renderInfoBox({
    infoBox('# Runs', df %>% summarise(runs = sum(total_runs)), 
            icon = icon('walking'), color = 'aqua', fill = T, width = 1)
  })
  output$teams <- renderInfoBox({
    infoBox('# Teams', df %>% summarise(teams = n_distinct(batting_team)), 
            icon = icon('users'), color = 'aqua', fill = T, width = 1)
  })
  output$wickets <- renderInfoBox({
    infoBox('# Wickets', df %>% filter(dismissal_kind %in% 
                                         c('bowled', 'caught', 'caught and bowled', 'lbw', 'hit wicket', 'stumped'))%>%  summarise(wickets = n()), 
            icon = icon('hand-pointer'), color = 'aqua', fill = T, width = 1)
  })
  output$fours <- renderInfoBox({
    infoBox('# Fours', filter(df, batsman_runs == 4) %>% summarise(fours = n()), 
            icon = icon('dice-four'), color = 'aqua', fill = T, width = 1)
  })
  output$sixes <- renderInfoBox({
    infoBox('# Sixes', filter(df, batsman_runs == 6) %>% summarise(fours = n()), 
            icon = icon('dice-six'), color = 'aqua', fill = T, width = 1)
  })
  output$mom <- renderInfoBox({
    infoBox('Most MOM', tags$p((df %>% group_by(player_of_match) %>% summarise(num = n_distinct(match_id)) %>% 
                                  arrange(desc(num)) %>% head(1)), style = 'font-size: 90%;'),
            icon = icon('user-plus'), color = 'aqua', fill = T, width = 1)
  })
  
  # Season Outputs --------------------------------------------------------
  
  output$top_10_batsman <- renderPlot({
    filter(df %>% group_by(batsman, season) %>% summarise(runs = sum(batsman_runs)), 
           season == input$season_filter) %>% arrange(desc(runs)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-runs), y = runs)) + geom_bar(stat="identity", width=0.6, fill="orange1") +
      geom_text(aes(label = as.numeric(runs), vjust = 2)) +
      labs(x = 'Batsman', y = 'Total Runs Scored') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$most_100s <- renderPlot({
    filter(df %>% group_by(batsman, season, match_id) %>% summarise(runs = sum(batsman_runs)),
           runs >= 100 & season == input$season_filter) %>% group_by(batsman) %>% summarise(hundreds = n()) %>% 
      arrange(desc(hundreds)) %>% 
      ggplot(aes(x = reorder(batsman,-hundreds), y = hundreds)) + geom_bar(stat="identity", width=0.6, fill="red") +
      geom_text(aes(label = as.numeric(hundreds), vjust = 2)) +
      labs(x = 'Batsman', y = 'No. of 50s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$most_50s <- renderPlot({
    filter(df %>% group_by(batsman, season, match_id) %>% summarise(runs = sum(batsman_runs)),
           runs >= 50 & season == input$season_filter) %>% group_by(batsman) %>% summarise(fifties = n()) %>% 
      arrange(desc(fifties)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-fifties), y = fifties)) + geom_bar(stat="identity", width=0.6, fill="green") +
      geom_text(aes(label = as.numeric(fifties), vjust = 2)) +
      labs(x = 'Batsman', y = 'No. of 50s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$top_10_6s <- renderPlot({
    filter(as.data.frame(filter(df, batsman_runs == 6) %>% group_by(batsman, season) %>% summarise(sixes = n())), 
           season == input$season_filter) %>% arrange(desc(sixes)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-sixes), y = sixes)) + geom_bar(stat="identity", width=0.6, fill="maroon") +
      geom_text(aes(label = as.numeric(sixes), vjust = 2)) +
      labs(x = 'Batsman', y = 'No. of 6s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$top_10_4s <- renderPlot({
    filter(as.data.frame(filter(df, batsman_runs == 4) %>% group_by(batsman, season) %>% summarise(fours = n())), 
           season == input$season_filter) %>% arrange(desc(fours)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-fours), y = fours)) + geom_bar(stat="identity", width=0.6, fill="navy") +
      geom_text(aes(label = as.numeric(fours), vjust = 2)) +
      labs(x = 'Batsman', y = 'No. of 4s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$top_10_bowlers <- renderPlot({
    filter(df %>% filter(dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 'lbw', 'hit wicket', 'stumped')) %>%
             group_by(bowler, season) %>% summarise(wickets = n()), 
           season == input$season_filter) %>% arrange(desc(wickets)) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler, -wickets), y = wickets)) + geom_bar(stat = 'identity', width = 0.6 , fill = 'orange1') +
      geom_text(aes(label = as.numeric(wickets), vjust = 2)) +
      labs(x = 'Bowler', y = 'Total Wickets Taken') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$maiden <- renderPlot({
    data.frame(filter(filter(df, season== input$season_filter)%>% group_by(match_id,inning, over,bowler) %>% 
                        summarise(runs = sum(batsman_runs)-sum(bye_runs)), runs ==0) %>% group_by(bowler) %>% summarise(maiden =n())) %>%
      inner_join( data.frame(filter(df, season== input$season_filter)%>% group_by(match_id,inning, over,ball,bowler) %>% 
                               summarise(runs_given = sum(batsman_runs)-sum(bye_runs)-sum(legbye_runs)) %>% group_by(bowler) %>% 
                               summarise(runs_given= sum(runs_given), overs = n_distinct(match_id,over))) %>% mutate(econ = round(runs_given/overs,2)),
                  by = "bowler") %>% arrange(desc(maiden), econ) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler, -maiden), y = maiden)) + geom_bar(stat = 'identity', width = 0.6 , fill = 'red') +
      geom_text(aes(label = as.numeric(maiden), vjust = 2)) +
      labs(x = 'Bowler', y = '# of times 4 Wickets') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$dot_balls <- renderPlot({
    filter(df, season== input$season_filter & total_runs == 0)%>% group_by(bowler) %>% summarise(dot = n())  %>% 
      arrange(desc(dot)) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler, -dot), y = dot)) + 
      geom_bar(stat = 'identity', width = 0.6 , fill = 'green') +
      geom_text(aes(label = as.numeric(dot), vjust = 2)) +
      labs(x = 'Bowler', y = '# of dot balls') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$wickets_4 <- renderPlot({
    filter(filter(df, season== input$season_filter) %>% 
             filter(dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                          'lbw','hit wicket', 'stumped')) %>% group_by(bowler, match_id) %>% summarise(wickets = n()), 
           wickets == 4) %>% group_by(bowler) %>% summarise(Four_wickets = n()) %>% 
      ggplot(aes(x = reorder(bowler, -Four_wickets), y = Four_wickets)) + 
      geom_bar(stat = 'identity', width = 0.6 , fill = 'maroon') +
      geom_text(aes(label = as.numeric(Four_wickets), vjust = 2)) +
      labs(x = 'Bowler', y = '# of times 4 Wickets') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  # Player Profile Output ---------------------------------------------------
  
  output$teamsplayedui <- renderUI({
    span(style = 'color:black;font-weight:bold;', DT::DTOutput('teamsplayed'))
  })
  
  output$teamsplayed <- DT::renderDataTable({
    filter(df,batsman == input$player) %>% distinct(batting_team)
  }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), colnames = NULL,  rownames = NULL
  )
  
  output$batting_ui <- renderUI({
    if(nrow(filter(df,batsman == input$player)) == 0) 
      return('No data to show')
    span(style = 'color:black;font-weight:bold;', DT::DTOutput('batting_table'))
  })
  
  output$batting_table <- DT::renderDataTable(
    {
      t(distinct(data.frame(if(nrow(filter(df, batsman == input$player) %>% summarise(Innings = n_distinct(match_id,inning))) == 0) {0} 
                            else {filter(df, batsman == input$player) %>% summarise(Innings = n_distinct(match_id,inning))},
                            if(nrow(filter(df, batsman == input$player) %>% summarise(Runs = sum(batsman_runs))) == 0) {0} 
                            else {filter(df, batsman == input$player) %>% summarise(Runs = sum(batsman_runs))},
                            if(nrow(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                    summarise(runs = sum(batsman_runs)) %>% filter(runs == max(runs)) %>% select(runs)) == 0) {0} 
                            else {filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                summarise(runs = sum(batsman_runs)) %>% filter(runs == max(runs)) %>% select(runs)},
                            if(nrow(filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs >= 50 & runs <100) %>% summarise(fifties = n())) == 0) {0} 
                            else {filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs >= 50 & runs <100) %>%  summarise(fifties = n())},
                            if(nrow(filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs >= 100) %>%  summarise(Hundreds = n())) == 0) {0} 
                            else {filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs >= 100) %>%  summarise(Hundreds = n())},
                            if(nrow(filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs == 0) %>%  summarise(Ducks = n())) == 0) {0} 
                            else {filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs == 0) %>%  summarise(Ducks = n())},
                            if(nrow(filter(df, batsman == input$player & batsman_runs == 4) %>% summarise(Fours = n())) == 0) {0} 
                            else {filter(df, batsman == input$player & batsman_runs == 4) %>% summarise(Fours = n())},
                            if(nrow(filter(df, batsman == input$player & batsman_runs == 6) %>% summarise(Sixes = n())) == 0) {0} 
                            else { filter(df, batsman == input$player & batsman_runs == 6) %>% summarise(Sixes = n())}
      ))) 
      
    }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), colnames = NULL,
    rownames = c('Innings', 'Runs', 'Highest Score', 'Fifties', 'Hundreds', 'Ducks', 'Fours', 'Sixes')
  ) 
  
  output$bowling_ui <- renderUI({
    if(nrow(filter(df,bowler == input$player)) == 0) 
      return('No data to show')
    span(style = 'color:black;font-weight:bold;', DT::DTOutput('bowling_table'))
  })
  
  output$bowling_table <- DT::renderDataTable(
    {
      t(distinct(data.frame(if(nrow(filter(df,bowler == input$player) %>% summarise(Overs = n_distinct(match_id,over))) == 0) {0} 
                            else {filter(df,bowler == input$player) %>% summarise(Overs = n_distinct(match_id,over))},
                            
                            if(nrow(filter(df,bowler == input$player) %>% summarise(Balls = n_distinct(match_id,over,ball))) == 0) {0} 
                            else {filter(df,bowler == input$player) %>% summarise(Balls = n_distinct(match_id,over,ball))},
                            
                            if(nrow(data.frame(filter(filter(df,bowler == input$player) %>% group_by(match_id,over,bowler) %>% 
                                                      summarise(runs= sum(total_runs)), runs == 0) %>% group_by(runs) %>%
                                               summarise(maiden = n())) %>% select('maiden')) == 0) {0} 
                            else {data.frame(filter(filter(df,bowler == input$player) %>% group_by(match_id,over,bowler) %>% 
                                                      summarise(runs= sum(total_runs)), runs == 0) %>% group_by(runs) %>%
                                               summarise(maiden = n())) %>% select('maiden')},
                            
                            if(nrow(filter(df,bowler == input$player & total_runs == 0 ) %>% summarise(dot = n())) == 0) {0} 
                            else {filter(df,bowler == input$player & total_runs == 0 ) %>% summarise(dot = n())},
                            
                            if(nrow(filter(df,bowler == input$player) %>% summarise(runs_given = sum(total_runs))) == 0) {0} 
                            else {filter(df,bowler == input$player) %>% summarise(runs_given = sum(total_runs)) %>% select(runs_given)},
                            
                            if(nrow(filter(df,bowler == input$player & dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                                                                             'lbw','hit wicket', 'stumped'))%>% summarise(wickets = n())) == 0) {0} 
                            else {filter(df,bowler == input$player & dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                                                                           'lbw', 'hit wicket', 'stumped'))%>% summarise(wickets = n())},
                            
                            if(nrow(filter(filter(df, bowler == input$player  & dismissal_kind %in% c('bowled', 'caught', 
                                                                                                      'caught and bowled', 'lbw','hit wicket', 'stumped')) %>% group_by(bowler, match_id) %>% 
                                           summarise(wickets = n()),  wickets == 4) %>% summarise(Four_wickets = n()) %>% select(Four_wickets)) == 0) {0} 
                            else {filter(filter(df, bowler == input$player  & dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                                                                                    'lbw','hit wicket', 'stumped')) %>% group_by(bowler, match_id) %>% summarise(wickets = n()), 
                                         wickets == 4) %>% summarise(Four_wickets = n()) %>% select(Four_wickets)},
                            
                            if(nrow(filter(df, bowler == input$player) %>% summarise(runs_given = sum(total_runs), 
                                                                                     overs = n_distinct(match_id,over)) %>% mutate(econ = round(runs_given/overs,2)) %>% select(econ)) == 0) {0} 
                            else {filter(df, bowler == input$player) %>% summarise(runs_given = sum(total_runs), 
                                                                                   overs = n_distinct(match_id,over)) %>% mutate(econ = round(runs_given/overs,2)) %>% select(econ)}
      )))
      
    }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), colnames = NULL,
    rownames = c('Overs', 'Balls', 'Maiden', 'Dot Balls', 'Run Conceded', 'Wickets', '4 Wickets in Innings', 'Economy' )
  ) 
  
  
  #Regression Output ---------------------------------------------------
  observe(
    updateSelectInput(session,"v1",choices = names(teams()))
  )
  
  observe(
    updateSelectInput(session,"v2",choices = names(teams()))
  )
  y11<-reactive({
    teams()[,input$v1]
  })
  x11<-reactive({
    teams()[,input$v2]
  }) 
  
  
  model <-reactive( lm(y11() ~ x11()))
  
  output$rplot<- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(x11(), y11(),xlab = input$v2 , ylab = input$v1)
    
    
    {abline(model(), col = "blue", lwd = 2)}
    output$pred1slope <- renderText({model()[[1]][2]})
    r2 <- rsq(lm(y11()~ x11()))
    output$Rsquared <- renderText(r2)
  })
 
  
  
}

shinyApp(ui, server)