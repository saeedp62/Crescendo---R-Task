library(shiny)
library(shinydashboard)

library(dplyr)
library(purrr)
library(rlang)
library(stringr)

library(DT)
library(r2d3)

library(pinnacle.data)
library(odds.converter)
library(tidyverse)
library(stringr)

data("MLB2016")

clean_pitcher_names <- function(Pitcher){
    
    Pitcher <- tolower(str_replace_all(Pitcher, ' ', ''))
    InitialLetterPitcher = str_sub(Pitcher, end = 1)
    Pitcher = str_sub(Pitcher, start = 2)
    Pitcher = str_c(InitialLetterPitcher, Pitcher, sep = ' ')
    
    return(Pitcher)
}

# Clean pitching columns
MLB2016 <- MLB2016 %>% 
    mutate(
        AwayStartingPitcher = clean_pitcher_names(AwayStartingPitcher),
        HomeStartingPitcher = clean_pitcher_names(HomeStartingPicher)
    ) %>%
    select(-HomeStartingPicher)

# Q1 -------------------------------
# Create list of teams
team_list <- MLB2016 %>%
    collect() %>%
    .$HomeTeam %>%
    unique()

# Q2 -------------------------------
# Create list of Pitchers
pitchers_list <- MLB2016 %>%
    collect() %>%
    .$HomeStartingPitcher %>%
    unique()

pitchers_list2 <- MLB2016 %>%
    collect() %>%
    .$AwayStartingPitcher %>%
    unique()

pitchers <- append(pitchers_list, pitchers_list2) %>%
    unique() %>%
    sort()


top_pitchers <<- tibble(Pitcher = pitchers, Games = 0, Wins = 0)

# R function to increment Games, Wins fields in top_pitcher tibble base on the winner of the game
win_increment = function(x, output) {
    # x is the row of dataframe
    home_ind <- match(x[["HomeStartingPitcher"]], top_pitchers$Pitcher)
    away_ind <- match(x[["AwayStartingPitcher"]], top_pitchers$Pitcher)
    
    top_pitchers[home_ind,"Games"] <<- top_pitchers[home_ind,"Games"] + 1
    top_pitchers[away_ind,"Games"] <<- top_pitchers[away_ind,"Games"] + 1
    
    if (x[["FinalScoreHome"]] > x[["FinalScoreAway"]]){
        top_pitchers[home_ind,"Wins"] <<- top_pitchers[home_ind,"Wins"] + 1
    }
    else {
        top_pitchers[away_ind,"Wins"] <<- top_pitchers[away_ind,"Wins"] + 1
    }
}

# Apply the win_increment function to each row of the dataframe
# apply(MLB2016, 1, win_increment)
load(file = "pitchers_data_frame.Rda")
print(top_pitchers)

# Q2.a
#How would you rank the pitchers from best to worst using the information form the table above?
# consideing the above table, ranking the pitchers could be done by sorting based on two columns of
# Number of Games the pitcher played and the percent of wins in those games
print(top_pitchers %>% 
          arrange(desc(Games), desc(Wins/Games)))

# Q3 ----------------------------------
MoneyUS <- MLB2016 %>%
    unnest(cols = c(Lines)) %>% 
    group_by(GameID) %>% 
    arrange(desc(EnteredDateTimeUTC)) %>%
    subset(!is.na(.$MoneyUS2)) %>%
    mutate(First_MoneyUS2 = odds.us2prob(MoneyUS2[[n()]]),
           Last_MoneyUS2 = odds.us2prob(MoneyUS2[[1]])) %>%
    slice(1) %>% 
    ungroup() %>%
    mutate(WinHomeTeam = ifelse(FinalScoreHome > FinalScoreAway, 1, 0)) %>%
    select(GameID, First_MoneyUS2, Last_MoneyUS2, WinHomeTeam)

print(MoneyUS)

# Q3a --------------------------------
# Create Regression Model to predict the outcome of the Game
# WE use a logistic reggression and look at the siginificance of the predictors
input <- MoneyUS[, c("First_MoneyUS2", "Last_MoneyUS2", "WinHomeTeam")]
model = glm(formula = WinHomeTeam ~., data = input, family = binomial)
print(summary(model))
print(anova(model, test="Chisq"))

# Use rlang's set_names() to easily create a valide "choices"
# argument of the dropdown where the displayed text has to be
# different than the value passed as the input selection

month_list <- as.list(1:12) %>%
    set_names(month.name)

month_list$`All Year` <- 99

ui <- dashboardPage(
    dashboardHeader(
        title = "Major League Baseball",
        titleWidth = 235
    ),
    dashboardSidebar(
        sidebarMenu(id='sidebarmenu',
                    menuItem('Home', tabName = 'main', icon = icon("home")),
                    menuItem('Summary', tabName = 'summary', icon = icon("chart-bar")),
                    conditionalPanel("input.sidebarmenu === 'main'",
                                     br(),
                                     actionLink("remove", "Remove detail tabs"),
                                     selectInput(
                                         inputId = "team",
                                         label = "Team:",
                                         choices = team_list,
                                         selected = "Chicago Cubs",
                                         selectize = FALSE
                                     ))
                    )
        ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'main',
                    tabsetPanel(
                        id = "tabs",
                        tabPanel(
                            title = "Main Dashboard",
                            value = "page1",
                            fluidRow(
                                valueBoxOutput("total_games"),
                                valueBoxOutput("per_month"),
                                valueBoxOutput("percent_wins")
                            ),
                            fluidRow(),
                            fluidRow(
                                column(
                                    width = 6,
                                    d3Output("group_totals")
                                ),
                                column(
                                    width = 6,
                                    d3Output("top_pitchers")
                                )
                            )
                        )
                        )
                    ),
            tabItem(tabName = 'summary',
                    tabsetPanel(
                        id = "summary_tabs",
                        tabPanel(
                            title = "Question 2",
                            fluidRow(),
                            fluidRow(
                                column(
                                    width = 6,
                                    dataTableOutput("Q2")
                                    ),
                                column(
                                    width = 6,
                                    br(),
                                    br(),
                                    br(),
                                    d3Output("Q2a")
                                )
                                )
                            ),
                        tabPanel(
                            title = "Question 3",
                            fluidRow(),
                            fluidRow(
                                column(
                                    width = 6,
                                    dataTableOutput("Q3")
                                ),
                                column(
                                    width = 6,
                                    h2("Summary of Regression Model"),
                                    verbatimTextOutput("model_summary")
                                ),
                                column(
                                    width = 12,
                                    h2("Discussion"),
                                    h3("Interpreting the results of our logistic regression model"),
                                    verbatimTextOutput("discussion")
                                )
                            )
                        )
                        )
                    )
        )
    )
)

server <- function(input, output, session) {
    tab_list <- NULL
    
    # Use a reactive() function to prepare the base
    # SQL query that all the elements in the dashboard
    # will use. The reactive() allows us to evaluate
    # the input variables
    base_games <- reactive({
        res <- MLB2016 %>%
            filter(HomeTeam == input$team | AwayTeam == input$team ) %>%
            mutate(month = strftime(EventDateTimeET, "%m"),
                   year = strftime(EventDateTimeET, "%Y"),
                   day = strftime(EventDateTimeET, "%d"))
        #if (input$month != 99) res <- filter(res, as.integer(month) == input$month)
        res
    })
    
    # Total Games (server) ------------------------------------------
    output$total_games <- renderValueBox({
        # The following code runs inside the database.
        # pull() bring the results into R, which then
        # it's piped directly to a valueBox()
        base_games() %>%
            tally() %>%
            pull() %>%
            as.integer() %>%
            prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Number of Games")
    })
    
    # Avg per month (server) --------------------------------------------
    output$per_month <- renderValueBox({
        # The following code runs inside the database
        base_games() %>%
            group_by(year, month) %>%
            tally() %>%
            ungroup() %>%
            summarise(avg = mean(n)) %>%
            pull(avg) %>%
            round() %>%
            prettyNum(big.mark = ",") %>%
            valueBox(
                subtitle = "Average Games per month",
                color = "blue"
            ) 
    })
    
    # Percent wins (server) ----------------------------------------
    output$percent_wins <- renderValueBox({
        base_games() %>%
            filter(!is.na(FinalScoreHome)) %>%
            mutate(TeamWin = ifelse(((HomeTeam == input$team) & (FinalScoreHome > FinalScoreAway)) |
                                       ((AwayTeam == input$team) & (FinalScoreAway > FinalScoreHome)), 1, 0)) %>%
            summarise(
                wins = sum(TeamWin),
                total = n()
            ) %>%
            mutate(percent = (wins / total) * 100) %>%
            pull() %>%
            round() %>%
            paste0("%") %>%
            valueBox(
                subtitle = "Final Win Percentage",
                color = "teal"
            )
    })
    
    # Montly trend (server) -------------------------------------
    output$group_totals <- renderD3({
        grouped <- expr(month)
        
        res <- base_games() %>%
            mutate(TeamWin = ifelse(((HomeTeam == input$team) & (FinalScoreHome > FinalScoreAway)) |
                                        ((AwayTeam == input$team) & (FinalScoreAway > FinalScoreHome)), 1, 0)) %>%
            group_by(!!grouped) %>%
            summarise(wins = sum(TeamWin),
                      total = n(), .groups = 'drop') %>%
            mutate(percent = round(wins / total * 100)) %>%
            collect()
        # Append the missing months
        month_mis <- c("01","02","03")
        total_mis <- c(0,0,0)
        wins_mis <- c(0,0,0)
        percent_mis <- c(0,0,0)
        df_mis <- data.frame(month_mis, wins_mis, total_mis, percent_mis)
        setnames(df_mis, colnames(res))
        res <- rbind(df_mis, res)
        res[nrow(res) + 1,] = list("12", 0, 0, 0)
        res <- res %>%
            mutate(
                y = percent,
                x = as.integer(!!grouped)
            ) %>%
            select(x, y) %>%
            inner_join(
                tibble(x = 1:12, label = substr(month.name, 1, 3)),
                by = "x"
                )
        r2d3(res, "col_plot.js")
    })
    
    # Top Pitchers (server) -------------------------------------------
    output$top_pitchers <- renderD3({
        # The following code runs inside the database
        top_pitchers %>%
            mutate(win_percent = round((Wins / Games) * 100),2) %>%
            collect() %>%
            arrange(desc(Games), desc(win_percent)) %>%
            head(15) %>%
            rename(
                x = Pitcher,
                y = win_percent,
                label = Pitcher
            ) %>%
            r2d3("bar_plot.js")
    })
    
    # Get details (server) --------------------------------------------
    get_details <- function(team = NULL, mth = NULL) {
        # Create a generic details function that can be called
        # by different dashboard events
        res <- base_games() %>%
            unnest(cols = c(Lines)) %>% 
            group_by(GameID) %>% 
            arrange(desc(EnteredDateTimeUTC)) %>% 
            slice(1) %>% 
            ungroup()
        if (!is.null(team)) res <- filter(res, HomeTeam == team | AwayTeam == team)
        if (!is.null(mth)) res <- filter(res, as.integer(month) == as.integer(mth))
        
        res %>%
            head(200) %>%
            select(GameID, EventDateTimeET, HomeTeam, AwayTeam, 
                   HomeStartingPitcher, AwayStartingPitcher, 
                   FinalScoreHome, FinalScoreAway, TotalPoints
            ) %>%
            collect()
    }
    
    # Month column click (server) ---------------------------------
    observeEvent(input$column_clicked != "", {

            mth <- input$column_clicked
            print(mth)
            tab_title <- paste(
                input$team, "-", month.name[as.integer(mth)]
            )
            if (!(tab_title %in% tab_list)) {
                appendTab(
                    inputId = "tabs",
                    tabPanel(
                        tab_title,
                        DT::renderDataTable(
                            get_details(mth = mth)
                        )
                    )
                )
                tab_list <<- c(tab_list, tab_title)
            }
            updateTabsetPanel(session, "tabs", selected = tab_title)
    },
    ignoreInit = TRUE
    )
    
    # Remote tabs (server) --------------------------------------------
    observeEvent(input$remove, {
        # Use purrr's walk command to cycle through each
        # panel tabs and remove them
        tab_list %>%
            walk(~ removeTab("tabs", .x))
        tab_list <<- NULL
    })
    
    ## Summary Tab
    # Q2 (server) ------------------------------------------
    output$Q2 <- renderDataTable(
        top_pitchers %>% 
            arrange(desc(Games), desc(Wins/Games))
    )
    
    # Top Pitchers Q2a (server) -------------------------------------------
    output$Q2a <- renderD3({
        # The following code runs inside the database
        top_pitchers %>%
            mutate(win_percent = round((Wins / Games) * 100),2) %>%
            collect() %>%
            arrange(desc(Games), desc(win_percent)) %>%
            head(15) %>%
            rename(
                x = Pitcher,
                y = win_percent,
                label = Pitcher
            ) %>%
            r2d3("bar_plot.js")
    })
    
    # Q3 (server) ------------------------------------------
    output$Q3 <- renderDataTable(
        MoneyUS
    )
    
    # Q3a (server) ------------------------------------------
    output$model_summary <- renderPrint(
        summary(model)
    )
    
    output$discussion <- renderText(
        print("Now we can analyze the fitting and interpret what the model is telling us.
First of all, we can see that First_MoneyUS2, is not statistically significant.
As for the statistically significant variable, Last_MoneyUS2 has the lowest p-value suggesting 
a strong association of the Last_MoneyUS2 with the probability of Home team winning the game.
The positive coefficient for this predictor suggests that all other variables being equal, the Last_MOneyUS2 is more likely to win the game. 
Remember that in the logit model the response variable is log odds: ln(odds) = ln(p/(1-p)) = a*x1 + b*x2 + . + z*xn. 
A unit increase in Last_MoneyUS2 increases the log odds by 5.1822.")
    )
}

shinyApp(ui, server)
