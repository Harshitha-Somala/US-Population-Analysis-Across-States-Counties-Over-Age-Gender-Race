library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

county_df <- read.csv("data/Cleaned_df.csv")
county_df$FIPS_CODE <- sprintf("%05d", county_df$FIPS_CODE)
county_df <- county_df %>%
  mutate(STATEFP = substr(FIPS_CODE, 1, 2), COUNTYFP = substr(FIPS_CODE, 3, 5))
states <- unique(county_df$STATE)

# UI interface layout
ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Age-Gender", tabName = "Age-Gender"),
      menuItem("Race", tabName = "Race")
    ),
    selectInput("stateInput", label = "State", choices = states),
    selectInput("countyInput", label = "County", choices = NULL)
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Age-Gender",
        fluidRow(
          box(
            plotlyOutput("agePlot"),
            title = "Population Distribution over Age groups",
            width = 12
          ),
          box(
            title = "Population Distribution over Gender",
            width = 5,
            splitLayout(cellWidths = c("75%", "25%"),
                        plotlyOutput("genderPlot"),
                        radioButtons("ageCondition", label = "Select Age Condition:",
                                     choices = c("Total", "Above 18", "Above 65"),
                                     selected = "Total"))
          ),
          box(
            plotlyOutput("ageLimitPlot"),
            title = "Population Distribution over Age Limit groups",
            width = 7
          )
        )
      ),
      
      tabItem(
        tabName = "Race",
        fluidRow(
          box(
            title = "Total Population Distribution over Race",
            width= 12,
            plotlyOutput("racePlot")
          ),
          box(
            title = "Population Distribution over One-Race",
            width = 6,
            plotlyOutput("oneRacePlot")
          ),
          box(
            title = "Population Distribution Over Sub-Race (1 race)",
            width = 6,
            splitLayout(cellWidths = c("75%", "25%"),
                        plotlyOutput("subRace1"),
                        radioButtons("selectSubRace1", label = "Select Sub Race:",
                                     choices = c("Asian","AmericanIndian & AlaskaNative",
                                                 "NativeHawaiian & OtherPacificIslander"),
                                     selected = "Asian")) 
          ),
          box(
            title = "Population Distribution over Two or More Races",
            width = 6,
            plotlyOutput("twoMoreRaces")
          ),
          box(
            title = "Population Distribution over Race alone or in combination with 1 or more races",
            width = 6,
            plotlyOutput("oneormoreRaces")
          ),
          box(
            title = "Population Distribution over Hispanic or Latino Race",
            width = 12,
            plotlyOutput("hisLat")
          )
        )
      )
    )
  )
)

# Server Changes
server <- function(input, output, session) {
  
  # To update selectInput list of counties based on State selected
  observe({
    counties <- unique(county_df$COUNTY[county_df$STATE == input$stateInput])
    updateSelectInput(session, "countyInput", choices = counties)
  })
  
  # Plot the distributions
  # Population Distribution over Age groups
  output$agePlot <- renderPlotly({
    filter_data <- county_df[county_df$STATE == input$stateInput & county_df$COUNTY == input$countyInput,]
    x_age <- c("< 5", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-54", 
               "55-59", "60-64", "65-74", "75-84", "> 85")
    y_age <- unlist(filter_data[, 8:20], use.names = FALSE)

    plot_data <- data.frame(
      Age_Group = factor(x_age, levels = x_age),
      Count = as.integer(y_age)
    )

    plot_ly(data = plot_data, x = ~Age_Group, y = ~Count, type = 'bar',
            color = ~Age_Group) %>%
      layout(xaxis = list(title = "Age groups"),
             yaxis = list(title = "Count"),
             legend = list(title = list(text = "Age Groups")),
             hoverinfo = "text",
             showlegend = TRUE,
             text = ~paste("Age Group: ", Age_Group, "<br>Count: ", Count)
      )

  })
  
    # Population Distribution over Gender
    output$genderPlot <- renderPlotly({
      filter_data <- county_df[county_df$STATE == input$stateInput & county_df$COUNTY == input$countyInput,]

      # Update plot based on the selection
      if (input$ageCondition == "Total") {
        y_gender <- unlist(filter_data[, 5:6], use.names = FALSE)
        title <- "Population distribution over Gender (Total)"
      } else if (input$ageCondition == "Above 18") {
        y_gender <- unlist(filter_data[, 28:29], use.names = FALSE)
        title <- "Population distribution over Gender (Above 18)"
      } else if (input$ageCondition == "Above 65") {
        y_gender <- unlist(filter_data[, 31:32], use.names = FALSE)
        title <- "Population distribution over Gender (Above 65)"
      }

      gender_data <- data.frame(
        gender = factor(c("Male", "Female"), levels = c("Male", "Female")),
        Count = as.integer(y_gender)
      )

      plot_ly(data = gender_data, x = ~gender, y = ~Count, type = 'bar',
              color = ~gender) %>%
        layout(xaxis = list(title = "Gender"),
               yaxis = list(title = "Count"),
               hoverinfo = "text",
               text = ~paste("Gender: ", gender, "<br>Count: ", Count),
               title = title,
               showlegend = FALSE,
               bargap = 0.6
        )
    })

    # Population Distribution over Age Limit groups
    output$ageLimitPlot <- renderPlotly({
      age_limit_data <- county_df[county_df$STATE == input$stateInput & county_df$COUNTY == input$countyInput,]
      y_age_limit <- unlist(age_limit_data[,23:27], use.names = FALSE)
      x_age_limit <- c(">16 years", ">18 years", ">21 years",">62 years",">65 years")

      age_limit_data <- data.frame(age_limit = factor(x_age_limit, levels = x_age_limit),
                                   Count = as.integer(y_age_limit))

      plot_ly(data = age_limit_data, x = ~age_limit, y = ~Count, type = 'bar',
              color = ~age_limit) %>%
        layout(xaxis = list(title = "Age Limit"),
               yaxis = list(title = "Count"),
               legend = list(title = list(text = "Age Limit")),
               hoverinfo = "text",
               text = ~paste("Age Limit: ", age_limit, "<br>Count: ", Count),
               showlegend = TRUE,
               bargap = 0.5)
    })
    
    # Total Population Distribution over Race
    output$racePlot <- renderPlotly({
      race_data <- county_df[county_df$STATE == input$stateInput & county_df$COUNTY == input$countyInput,]
      y_race <- unlist(race_data[,c(35:36,62,69)], use.names = FALSE)
      x_race <- c("One Race", "Two or More Races", "Race Alone/Comb of 1-more", "Hispanic/Latino")
      
      race_data <- data.frame(race = factor(x_race, levels = x_race),
                                   Count = as.integer(y_race))
      
      plot_ly(data = race_data, x = ~race, y = ~Count, type = 'bar',
              color = ~race) %>%
        layout(xaxis = list(title = "Race"),
               yaxis = list(title = "Count"),
               legend = list(title = list(text = "Race")),
               hoverinfo = "text",
               text = ~paste("Race: ", race, "<br>Count: ", Count),
               showlegend = TRUE)
    })
    
    # Population Distribution over One-Race
    output$oneRacePlot <- renderPlotly({
      race1_data <- county_df[county_df$STATE == input$stateInput & county_df$COUNTY == input$countyInput,]
      y_race1 <- unlist(race1_data[,c(37:39,44,52, 57)], use.names = FALSE)
      x_race1 <- c("White", "Black/AfricanAmerican", "AmericanIndian & AlaskaNative",
                  "Asian","NativeHawaiian & OtherPacificIslander","SomeOtherRaces")
      
      race1_data <- data.frame(race1 = factor(x_race1, levels = x_race1),
                              Count = as.integer(y_race1))
      
      plot_ly(data = race1_data, x = ~race1, y = ~Count, type = 'bar',
              color = ~race1) %>%
        layout(xaxis = list(title = "One Race Categories"),
               yaxis = list(title = "Count"),
               legend = list(title = list(text = "One Race")),
               hoverinfo = "text",
               text = ~paste("Race: ", race1, "<br>Count: ", Count),
               showlegend = TRUE)
    })
    
    # Population Distribution Over Sub-Race (1 race)
    output$subRace1 <- renderPlotly({
      racesub_data <- county_df[county_df$STATE == input$stateInput & county_df$COUNTY == input$countyInput,]
      y_racesub <- unlist(racesub_data[,c(37:39,44,52, 57)], use.names = FALSE)
      
      if (input$selectSubRace1 == "Asian") {
        x_racesub <- c("AsianIndian", "Chinese", "Philipino", "Japanese", "Korean", "Vietnamese",
                       "Other Asian")
        y_racesub <- unlist(racesub_data[, 45:51], use.names = FALSE)
        title <- "Population distribution - Asian"
      } else if (input$selectSubRace1 == "AmericanIndian & AlaskaNative") {
        x_racesub <- c("Cherokee", "Chippewa", "Navoja", "Sioux")
        y_racesub <- unlist(racesub_data[, 40:43], use.names = FALSE)
        title <- "Population distribution - AmericanIndian & AlaskaNative "
      } else if (input$selectSubRace1 == "NativeHawaiian & OtherPacificIslander") {
        x_racesub <- c("Native Hawaiian", "Guamanian/Chomorro", "Samoan",
                       "Other Pacific Islander")
        y_racesub <- unlist(racesub_data[, 53:56], use.names = FALSE)
        title <- "Population distribution - NativeHawaiian & OtherPacificIslander"
      }
      
      racesub_data <- data.frame(racesub = factor(x_racesub, levels = x_racesub),
                              Count = as.integer(y_racesub))
      
      plot_ly(data = racesub_data, x = ~racesub, y = ~Count, type = 'bar',
              color = ~racesub) %>%
        layout(xaxis = list(title = "One Race Categories"),
               yaxis = list(title = "Count"),
               hoverinfo = "text",
               text = ~paste("Race: ", racesub, "<br>Count: ", Count),
               showlegend = FALSE)
    })
    
    # Population Distribution over Two or More Races
    output$twoMoreRaces <- renderPlotly({
      race2_data <- county_df[county_df$STATE == input$stateInput & county_df$COUNTY == input$countyInput,]
      y_race2 <- unlist(race2_data[,58:61], use.names = FALSE)
      x_race2 <- c("White & Black/AfricanAmerican", "White & AmericanIndian & AlaskaNative",
                   "White & Asian","Black/AfricanAmerican & AmericanIndian & AlaskaNative")
      
      race2_data <- data.frame(race2 = factor(x_race2, levels = x_race2),
                              Count = as.integer(y_race2))
      
      plot_ly(data = race2_data, x = ~race2, y = ~Count, type = 'bar',
              color = ~race2) %>%
        layout(xaxis = list(title = "Two or More Races"),
               yaxis = list(title = "Count"),
               legend = list(title = list(text = "Two or More Races")),
               hoverinfo = "text",
               text = ~paste("Race: ", race2, "<br>Count: ", Count),
               showlegend = TRUE)
    })
    
    # Population Distribution over Race alone or in combination with 1 or more races
    output$oneormoreRaces <- renderPlotly({
      racemore_data <- county_df[county_df$STATE == input$stateInput & county_df$COUNTY == input$countyInput,]
      y_racemore <- unlist(racemore_data[,63:68], use.names = FALSE)
      x_racemore <- c("White" ,"Black/AfricanAmerican", "AmericanIndian & AlaskaNative",
                   "Asian","NativeHawaiian & OtherPacificIslander", "SomeOtherRaces")
      
      racemore_data <- data.frame(racemore = factor(x_racemore, levels = x_racemore),
                               Count = as.integer(y_racemore))
      
      plot_ly(data = racemore_data, x = ~racemore, y = ~Count, type = 'bar',
              color = ~racemore) %>%
        layout(xaxis = list(title = "1 or More Races"),
               yaxis = list(title = "Count"),
               legend = list(title = list(text = "Two or More Races")),
               hoverinfo = "text",
               text = ~paste("Race: ", racemore, "<br>Count: ", Count),
               showlegend = TRUE)
    })
    
    # Population Distribution over Hispanic or Latino Race
    output$hisLat <- renderPlotly({
      racehl_data <- county_df[county_df$STATE == input$stateInput & county_df$COUNTY == input$countyInput,]
      y_racehl <- unlist(racehl_data[, 70:74], use.names = FALSE)
      x_racehl <- c("Hispanic/LatinoOfAnyRace", "Mexican", "PuertoRican", "Cuban", 
                    "OtherHispanic/Latino")
      
      racehl_data <- data.frame(racehl = factor(x_racehl, levels = x_racehl),
                               Count = as.integer(y_racehl))
      
      plot_ly(data = racehl_data, x = ~racehl, y = ~Count, type = 'bar',
              color = ~racehl) %>%
        layout(xaxis = list(title = "Hispanic/Latino"),
               yaxis = list(title = "Count"),
               legend = list(title = list(text = "Hispanic/Latino")),
               showlegend = TRUE)
    })
    
}

# Call to ShinyAPP
shinyApp(ui, server)

