# Simple Sabermetrics #

#install.packages("shiny")
library(shiny)

#install.packages("dplyr")
library(dplyr)

#install.packages("ggplot2")
library(ggplot2)

# statcast_data <- read.csv("mlb_2020_statcast_pitcher.csv")
# NL_CY <- statcast_data %>% 
#     filter(player_name %in% c("Trevor Bauer", "Jacob deGrom", "Yu Darvish"))

# https://sambornstein.shinyapps.io/Simple_Sabermetrics_Statcast_App/

NL_CY <- read.csv("NL_CY.csv")
NL_CY$game_date <- as.Date(NL_CY$game_date)

ui <- fluidPage(

    titlePanel("Simple Sabermetrics: Statcast Shiny Application"),
    
    br(),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "PitcherInput", label = "Select Pitcher", choices = sort(unique(NL_CY$player_name))),
            dateRangeInput(inputId = "DateRangeInput", label = "Select Date Range", start = min(NL_CY$game_date), end = max(NL_CY$game_date)),
            img(src = "ss_logo.png", style = "display: block; margin-left: auto; margin-right: auto;", height = 150, width = 150)
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Pitch Usage - Bar Chart", br(), plotOutput("barchart")),
                tabPanel("Pitch Velocity - Box Plot", br(), plotOutput("boxplot")),
                tabPanel("Pitch Velocity Trend - Line Plot", br(), plotOutput("lineplot"))
            )
        )
    )
)


server <- function(input, output) {

    output$barchart <- renderPlot({
        dataFilter <- reactive({
            NL_CY %>% 
                filter(player_name == input$PitcherInput,
                       between(game_date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
                group_by(pitch_name) %>%
                summarize('count' = n())
        })
        ggplot(dataFilter(), aes(x = reorder(pitch_name, -count), y = count, fill = pitch_name)) + 
            geom_bar(stat = "identity") +
            labs(x = "Pitch Type", y = "Count", title = "Pitch Usage") +
            theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
            theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
    }, width = 850, height = 450)
    
    
    output$boxplot <- renderPlot({
        dataFilter <- reactive({
            NL_CY %>%
                filter(player_name == input$PitcherInput,
                       between(game_date, input$DateRangeInput[1], input$DateRangeInput[2]))
        })
        ggplot(dataFilter(), aes(x = reorder(pitch_name, -release_speed), y = release_speed, fill = pitch_name)) +
            geom_boxplot(width = 0.5) + 
            labs(x = "Pitch Type", y = "Velocity (MPH)", title = "Distribution of Pitch Velocity") +
            theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
            theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
    }, width = 850, height = 450)
    
    
    output$lineplot <- renderPlot({
        dataFilter <- reactive({
            NL_CY %>%
                filter(player_name == input$PitcherInput,
                       between(game_date, input$DateRangeInput[1], input$DateRangeInput[2]),
                       pitch_type == "FF") %>%
                group_by(player_name, pitch_type, game_date) %>%
                summarize('mean_release_speed' = mean(release_speed, na.rm = TRUE))
        })
        ggplot(dataFilter(), aes(x = game_date, y = mean_release_speed, group = player_name, color = player_name)) + 
            geom_line(size = 1) + geom_point(size = 3) +
            labs(x = "Game Date", y = "Velocity (MPH)", title = "Average FB Velocity by Game", color = "") +
            theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
            theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
    }, width = 850, height = 450)

}

shinyApp(ui = ui, server = server)
