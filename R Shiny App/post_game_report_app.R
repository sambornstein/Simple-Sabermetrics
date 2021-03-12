# Simple Sabermetrics #

#install.packages("shiny")
library(shiny)

#install.packages("dplyr")
library(dplyr)

#install.packages("DT")
library(DT)

#install.packages("ggplot2")
library(ggplot2)

# https://sambornstein.shinyapps.io/Simple_Sabermetrics_Post_Game_Report/

NL_CY <- read.csv("NL_CY.csv")
NL_CY$game_date <- as.Date(NL_CY$game_date, "%Y-%m-%d")

NL_CY$pitch_type <- factor(NL_CY$pitch_type, levels = c("FF", "SI", "FC", "CS", "CU", "KC", "SL", "CH", "FS"))
NL_CY$pitch_name <- factor(NL_CY$pitch_name, levels = c("4-Seam Fastball", "Sinker", "Cutter", "Curveball", "Knuckle Curve", "Slider", "Changeup", "Split-Finger"))

ui <- fluidPage(
    column(10, offset = 1,
           titlePanel("Simple Sabermetrics: Statcast Shiny Application - Post-Game Report"),
           hr(style="border-color: black;"), 
           fluidRow(
               column(2, selectInput(inputId = "PitcherInput", label = "Select Pitcher", choices = sort(unique(NL_CY$player_name)))),
               column(2, selectInput(inputId = "GameInput", label = "Select Game", choices = ""))
           ),
           hr(style="border-color: black;"),
           wellPanel(style = "background: white; border-color:black; border-width:2px",
                     fluidRow(
                         column(2, img(src = "ss_logo.png", height = 150, width = 150), align = "center"), 
                         column(4, h2(strong(textOutput("selected_pitcher"))), hr(style="border-color: black;"), style = "padding-right:0px;"),
                         column(6, h2("Post-Game Report"), hr(style="border-color: black;"), h2(textOutput("selected_game")), align = "right", style = "padding-left:0px;")),
                     hr(style="border-color: black;"), 
                     fluidRow(
                         column(10, offset = 1, h3(strong("Pitcher Summary Table")), dataTableOutput("pitcher_summary_table"), align = "center")
                     ), br(), br(), 
                     fluidRow(
                         column(4, plotOutput("pitch_movement_plot"), align = "center"),
                         column(4, plotOutput("pitch_location_plot"), align = "center"),
                         column(4, plotOutput("pitch_velocity_plot"), align = "center")
                     ), br(), br(), br()
           ), br(),
           p(em("If the contents of this page appear distorted, please decrease your web browser zoom to 80% or 90%."), align = "center")
    )
)


server <- function(input, output, session) {
    
    observeEvent(input$PitcherInput, 
                 updateSelectInput(session, inputId = "GameInput", label = "Select Game", 
                                   choices = sort(unique(NL_CY$game_date[NL_CY$player_name == input$PitcherInput]))))
    
    output$selected_pitcher <- renderText({paste(input$PitcherInput)})
    
    output$selected_game <- renderText({paste(input$GameInput)})
    
    output$pitcher_summary_table <- renderDataTable({
        table <- NL_CY %>%
            filter(player_name == input$PitcherInput, game_date == input$GameInput) %>%
            group_by('Pitch' = pitch_name) %>%
            summarize('No.' = n(),
                      'Max Velo (MPH)' = round(max(release_speed, na.rm = TRUE),1),
                      'Avg. Velo (MPH)' = round(mean(release_speed, na.rm = TRUE),1),
                      'Avg. Spin (RPM)' = round(mean(release_spin_rate, na.rm = TRUE),0),
                      'Strike %' = round(sum(type %in% c("S", "X"))/n(),3)*100,
                      'Whiff %' = round(sum(description %in% c("swinging_strike", "swinging_strike_blocked"))/
                                            sum(description %in% c("swinging_strike", "foul", "bunt_foul_tip", "foul_bunt", 
                                                                   "foul_tip", "hit_into_play", "hit_into_play_no_out", 
                                                                   "hit_into_play_score", "missed_bunt", "swinging_strike_blocked")),3)*100)
        tableFilter <- reactive({table})
        datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
            formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
    })
    
    
    output$pitch_movement_plot <- renderPlot({
        dataFilter <- reactive({
            NL_CY %>%
                filter(player_name == input$PitcherInput, game_date == input$GameInput)
        })
        ggplot(data = dataFilter(), aes(x = pfx_x*-1, y = pfx_z, color = pitch_type)) +
            labs(x = "Horizontal Movement (ft.)", y = "Vertical Movement (ft.)", color = " ", title = "Pitch Movement") + 
            xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
            geom_segment(aes(x = 0, y = -2.5, xend = 0, yend = 2.5), size = 1, color = "grey55") + 
            geom_segment(aes(x = -2.5, y = 0, xend = 2.5, yend = 0), size = 1, color = "grey55") +
            geom_point(size = 3, na.rm = TRUE) +
            theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
            theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
    }, width = 450, height = 450)
    
    
    output$pitch_location_plot <- renderPlot({
        dataFilter <- reactive({
            NL_CY %>%
                filter(player_name == input$PitcherInput, game_date == input$GameInput)
        })
        ggplot(data = dataFilter(), aes(x = plate_x*-1, y = plate_z, color = pitch_type)) +
            xlim(-3,3) + ylim(0,5) + labs(color = "", title = "Pitch Location") +
            geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
            geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
            geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
            geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
            geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
            geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
            geom_point(size = 3, na.rm = TRUE) +
            theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
            theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
    }, width = 350, height = 450)
    
    
    output$pitch_velocity_plot <- renderPlot({
        dataFilter <- reactive({
            NL_CY %>%
                filter(player_name == input$PitcherInput, game_date == input$GameInput) %>%
                group_by(pitch_type) %>%
                mutate(PitchNo = row_number())
        })
        ggplot(data = dataFilter()) + 
            geom_line(aes(y = release_speed, x = PitchNo, color = pitch_type), size = 2) + 
            labs(x = "Pitch Count", y = "Pitch Velocity (MPH)", color = " ", title = "Pitch Velocity") + 
            theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 12)) +
            theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
    }, width = 450, height = 450)
    
    
}

shinyApp(ui = ui, server = server)
