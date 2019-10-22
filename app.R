#
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html
#
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(lubridate)
library(tpltheme)
library(ggtext)
library(ggalt)

shinyApp(
    ui = tagList(
        navbarPage(
            "Design Contest",
            theme = shinytheme("cosmo"),
            tabPanel("Interactive Dumbell Plot",
                     sidebarPanel(
                         h4("Select the Year to see the data"),
                         sliderInput("year1", "Year :", min=1980, max=2019, value=1980, 
                                     animate =
                                         animationOptions(interval=1000,loop=TRUE))
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Dumbell Plot",
                                      plotOutput("dumbellPlot")
                             ),
                             tabPanel("Data",
                                      h4("Please find the data here"),
                                      DT::dataTableOutput("table1")
                                      )
                         )
                     )
            ),
            tabPanel("Visual 2",
                     sidebarPanel(
                         textInput("txt", "Text input for Visual 2:", "general"),
                         sliderInput("slider", "Slider input:", 1, 100, 30),
                         tags$h5("Default actionButton:"),
                         actionButton("action", "Search"),
                         
                         tags$h5("actionButton with CSS class:"),
                         actionButton("action2", "Action button", class = "btn-primary")
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Plot 2",
                                      h4("We will put the plot here")
                             ),
                             tabPanel("Data",
                                      h4("Data here for Visual 2"),
                                      tableOutput("table2")
                             )
                         )
                     )
            ),
            tabPanel("Visual 3", 
                     sidebarPanel(
                         textInput("txt", "Text input for Visual 3:", "general"),
                         sliderInput("slider", "Slider input:", 1, 100, 30),
                         tags$h5("Default actionButton:"),
                         actionButton("action", "Search"),
                         
                         tags$h5("actionButton with CSS class:"),
                         actionButton("action2", "Action button", class = "btn-primary")
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Plot 3",
                                      h4("We will put the plot here")
                             ),
                             tabPanel("Data",
                                      h4("Data here for Visual 3"),
                                      tableOutput("table3")
                             )
                         )
                     )
                )
        )
    ),
    
    server = function(input, output) {
        
        ipf_lifts <- read_csv("data/ipf_lifts.csv")
        ipf_lifts1 <- ipf_lifts %>% 
            mutate(year = lubridate::year(date))
        ipf_lifts_reshape <- ipf_lifts1 %>% 
            tidyr::pivot_longer(cols = c("best3squat_kg", "best3bench_kg", "best3deadlift_kg"), names_to = "lift") %>% 
            select(name, sex, year, lift, value)
        ipf_lifts_maxes <- ipf_lifts_reshape %>% 
            group_by(year, sex, lift) %>% 
            top_n(1, value) %>% 
            ungroup %>% 
            distinct(year, lift, value, .keep_all = TRUE)
        max_pivot <- ipf_lifts_maxes %>% 
            spread(sex, value)
        male_lifts <- max_pivot %>% 
            select(-name) %>% 
            filter(!is.na(M)) %>% 
            group_by(year, lift) %>% 
            summarise(male = mean(M))
        
        female_lifts <- max_pivot %>% 
            select(-name) %>% 
            filter(!is.na(`F`)) %>% 
            group_by(year, lift) %>% 
            summarise(female = mean(`F`))
        max_lifts <- merge(male_lifts, female_lifts)
        max_lifts_final <- max_lifts %>% 
            group_by(year, lift) %>% 
            mutate(diff = male - female)
        max_lift_final_year <- reactive({filter(max_lifts_final,year==input$year1)})
        
        output$dumbellPlot <- renderPlot({
            ggplot(max_lift_final_year()) + 
                ggalt::geom_dumbbell(aes(y = lift,
                                  x = female, xend = male),
                              colour = "grey", size = 5,
                              colour_x = "#D6604C", colour_xend = "#395B74") +
                labs(y = element_blank(),
                     x = "Top Lift Recorded (kg)",
                     title =  "How <span style='color:#D6604C'>Women</span> and <span style='color:#395B74'>Men</span> Differ in Top Lifts") +
                theme(plot.title = element_markdown(lineheight = 1.1, size = 20),
                      plot.subtitle = element_text(size = 15)) +
                scale_y_discrete(labels = c("Bench", "Deadlift", "Squat")) +
                drop_axis(axis = "y") +
                geom_text(aes(x = female, y = lift, label = paste(female, "kg")),
                          color = "#D6604C", size = 4, vjust = -2) +
                geom_text(aes(x = male, y = lift, label = paste(male, "kg")),
                          color = "#395B74", size = 4, vjust = -2) +
                geom_rect(aes(xmin=430, xmax=470, ymin=-Inf, ymax=Inf), fill="grey80") +
                geom_text(aes(label=diff, y=lift, x=450), fontface="bold", size=4) +
                geom_text(aes(x=450, y=3, label="Difference"),
                          color="grey20", size=4, vjust=-3, fontface="bold")
        })
        output$table1 <- DT::renderDataTable(DT::datatable(max_lifts_final))
            
        output$table2 <- renderTable({
            head(cars, 8)
        })
        output$table3 <- renderTable({
            head(cars, 10)
        })
    }
)