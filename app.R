#
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html
#
library(shiny)
library(shinythemes)

shinyApp(
    ui = tagList(
        navbarPage(
            "Design Contest",
            theme = shinytheme("superhero"),
            tabPanel("Visual 1",
                     sidebarPanel(
                         textInput("txt", "Text input for Visual 1:", "general"),
                         sliderInput("slider", "Slider input:", 1, 100, 30),
                         tags$h5("Default actionButton:"),
                         actionButton("action", "Search"),
                         
                         tags$h5("actionButton with CSS class:"),
                         actionButton("action2", "Action button", class = "btn-primary")
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Plot 1",
                                      h4("We will put the plot here"),
                                      verbatimTextOutput("txtout")
                             ),
                             tabPanel("Data",
                                      h4("Data here for Visual 1"),
                                      tableOutput("table1")
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
        output$txtout <- renderText({
            paste(input$txt, input$slider, format(input$date), sep = ", ")
        })
        output$table1 <- renderTable({
            head(cars, 4)
        })
        output$table2 <- renderTable({
            head(cars, 8)
        })
        output$table3 <- renderTable({
            head(cars, 10)
        })
    }
)