library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
print(str(bcl))

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(h3("our inputs will go here"),
                 sliderInput("priceInput","select $$Price$$ at your peril",
                             min = 0,
                             max = 1000, 
                             value = c(10,200), 
                             pre = "$",
                             ),
                 checkboxGroupInput("typeInput", 
                              "Product type",
                              choices = c("WINE", "BEER", "SPIRITS", "REFRESHMENTS"),
                              selected = "WINE",
                              ),
                 selectInput("countryInput", "Country", 
                             choices = c("CANADA", "FRANCE", "ITALY"),
                             multiple = TRUE,
                             selected = "FRANCE"
                             )
                 ),
    mainPanel(plotOutput("coolplot"),
              br(), 
              h2(textOutput("resnum")),
              br(),
              tableOutput("results")
              )
  ),
  actionButton("button1","do not click this"),
  selectInput("select1","Select at own risk", c(
    "this one",
    "don't ever select this one",
    "maybe this one",
    "or maybe this one")
    )


  
)
  
server <- function(input, output) {
  output$coolplot <- renderPlot({
    filtered <- 
      bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
             )
    ggplot(filtered, aes(x=Price, color = Type)) + 
      geom_histogram() +
      geom_point(aes(y=Alcohol_Content)) + 
      theme_test()
  })
  output$resnum <- renderText("100 results were found.")
  output$results <- renderTable({
    filtered <- 
      bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
    filtered
  })
}



shinyApp(ui = ui, server = server)