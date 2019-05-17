# DATA 608 Final ----
# Fast Break: Academics, Athletics, and the Journey to the NBA ----
# Brian K. Liles ----

# load libraries ----
library(shiny)
library(dplyr)
library(DT)
library(shinythemes)
library(ggplot2)
library(rsconnect)
rsconnect::deployApp('path/to/your/app')

# load the NBA dataset ----
nba <- read.csv("C:\\Users\\lizza\\Desktop\\NBA App\\nba.csv", encoding = "UTF-8")

# define the User Interface ----
ui <- fluidPage(theme = shinytheme("united"),
  titlePanel("Fast Break: Academics, Athletics, and the Journey to the NBA"),
# sidebar layout ----
  sidebarLayout(
    sidebarPanel(
      h2("Glory Road"),
      br(),
      strong("From the radio buttons below, please select a position"),
      br(),br(),
      radioButtons("posInput", "Position",
                   choices = c("C","C-F","F","F-C","F-G","G","G-F"),
                   selected = "G"),
      strong("From the drop down list below, please select a college"),
      br(),br(),
      uiOutput("collegeOutput"),
      h2("Basketball Diaries"),
      p("Founded in 1946, the National Basketball Association(NBA) is one of the most popular sports worldwide."),
      br(),
      p("Consisting of 30 teams, the NBA has created household names such as Michael Jordan, Kobe Bryant, and LeBron James. Marketing firms have attached sneakers, clothing, and even toothpaste with players from this illustrious league which has broadened their reach."),
      br(),
      p("Athletes have also gone from the blacktop to boardroom. NBA players have gone on to sell everything from sneakers, clothing, colognes and toothpaste. Not only do the players cash in, the colleges they attend also receive huge paydays from television giants such as ESPN."),
      br(),
      p("With this app, the features of RStudio and its shiny package will allow users to see which colleges have produced the most talent in the NBA, how long they were in the league, and which position did they flourish in."),
      br(),
      img(src = "basketball.png", height = 100, width = 100),
      br(),
      "Baseketball image provided by", 
      a("UI-EX", 
        href = "https://ui-ex.com/explore/basketball-transparent-clear-background/")
    ),
    
# main panel ----

    mainPanel(
      h1("He Got Game"),
      p("Below we will find a scatterplot which showcases the height & weight of the chosen
        player's position and college."),
      strong("Data was collected from 1947 through 2018."),
      br(),
      br(),
      plotOutput("htWtplot"),
      h1("Coach Carter"),
      p("Below we will take a look at how long a player was able to sustain a career in the NBA."),
      plotOutput("years"),
      h1("The 6th Man"),
      p("Below we will take a look at descriptive statistics on total years in the league, height, and weight"),
      verbatimTextOutput("summary"),
      h1("Above the Rim"),
      p("Below we will find an interactive table which lists the player's that 
        were plotted in the scatterplot above."),
      br(),
      DT::dataTableOutput("outcomes")
      
    )
  )
)

# define the server ----
server <- function(input, output){
  output$collegeOutput <- renderUI({
    selectInput("collegeInput", "Colleges",
                sort(unique(nba$college)),
                selected = "Duke University")
  })
  
  filtered <- reactive({
      if(is.null(input$collegeInput)){
          return(NULL)
      }
      
  nba %>%
      dplyr::filter(position == input$posInput,
             college == input$collegeInput)
})
  
  filtered2 <- reactive({
      if(is.null(input$collegeInput)){
          return(NULL)
      }
      
      nba %>%
          dplyr::select(years_tot,height,weight)
  })

  output$htWtplot <- renderPlot({
      if (is.null(filtered())){
          return()
      }
      ggplot(filtered(), aes(x = height, y = weight)) +
          geom_point(color = "darkblue") +
          ggtitle("Scatterplot of Height & Weight") +
          xlab("Height (Inches)") + ylab("Weight (Pounds)") +
          theme(
              plot.title = element_text(hjust = 0.5,
                                        color="darkgreen",
                                        size=18,
                                        face="bold.italic"),
              axis.title.x = element_text(color="navyblue",
                                          size=14,
                                          face="bold",
                                          vjust = 0.5),
              axis.title.y = element_text(color="navyblue",
                                          size=14,
                                          face="bold",
                                          hjust = 0.5)
          )
      
  })
  output$years <- renderPlot({
      if (is.null(filtered())){
          return()
      }
      ggplot(filtered(), aes(x = years_tot)) +
          geom_density() +
          ggtitle("Density Plot of Years in the League") +
          xlab("Years in Total") +
          theme(
              plot.title = element_text(hjust = 0.5,
                                        color="darkgreen",
                                        size=18,
                                        face="bold.italic"))
      
  })
  
  output$summary <- renderPrint({
      summary(filtered2())
  })
      
  
  output$outcomes <- DT::renderDataTable({
      filtered()
})
  
  
}


# run the shinyApp ----
shinyApp(ui = ui, server = server)


