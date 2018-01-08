library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(tidyr)
library(DT)

## Set images resource path
addResourcePath("images", "images")

## Load the jeopardy questions
jeopardy <- read_csv("data/datasciencejeopardy.csv")

## Create the board
board <- data.frame(
    C1 = paste0("$", seq(100, 500, by = 100)),
    C2 = paste0("$", seq(100, 500, by = 100)),
    C3 = paste0("$", seq(100, 500, by = 100)),
    C4 = paste0("$", seq(100, 500, by = 100)),
    C5 = paste0("$", seq(100, 500, by = 100)),
    C6 = paste0("$", seq(100, 500, by = 100))
)

default <- data.frame(QID = NA, Category = NA, Question = "This guy wrote ggplot2", Answer = "Who is \"Hadley Wickham\"?")
default <- default[rep(seq_len(nrow(default)), each = 30),]

ui <- fluidPage(theme = shinytheme("cerulean"),
   
   titlePanel("Data Science Jeopardy"),
   
   sidebarLayout(
      sidebarPanel(
          a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
          
          h4("Question"),
          textOutput("question"),
          conditionalPanel(condition = "input.board_cells_selected.length > 0",
                           h4("Answer"),
                           textInput("answer", "Answer")
          )
      ),
      
      mainPanel(
          DT::dataTableOutput("board")
          #DT::dataTableOutput("game")
      )
   )
)

server <- function(input, output) {
    
    categories <- reactive({
        sample(unique(jeopardy$Category), 6)
    })
    
    game_data <- reactive({
        mydefault <- default
        mycategory <- categories()
        
        mydefault$Category <- rep(mycategory, each = 5)
        
        jeopardy %>%
            rbind(mydefault) %>%
            filter(Category %in% mycategory) %>%
            group_by(Category) %>%
            slice(1:5) %>%
            sample_n(size = 5) %>%
            ungroup() %>%
            mutate(Category = factor(Category, levels = mycategory)) %>%
            arrange(Category)
    })
    
    board_data <- reactive({
        names(board) <- categories()
        
        return(board)
    })
    
    output$board <- DT::renderDataTable(
        board_data(), selection = list(target = "cell", mode = "single"), options = list(
            pageLength = 5,
            paginate = FALSE,
            searching = FALSE,
            ordering = FALSE,
            info = FALSE
        ), rownames = FALSE
    )
    
    output$game <- DT::renderDataTable(
        game_data()
    )
    
    output$question <- renderText({
        if (length(input$board_cells_selected) == 0) {
            return("Please select a category and dollar value by clicking the cell in the table!")
        } else {
            selec <- input$board_cells_selected
            
            #cat("Column is\n", selec[1,2])
            #cat("Row is\n", selec[1,1])
            
            return(game_data()$Question[5 * selec[1,2] + selec[1,1]])
        }
    })

}

shinyApp(ui = ui, server = server)
