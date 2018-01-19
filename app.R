library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(tidyr)
library(DT)
library(shinyBS)

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
, stringsAsFactors = FALSE)

default <- data.frame(QID = NA, Category = NA, Question = "Hailing from New Zealand, this former professor and current RStudio employee received his Doctorate from Iowa State University with a dissertation describing practical tools for R programmers, such as reshape2 and ggplot2.", Answer = "Who is \"Hadley Wickham\"?")
default <- default[rep(seq_len(nrow(default)), each = 30),]

ui <- fluidPage(theme = shinytheme("cerulean"),
                
   includeCSS("css/styles.css"),
   
   titlePanel("Data Science Jeopardy"),
   
   sidebarLayout(
      sidebarPanel(
          a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
          
          h4("Question"),
          textOutput("question"),
          conditionalPanel(condition = "input.board_cells_selected.length > 0",
                           h4("Answer"),
                           helpText("Type your answer here. Remember to answer in the form of a question!"),
                           textInput("answer", "Answer"),
                           actionButton("submit", "Submit")
          )
      ),
      
      mainPanel(
          bsModal(id = 'correctModal', title = 'Correct Answer :)', trigger = '',
                  size = 'medium', HTML("That is Correct!")),
          
          bsModal(id = 'incorrectModal', title = 'Incorrect Answer :(', trigger = '',
                  size = 'medium', HTML("That is Incorrect!")),
          
          h3("Game Board"),
          
          div(DT::dataTableOutput("board"), style = "font-size: 125%; text-align: center"),
          
          hr(),
          
          h3("Score"),
          div(textOutput("score"), style = "font-size: 200%")
          #DT::dataTableOutput("game")
      )
   )
)

server <- function(input, output, session) {
    
    values <- reactiveValues(score = 0, complete = NULL, selected = NULL)
    
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
        names(board) <- paste0("<span style=\"color: yellow; background-color: #b0bed9\">", categories(), "</span>")

        if (!is.null(values$complete)) {
            sapply(values$complete, function(val) {
                mycol <- ceiling(val / 5)
                
                myrow <- val %% 5
                if (myrow == 0) myrow <- 5
                
                board[myrow, mycol] <<- NA
            })
        }

        mydt <- datatable(board, 
                          options = list(pageLength = 5,
                                            paginate = FALSE,
                                            searching = FALSE,
                                            ordering = FALSE,
                                            info = FALSE),
                          selection = list(target = "cell", mode = "single"),
                          escape = FALSE,
                          rownames = FALSE) %>%
            formatStyle(names(board), color = "yellow", backgroundColor = "blue")
        
        return(mydt)
    })
    
    output$board <- DT::renderDataTable(
        board_data()
    )
    
    proxy = dataTableProxy("board")
    
    output$game <- DT::renderDataTable(
        game_data()
    )
    
    output$question <- renderText({
        if (length(input$board_cells_selected) == 0) {
            return("Please select a category and dollar value by clicking the cell in the table!")
        } else {
            selec <- input$board_cells_selected
            
            isolate({
                if (!is.null(values$selected)) {
                    proxy %>% selectCells(values$selected)
                } else {
                    values$selected <- selec
                }
                
                gameind <- 5 * values$selected[1,2] + values$selected[1,1]
                
                if (gameind %in% values$complete) {
                    values$selected <- NULL
                    proxy %>% selectCells(NULL)
                    return("Please select a category and dollar value by clicking the cell in the table!")
                }
                
                return(game_data()$Question[gameind])
            })
        }
    })
    
    output$score <- renderText({
        return(paste0("$", values$score))
    })
    
    observeEvent(input$submit, {
        values$selected <- NULL
        selec <- input$board_cells_selected
        
        answer <- gsub("[[:punct:]]", "", tolower(input$answer))
        correct <- gsub("[[:punct:]]", "", tolower(game_data()$Answer[5 * selec[1,2] + selec[1,1]]))
        
        values$score <- values$score + 100 * selec[1,1] * ifelse(answer == correct, 1, -1)
        
        if (answer == correct) {
            toggleModal(session, "correctModal", toggle = "open")
        } else {
            toggleModal(session, "incorrectModal", toggle = "open")
        }
        
        values$complete <- c(values$complete, 5 * selec[1,2] + selec[1,1])
        
        updateTextInput(session, "answer", value = "")
    })

}

shinyApp(ui = ui, server = server)
