library(shiny)
library(DT)
library(dplyr)
library(tidyr)

dat <- read.csv("moveset_table.csv")
dat2 <- dat

dat2$Level <- dplyr::recode(dat2$Level, 
                      `Starting Moves` = "1",
                      `Level 10` = "10",
                      `Level 14` = "14",
                      `Level 17` = "17",
                      `Level 18` = "18",
                      `Level 2` = "2",
                      `Level 6` = "6")

dat2$Level <- as.numeric(as.character(dat2$Level))

# Define UI 
ui <- navbarPage(title = "Generate Pokemon", id = "tabber", 
   tabPanel("Build",
   sidebarLayout(
      sidebarPanel(
        selectInput("wpokemon", "What Pokemon?", choices = c("Random", levels(dat$pokemon))),
        numericInput("plevel", "What Level?",value = 0, min = 1, max = 20),
        actionButton("runbutton", "Create")
      ),
      
      # Show a table 
      mainPanel(
         DTOutput("movetable")
      ))),
   
   tabPanel("Output",
            mainPanel(
              DTOutput("gen_pokemon"),
              br(),
              br(),
              br(),
              textOutput("attr_summary"),
              textOutput("move_summary"))
            
   )
)

# Define server logic 
server <- function(input, output, session) {
  
  dat3 <- reactive({
    
    if (input$wpokemon == "Random"){
      pval = sample(dat2$pokemon, size = 1)
    }
    else {
      pval = input$wpokemon
    }
    
    if (input$plevel == 0){
      plev = sample(1:20, size = 1)
    }
    
    else {
      plev = input$plevel
    }
    return(list(func_call = filter(dat2, pokemon == pval & Level <= plev),
                val1 = plev,
                val2 = pval))
    
  })
  
  
  observeEvent(input$runbutton, {
    updateTabsetPanel(session, "tabber", selected = "Output")
  })
  
  
  output$movetable <- renderDT({
       datatable(dat)
     })
  
  output$gen_pokemon <- renderDT({
    datatable(dat3()$func_call)
  })
  
 
  
  output$move_summary <- renderText({
    dat_moves <- separate_rows(dat3()$func_call, Moves, sep = ",")
    movelist <- dat_moves$Moves
    if (length(movelist > 4)){
      final_movelist <- sample(movelist, 4)
    }
    else{
      final_movelist <- movelist
    }
    movelist2 <- paste(final_movelist, collapse = ",")
    paste("It's moveset is ", movelist2)
  })
  
  output$attr_summary <- renderPrint({
    paste("The pokemon is", dat3()$val2, "at Level", dat3()$val1)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

