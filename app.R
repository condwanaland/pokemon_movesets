library(shiny)
library(DT)
library(dplyr)

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
ui <- fluidPage(
   
   # Application title
   titlePanel("Generate Pokemon"),
   
   # Sidebar layout 
   sidebarLayout(
      sidebarPanel(
        selectInput("wpokemon", "What Pokemon?", choices = c("Random", levels(dat$pokemon))),
        numericInput("plevel", "What Level?",value = 0, min = 1, max = 20),
        actionButton("runbutton", "Create")
      ),
      
      # Show a table 
      mainPanel(
         DTOutput("movetable"),
         uiOutput("random_pokemon")
      )
   )
)

# Define server logic 
server <- function(input, output) {
   
  # Calculate table from inputs
  pval = NULL
  observeEvent(input$runbutton, {
    if (input$wpokemon == "Random"){
      pval = sample(dat2$pokemon, size = 1)
    }
    else {
      pval = input$wpokemon
    }
    dat2 <<- dat2 %>% 
      filter(pokemon == pval & Level <= input$plevel)
  })
  
  
  
  output$movetable <- renderDT({
     
     input$runbutton
     
     if (input$runbutton == 0){
       datatable(dat, filter = "top")
     }
     else{
       datatable(dat2)
     }
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

