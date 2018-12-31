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
            mainPanel(DTOutput("gen_pokemon"))
   )
)

# Define server logic 
server <- function(input, output, session) {
   
  # Calculate table from inputs
  # pval = NULL
  # observeEvent(input$runbutton, {
  #   if (input$wpokemon == "Random"){
  #     pval = sample(dat2$pokemon, size = 1)
  #   }
  #   else {
  #     pval = input$wpokemon
  #   }
  #   dat3 <<- dat2 %>%
  #     filter(pokemon == pval & Level <= input$plevel)
  #   print(pval)
  # })
  
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
    filter(dat2, pokemon == pval & Level <= plev)
    #print(as.data.frame(pval))
  })
  
  observeEvent(input$runbutton, {
    updateTabsetPanel(session, "tabber", selected = "Output")
  })
  
  
  output$movetable <- renderDT({
       datatable(dat)
     })
  
  output$gen_pokemon <- renderDT({
    datatable(dat3())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

