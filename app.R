library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Chaos Game 244"),
    
    sidebarLayout(
        sidebarPanel(
          selectInput("figura", h3("Selectati poligonul: "), 
                      choices = list("Triunghi" = 1, "Pătrat" = 2,
                                     "Pentagon" = 3, "Hexagon"= 4), selected = 1),
          
          sliderInput("dist", h3("Distanta: "),
                      min = 1, max = 99, value = 50),
          
          selectInput("categ", h3("Selectati range-ul: "), 
                      choices = list("1-100" = 1, "101-1000" = 2,
                                     "1001-10000" = 3), selected = 1),
          conditionalPanel(condition = "input.categ == 1",
            sliderInput("p1", h3("Numarul de puncte: "),
                min = 1, max = 100, value = 1)),
            
          conditionalPanel(condition = "input.categ == 2",
            sliderInput("p2", h3("Numarul de puncte: "),
                        min = 101, max = 1000, value = 101)),
          
          conditionalPanel(condition = "input.categ == 3",
            sliderInput("p3", h3("Numarul de puncte: "),
                        min = 1001, max = 10000, value = 1001))
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
