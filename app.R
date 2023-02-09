library(shiny)


genTriunghi <- function(distanta)
{
  maxim = 10000
  
  #Triunghi echilateral
  varfuri = matrix(NA, ncol = 2, nrow = 3)
  varfuri[1,] = c(0, 0)
  varfuri[2,] = c(1, 0)
  varfuri[3,] = c(0.5, sqrt(3)/2)
  #punctele adaugate -> pana la 10000 de puncte -> tot atatea randuri
  #coordonate x si y -> 2 coloane
  puncte = matrix(NA, ncol = 2, nrow = maxim)
  #Generarea primului punct random
  p1 = runif(1)
  p2 = runif(1)
  sqr = sqrt(p1)
  puncte[1,] = c(((1-sqr)*0 + (sqr * (1-p2)) * 1 + (sqr * p2) * 0.5), ((1-sqr)*0 + (sqr * (1-p2)) * 0 + (sqr * p2) * sqrt(3)/2))
  #Generarea restului de puncte
  for(i in 2:maxim)
  {
    #Determinam un varf random si ii memoram coordonatele
    varf = sample(1:3, 1, replace=TRUE)
    x_varf = varfuri[varf[1], 1]
    y_varf = varfuri[varf[1], 2]
    
    #Gasim punctul aflat la distanta data intre punctul anterior si varful determinat
    punct = c((x_varf - puncte[i-1, 1]) * distanta + puncte[i-1, 1], (y_varf - puncte[i-1, 2]) * distanta + puncte[i-1, 2])
    #Memoram noul punct
    puncte[i, ] = punct
  }
  
  return (list(puncte, varfuri))
  
}

genPatrat <- function(distanta)
{
  maxim = 10000
  
  #Patrat
  varfuri = matrix(NA, ncol = 2, nrow = 4)
  varfuri[1,] = c(0, 0)
  varfuri[2,] = c(1, 0)
  varfuri[3,] = c(1, 1)
  varfuri[4,] = c(0, 1)
  #punctele adaugate -> pana la 10000 de puncte -> tot atatea randuri
  #coordonate x si y -> 2 coloane
  puncte = matrix(NA, ncol = 2, nrow = maxim)
  #Generarea primului punct random
  p1 = runif(1)
  p2 = runif(1)
  puncte[1,] = c(p1, p2)
  #Generarea restului de puncte
  for(i in 2:maxim)
  {
    #Determinam un varf random si ii memoram coordonatele
    varf = sample(1:4, 1, replace=TRUE)
    x_varf = varfuri[varf[1], 1]
    y_varf = varfuri[varf[1], 2]
    
    #Gasim punctul aflat la distanta data intre punctul anterior si varful determinat
    punct = c((x_varf - puncte[i-1, 1]) * distanta + puncte[i-1, 1], (y_varf - puncte[i-1, 2]) * distanta + puncte[i-1, 2])
    #Memoram noul punct
    puncte[i, ] = punct
  }
  
  return (list(puncte, varfuri))
  
}

genPentagon <- function(distanta)
{
  maxim = 10000
  
  #Pentagon
  varfuri = matrix(NA, ncol = 2, nrow = 5)
  varfuri[1,] = c(0.5, 1)
  varfuri[2,] = c(0, 0.63)
  varfuri[3,] = c(1, 0.63)
  varfuri[4,] = c(0.18, 0)
  varfuri[5,] = c(0.82, 0)
  #punctele adaugate -> pana la 10000 de puncte -> tot atatea randuri
  #coordonate x si y -> 2 coloane
  puncte = matrix(NA, ncol = 2, nrow = maxim)
  #Generarea primului punct random
  p1 = runif(1)
  p2 = runif(1)
  puncte[1,] = c(p1, p2)
  #Generarea restului de puncte
  for(i in 2:maxim)
  {
    #Determinam un varf random si ii memoram coordonatele
    varf = sample(1:5, 1, replace=TRUE)
    x_varf = varfuri[varf[1], 1]
    y_varf = varfuri[varf[1], 2]
    
    #Gasim punctul aflat la distanta data intre punctul anterior si varful determinat
    punct = c((x_varf - puncte[i-1, 1]) * distanta + puncte[i-1, 1], (y_varf - puncte[i-1, 2]) * distanta + puncte[i-1, 2])
    #Memoram noul punct
    puncte[i, ] = punct
  }
  
  return (list(puncte, varfuri))
  
}

genHexagon <- function(distanta)
{
  maxim = 10000
  
  #Hexagon
  varfuri = matrix(NA, ncol = 2, nrow = 6)
  varfuri[1,] = c(0.25, 0)
  varfuri[2,] = c(0.75, 0)
  varfuri[3,] = c(0, 0.5)
  varfuri[4,] = c(1, 0.5)
  varfuri[5,] = c(0.25, 1)
  varfuri[6,] = c(0.75, 1)
  #punctele adaugate -> pana la 10000 de puncte -> tot atatea randuri
  #coordonate x si y -> 2 coloane
  puncte = matrix(NA, ncol = 2, nrow = maxim)
  #Generarea primului punct random
  p1 = runif(1)
  p2 = runif(1)
  puncte[1,] = c(p1, p2)
  #Generarea restului de puncte
  for(i in 2:maxim)
  {
    #Determinam un varf random si ii memoram coordonatele
    varf = sample(1:6, 1, replace=TRUE)
    x_varf = varfuri[varf[1], 1]
    y_varf = varfuri[varf[1], 2]
    
    #Gasim punctul aflat la distanta data intre punctul anterior si varful determinat
    punct = c((x_varf - puncte[i-1, 1]) * distanta + puncte[i-1, 1], (y_varf - puncte[i-1, 2]) * distanta + puncte[i-1, 2])
    #Memoram noul punct
    puncte[i, ] = punct
  }
  
  return (list(puncte, varfuri))
  
}

ui <- fluidPage(
  
  titlePanel("Chaos Game 244"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("poligon", h3("Selectati poligonul: "), 
                  choices = list("Triunghi" = 1, "Pătrat" = 2,
                                 "Pentagon" = 3, "Hexagon"= 4), selected = 1),
      
      sliderInput("dist", h3("Distanta: "),
                  min = 1, max = 99, value = 50),
      
      selectInput("categ", h3("Selectati range-ul: "), 
                  choices = list("1-100" = 1, "101-1000" = 2,
                                 "1001-10000" = 3), selected = 1),
      
      conditionalPanel(condition = "input.categ == 1",
                       sliderInput("p1", h3("Numarul de puncte: "),
                                   min = 1, max = 100, step = 1, value = 1, animate=animationOptions(interval = 400))),
      
      conditionalPanel(condition = "input.categ == 2",
                       sliderInput("p2", h3("Numarul de puncte: "),
                                   min = 101, max = 1000, step = 25, value = 101, animate=animationOptions(interval = 400))),
      
      conditionalPanel(condition = "input.categ == 3",
                       sliderInput("p3", h3("Numarul de puncte: "),
                                   min = 1001, max = 10000, step = 250, value = 1001, animate=animationOptions(interval = 400)))
    ),
    
    
    mainPanel(
      fluidRow(
        
        conditionalPanel(condition = "input.categ == 1",
                         plotOutput("m1", width = "400px", height = "400px")),
        
        conditionalPanel(condition = "input.categ == 2",
                         plotOutput("m2", width = "400px", height = "400px")),
        
        conditionalPanel(condition = "input.categ == 3",
                         plotOutput("m3", width = "400px", height = "400px")),
      )
    )
  )
)


server <- function(input, output) {
  
  poligon <- reactive({
    #Triunghi
    if(input$poligon == 1)
      return(genTriunghi(input$dist/100))
    #Pătrat
    if(input$poligon == 2)
      return(genPatrat(input$dist/100))
    #Pentagon
    if(input$poligon == 3)
      return(genPentagon(input$dist/100))
    #Hexagon
    if(input$poligon == 4)
      return(genHexagon(input$dist/100))
  })
  
  #Generarea plotului
  output$m1 = renderPlot({
    
    pol = poligon();
    puncte = pol[[1]]
    varfuri = pol[[2]]
    
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(varfuri[, 1],varfuri[, 2], pch = 20, cex = 2, col = "red")
    points(puncte[1 : input$p1 - 1, 1], puncte[1 : input$p1 - 1, 2], pch = 20, cex=1, col="blue")
    
  })
  
  output$m2 = renderPlot({
    
    pol = poligon();
    puncte = pol[[1]]
    varfuri = pol[[2]]
    
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(varfuri[, 1],varfuri[, 2], pch = 20, cex = 2, col = "red")
    points(puncte[1 : input$p2, 1], puncte[1 : input$p2, 2], pch = 20, cex=1, col="blue")
    
  })
  
  output$m3 = renderPlot({
    
    pol = poligon();
    puncte = pol[[1]]
    varfuri = pol[[2]]
    
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(varfuri[, 1],varfuri[, 2], pch = 20, cex = 2, col = "red")
    points(puncte[1 : input$p3, 1], puncte[1 : input$p3, 2], pch = 20, cex=1, col="blue")
    
  })
  
}

shinyApp(ui = ui, server = server)