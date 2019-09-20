library(jmspackage)
library(shiny)


ui <- fluidPage(
  sliderInput(inputId="num",label="Choose nr,nc: ",value=20,min=1,max=100),
              plotOutput("binplot")
              )


server <- function(input, output) {

  output$binplot<-renderPlot({
    x = rnorm(10000) ; y = rnorm(10000)
    plot(x,y)
    binplot(x,y,10,10)
    binplot(x,y,50,50)
    binplot(x,y,100,100)
    binplot(x,y,100,100,'l')
    binplot(x,y,500,500,'l')
    ux = rnorm(5000)/3
    uy = ux^2 -0.5
    par(mar=c(4,4,1,1))
    plot(x=c(y,uy),y=c(x,ux),pch=20,col=3,xlab="X",ylab="Y",cex=0.7)

    binplot(c(y,uy)+20,c(x,ux),100,100)
    binplot(c(y,uy)+20,c(x,ux),100,100,"l")

  })
}



shinyApp(ui = ui, server = server)
