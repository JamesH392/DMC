library(jmspackage)
library(shiny)

#simulate data;
x = rnorm(10000) ; y = rnorm(10000)
ux = rnorm(5000)/3
uy = ux^2 -0.5
D<-data.frame(y,uy,x,ux)
#write.table(D,file="temp.txt",sep=",",row.names = FALSE,col.names = FALSE,)



ui <- fluidPage(
  titlePanel("Binplot panel"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             h2('The uploaded file data'),
                             dataTableOutput('mytable'),
                             fileInput('file', 'Choose info-file to upload',
                                       accept = c(
                                         'text/csv',
                                         'text/comma-separated-values',
                                         'text/tab-separated-values',
                                         'text/plain',
                                         '.csv',
                                         '.tsv'
                                       )
                             ),
                             # Taken from: http://shiny.rstudio.com/gallery/file-upload.html
                             tags$hr(),
                             checkboxInput('header', 'Header', TRUE),
                             radioButtons('sep', 'Separator',
                                          c(Comma=',',
                                            Semicolon=';',
                                            Tab='\t'),
                                          ','),
                             radioButtons('quote', 'Quote',
                                          c(None='',
                                            'Double Quote'='"',
                                            'Single Quote'="'"),
                                          '"'),
                             ################################################################

                             actionButton("choice1", "incorporate external information for column1"),
                             selectInput("column1", "Select Columns", choices = NULL), # no choices before uploading
                             tableOutput("table_display1"),

                             actionButton("choice2", "incorporate external information for column2"),
                             selectInput("column2", "Select Columns", choices = NULL), # no choices before uploading
                             tableOutput("table_display2"),

                            actionButton("choice3", "incorporate external information for column3"),
                            selectInput("column3", "Select Columns", choices = NULL), # no choices before uploading
                            tableOutput("table_display3"),


                             actionButton("choice4", "incorporate external information for column4"),
                             selectInput("column4", "Select Columns", choices = NULL), # no choices before uploading
                             tableOutput("table_display4")),






                mainPanel("main panel",
                          plotOutput("plot_display"),plotOutput("plot_display2"))))

server <- function(input, output, session) { # added session for updateSelectInput

  info1 <- eventReactive(input$choice1, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)

    # Changes in read.table
    f1 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f1)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "column1","Select Columns", choices = vars)
    f1
  })

  info2 <- eventReactive(input$choice2, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)

    # Changes in read.table
    f2 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f2)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "column2","Select Columns", choices = vars)
    f2
  })

  info3 <- eventReactive(input$choice3, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)

    # Changes in read.table
    f3 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f3)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "column3","Select Columns", choices = vars)
    f3
  })

  info4 <- eventReactive(input$choice4, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)

    # Changes in read.table
    f4 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f4)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "column4","Select Columns", choices = vars)
    f4
  })

  output$table_display1 <- renderTable({
    f <- info1()
    f <- subset(f, select = input$column1) #subsetting takes place here
    head(f)
  })
  output$table_display2 <- renderTable({
    f <- info2()
    f <- subset(f, select = input$column2) #subsetting takes place here
    head(f)
  })
  output$table_display3 <- renderTable({
    f <- info3()
    f <- subset(f, select = input$column3) #subsetting takes place here
    head(f)
  })
  output$table_display4 <- renderTable({
    f <- info4()
    f <- subset(f, select = input$column4) #subsetting takes place here
    head(f)
  })


  output$plot_display <- renderPlot({
    f1 <- info1()
    y <- subset(f1, select = input$column1)
    f2 <- info2()
    uy <- subset(f2, select = input$column2)
    f3 <- info3()
    x <- subset(f3, select = input$column3)
    f4 <- info4()
    ux <- subset(f4, select = input$column4)


    binplot(c(unlist(y),unlist(uy))+20,c(unlist(x),unlist(ux)),nr=100,nc=100)


  })

  output$plot_display2 <- renderPlot({
    f1 <- info1()
    y <- subset(f1, select = input$column1)
    f3 <- info3()
    x <- subset(f3, select = input$column3)

    binplot(unlist(y), unlist(x),nr=500, nc=500, "l")

  })





}

shinyApp(ui, server)
