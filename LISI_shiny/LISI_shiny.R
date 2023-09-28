
library(shiny)

ui <- tagList(
  fluidPage(
    titlePanel("LISI (Measure single cell integration methods)"),
    sidebarLayout(
      sidebarPanel(
        # uiOutput 做上传文件的 ui, 对应后面的 output$file1
        uiOutput('file1'),
        # 对应后面的 output$file2
        uiOutput('file2'),
        
        actionButton('reset', 'RESET'),
        hr(),
        downloadButton("downloadData", "Download"),
        hr(),
        h5('Developer:'),
        h6('Small runze (shiny app)'),
        br(),
        h5('Github: '),
        h6('https://github.com/hzaurzli (Small runze)'),
        br(),
        h5('Cition:'),
        h6('https://github.com/immunogenomics/LISI')
      ),
      mainPanel(
        h4("LISI table"),
        br(),
        br(),
        shinycssloaders::withSpinner(
          dataTableOutput("table")
        )
      )
    )
  )
)



server <- function(input, output, session) {
  options(shiny.maxRequestSize=1024*1024*1024^2)
  
  values <- reactiveValues(
    file = NULL
  )
  
  X <- reactive({
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    read.csv(infile$datapath,header = T,row.names = 1)
  })
  
  meta_data <- reactive({
    infile <- input$file2
    if (is.null(infile)){
      return(NULL)      
    }
    read.csv(infile$datapath,header = T)
  })
  
  
  # observeEvent(input$reset), 代表点击 RESET 时触发的动作,此时重新渲染 fileInput 的 ui
  observeEvent(input$reset, {
    values$file <- NULL
    output$file1 <- renderUI({
      fileInput("file1", "Step 1: Choose scRNA PC,tSNE,UMAP dimensions csv",
                accept=c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
      )
    })
  }, ignoreNULL = F)
  
  # observeEvent(input$reset), 代表点击 RESET 时触发的动作,此时重新渲染 fileInput 的 ui
  observeEvent(input$reset, {
    values$file <- NULL
    output$file2 <- renderUI({
      fileInput("file2", "Step 2: Choose metadata csv",
                accept=c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
      )
    })
  }, ignoreNULL = F)
  
  
  output$table <- renderDataTable({
    library(lisi)
    
    X <- X()
    meta_data <- meta_data()
    
    if(is.null(X) | is.null(meta_data)){
      warning("Please upload files!")
    } 
    else{
      res <<- compute_lisi(X, meta_data, c('label1', 'label2'))
    }
  }, options = list(pageLength = 10))
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(linkList,file,row.names = T,quote = F)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

