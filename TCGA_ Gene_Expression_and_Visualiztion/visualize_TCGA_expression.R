library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggfortify)
# Load data
data <- read.csv("TCGA_gene_expression_data.csv",header = TRUE)
genes <- data[,1]
rownames(data)<-genes
data <- data[,-1]

# Define UI
ui <- fluidPage(theme = "lumen" ,
                titlePanel("Gene Data Visualisation of 10 samples") ,
                sidebarLayout(
                  sidebarPanel(fluidRow
                               (column(5 ,
                                       #define inputs
                                       
                                       selectInput(inputId = "genename" ,
                                                   label = "select a gene" ,
                                                   choices = rownames(data) ,
                                                   selected = "A1BG") ,
                                       
                                       actionButton(inputId = "update" ,
                                                    label = "update") ,
                                       
                                       actionButton(inputId = "plot" ,
                                                    label = "boxplot")
                               ))) ,
                  #define outputs
                  mainPanel(
                    tabsetPanel(type = "tabs" ,
                                tabPanel("table" , dataTableOutput(outputId = "table")) ,
                                tabPanel("plot" , plotOutput(outputId = "boxp")
                                )))
                ))

#Define server

server <- function(input, output)
{
  #subset data
  
  selected_gene <- eventReactive(input$update ,
                                {
                                  data %>% filter(rownames(data) == paste0(input$genename , collapse = "")) 
                                }
  )
  
  output$table <- renderDataTable(selected_gene())
  
  box <- eventReactive(input$plot , 
  {
    renderPlot
    (
      { dat1<-as.matrix(selected_gene())
        dat1<-t(dat1)
        ggplot(data = dat1,mapping = aes("",dat1[1:9,])) +
        geom_boxplot() + 
        xlab(input$genename) + 
        ylab("expression")
      }
    ) 
    
    
  })
  
  output$boxp <- renderPlot(box())
  
}
shinyApp(ui = ui, server =  server)
