library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)

load(file = "testdata.RData")
testdata %>% dplyr::select(trt) %>% unique() %>% pull -> trt_list
myadj.pvals = c(.001, .01, .05, .10, .15, .20, 1)

##############USER INTERFACE CODING####################
####################################################
ui <- navbarPage(theme = shinytheme("cosmo"),
  "repro",
  tabPanel("Joint Contrasts",
           sidebarLayout(
             sidebarPanel(
               helpText("Choose two contrasts to see their Joint Distribution", width = "3in"),
               selectInput('xaxis', "Choose X-axis Contrast", trt_list, selected = trt_list[2], width="3in"),
               selectInput('yaxis', "Choose Y-axis Contrast", trt_list, selected = trt_list[1], width="3in"),
               selectInput('adj.pvalue', 'Adjusted Pvalue', myadj.pvals, selected = myadj.pvals[4], width="1.5in"),
               br(),
               submitButton("Run")
               ),
             mainPanel(
               DT::dataTableOutput("mytable", width = "8in")
               )
             ))
  )

##############SERVER SIDE CODING####################
####################################################
server <- function(input, output) {
# CREATE A DATASET THAT VARIES BASED ON USER INPUT DEFINED IN SIDEBAR PANEL 
  joint_contrast_dataset <- reactive({
    testdata %>% 
      dplyr::filter(trt == input$xaxis) %>% 
      dplyr::select(x = log2FoldChange, qx = padj, mylabel) -> x
    testdata %>%
      dplyr::filter(trt == input$yaxis) %>% 
      dplyr::select(y = log2FoldChange, qy = padj, mylabel) -> y
    plotdata <- dplyr::left_join(x, y, by = c("mylabel"))
    plotdata %>% dplyr::mutate(x = ifelse(is.na(x), 0, x),
                        y = ifelse(is.na(y), 0, y),
                        qx = ifelse(is.na(qx), 1, qx),
                        qy = ifelse(is.na(qy), 1, qy)) %>%
      dplyr::mutate(significance = ifelse(plotdata$qx <= input$adj.pvalue & plotdata$qy > input$adj.pvalue, "X-Significant",
                                   ifelse(plotdata$qx > input$adj.pvalue & plotdata$qy <= input$adj.pvalue, "Y-Significant",
                                          ifelse(plotdata$qx <= input$adj.pvalue & plotdata$qy <= input$adj.pvalue, "Both-Significant", "Neither")))) %>%
      dplyr::filter(significance != "Neither") -> plotdata 
    final <- as.data.frame(table(plotdata$significance))
  })

 
  output$mytable <- DT::renderDataTable({joint_contrast_dataset()})
  
}
getwd()

# Run the application 
shinyApp(ui = ui, server = server)


