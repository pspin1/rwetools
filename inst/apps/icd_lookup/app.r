devtools::load_all(".")
library(shiny)
library(DT)
library(shinyalert)
library(shinydashboard)



header <- dashboardHeader(title = 'ICD Code Lookup')
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('dashboard', tabName = 'dashboard', icon = icon('dashboard'))
  )
)
body <- dashboardBody(
  fluidPage(fluidRow(
    column(2,
           textInput("fileName", "File Name",
                       value = paste0("my file")
           )
    ),
    useShinyalert(),
    column(2,
            selectInput("fileType", "File Type",
                        choices = c("csv"=".csv", "xlsx"=".xlsx")
            )
    ),
    useShinyalert(),
    column(2,
           downloadButton("downloadData", "Download Selected Rows", icon = icon("download"),
                          style="color: #333; background-color: #FFF; border-color: #333")),
    useShinyalert()
    # column(2,
    #        actionButton(inputId = "run", label = "Write Selected Rows to SQL", icon = icon("paper-plane"),
    #                     style="color: #333; background-color: #FFF; border-color: #333")),
    #useShinyalert()
  )),
  p(),
  box(
    title = 'box', width = NULL, status = 'primary',
    DT::dataTableOutput('table2')
  )
)

ui<-dashboardPage(header, sidebar, body)

server = function(input, output) {
  output$table2 = DT::renderDataTable(
    rwetools::icd10_2019,
    filter = "top",
    extensions = c("Buttons", "Select"),
    options = list(
      lengthChange = FALSE,
      selection = "multiple",
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$fileName, "_", Sys.Date(), input$fileType)
    },
    content = function(file) {
      write.csv(rwetools::icd10_2019[input$table2_rows_selected,], file, row.names = FALSE)
    }
  )

}




shinyApp(ui, server)
