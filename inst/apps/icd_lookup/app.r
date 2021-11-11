#devtools::load_all(".")
library(shiny)
library(DT)
library(shinyalert)
library(shinydashboard)



header <- dashboardHeader(title = 'ICD Code Lookup')
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('dashboard', tabName = 'dashboard', icon = icon('dashboard')),
    menuItem('dashboard2', tabname = 'dashboard2')
  )
)
body <- dashboardBody(
  # fluidPage(fluidRow(
  #   column(2,
  #          textInput("fileName", "File Name",
  #                      value = paste0("my file")
  #          )
  #   ),
  #   useShinyalert(),
  #   column(2,
  #           selectInput("fileType", "File Type"
  #                       choices = c("csv"=".csv", "xlsx"=".xlsx")
  #           )
  #   ),
  #   useShinyalert(),
  #   column(2,
  #          downloadButton("downloadData", "Download Selected Rows", icon = icon("download"),
  #                         style="color: #333; background-color: #FFF; border-color: #333")),
  #   useShinyalert()
  #   # column(2,
  #   #        actionButton(inputId = "run", label = "Write Selected Rows to SQL", icon = icon("paper-plane"),
  #   #                     style="color: #333; background-color: #FFF; border-color: #333")),
  #   #useShinyalert()
  # )),
  p(),
  box(
    title = 'ICD Codes', width = NULL, status = 'primary',
    DT::dataTableOutput('table2')
  )
)

ui<-dashboardPage(header, sidebar, body)

server = function(input, output) {
  output$table2 = DT::renderDataTable({
    DT::datatable(
      rwetools::icd10_2019,
      filter = "top",
      class = "compact hover row-border",
      extensions = c("Scroller", "Buttons", "Select"),
      options = list(
        lengthChange = FALSE,
        select = list(style = "multi", items = "row"),
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        dom = "BRlfrtip",
        buttons = list(list(extend='selectAll',className='selectAll',
                            text="select all rows",
                            action=DT::JS("function () {
                                var table = $('#DataTables_Table_0').DataTable();
                                table.rows({ search: 'applied'}).deselect();
                                table.rows({ search: 'applied'}).select();
                }")
        ), list(extend='selectNone',
                text="DeselectAll",
                action=DT::JS("function () {
                                var table = $('#DataTables_Table_0').DataTable();
                                table.rows({ search: 'applied'}).select();
                                table.rows({ search: 'applied'}).deselect();
                }")
        ))

      ), selection="none"
    )
  })

  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste0(input$fileName, "_", Sys.Date(), input$fileType)
  #   },
  #   content = function(file) {
  #     write.csv(rwetools::icd10_2019[input$table2_rows_selected,], file, row.names = FALSE)
  #   }
  # )

}




shinyApp(ui, server)
