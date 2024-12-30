# ShowData module

ShowDataUI <-function(id,idata){
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "showdata")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  tagList(
    hidden(
      selectInput(NS(id,"idata"),"", choices=idata, selected=idata, multiple=FALSE)
    ),
    bslib::card(
      bslib::card_body( uiOutput((NS(id, "showdataset"))))
    )
  )
}


ShowDataServer  <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$showdataset <- renderUI ({
      renderDT(server=FALSE,{
        # Load data
        data <- read_csv(input$idata)
        # Show data
        datatable(data, extensions = 'Buttons', filter = 'top',
                  options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                                 paging = TRUE, searching = TRUE,
                                 fixedColumns = TRUE, autoWidth = TRUE,
                                 ordering = TRUE, dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel','pdf')))
      })
    })
  }
  )
}