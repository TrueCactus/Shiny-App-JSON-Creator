# Crossvar modules.

## UI SELECT Crossvar

crossvarUI <- function(id, idata) {
  id_list <<- append(id_list, id)
  
  routine_id <- list(id = id, type = "crossvar")
  id_list_routine <<- append(id_list_routine, list(routine_id))
  data <- read_csv(idata)
  cat_cols <- sapply(data, function(x) class(x) %in% c("factor", "character"))
  var_choices <- names(cat_cols)[cat_cols]
  tagList(
    hidden(selectInput(NS(id, "idata"), "", choices = idata, selected = idata, multiple = FALSE)),
    fluidRow(
      div(class = "d-flex align-items-center",
          column(6, selectInput(NS(id, "var1"), "Variable 1", choices = var_choices)),
          column(6, selectInput(NS(id, "var2"), "Variable 2", choices = var_choices))
      )
    )
  )
}


CrossVarUITable <- function(id) {
  id_list <<- append(id_list, id)
  routine_id <- list(id = id, type = "crossvartable")
  id_list_routine <<- append(id_list_routine, list(routine_id))
  
  uiOutput(NS(id, "resultCrossVarTable"))
}


CrossVarUIGraph <- function(id) {
  id_list <<- append(id_list, id)
  routine_id <- list(id = id, type = "crossvargraph")
  id_list_routine <<- append(id_list_routine, list(routine_id))
  
  uiOutput(NS(id, "resultCrossVarGraph"))
}



## SERVER TABLE OUTPUT  
CrossVarTableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive(data_list[input$idata])
    output$resultCrossVarTable <- renderUI({
      
      data_frame <- as.data.frame(data())
      clean_names <- gsub(paste0(input$idata,"."), "", names(data_frame))
      names(data_frame) <- clean_names

      var1 <- input$var1
      var2 <- input$var2
      
      freq_table <- as.data.frame(table(data_frame[[var1]], data_frame[[var2]]))
      colnames(freq_table) <- c(var1, var2, "Frequency")
      
      gt_table <- gt::gt(freq_table)
      res <- render_gt(gt_table)
      
      tagList(fluidRow(column(12, bslib::card(card_header(class = "bg-success", 
                                                          paste(var1, "x", var2)), card_body(res)))))
    })
  })
}


## SERVER GRAPH OUTPUT
CrossVarGraphServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(data_list[input$idata])
    output$resultCrossVarGraph <- renderUI({
      data_frame <- as.data.frame(data())
      clean_names <- gsub(paste0(input$idata,"."), "", names(data_frame))
      names(data_frame) <- clean_names
      var1 <- input$var1
      var2 <- input$var2
      
      freq_table <- as.data.frame(table(data_frame[[var1]], data_frame[[var2]]))
      colnames(freq_table) <- c(var1, var2, "Frequency")
      
      res <- renderPlot(ggplot(freq_table, aes(x = get(var1), y = Frequency, fill = get(var2))) +
                          geom_bar(stat = "identity", position = "dodge") +
                          theme_minimal() +
                          labs(title = paste("Cross-Var Plot:", var1, "x", var2), x = var1, y = "Frequency"))
      
      tagList(fluidRow(column(12, bslib::card(card_header(class = "bg-success", paste(var1, "x", var2)), card_body(res)))))
    })
  })
}