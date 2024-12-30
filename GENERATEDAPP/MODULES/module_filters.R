## Filters

## UI Part

#' filtersUI
#' UI Part for filters. It makes a collapsible card. In the card, there is the
#' variables options filters that are described in the json description file. 
#' And a reset filters button. There is two options, one for dash, one for nav
#' mode.
#' @param content 
#' @param session 
filtersUI <- function(content,session) {
  id  <- content$id
  routine_id <- list(id = id, type = "filters")
  id_list_routine <<- append(id_list_routine, list(routine_id))
  
  routine_id <- list(id = id, type = "resetfilters")
  id_list_routine <<- append(id_list_routine, list(routine_id))
  
  ui_json <- session$userData$ui_json
  idata <- content$data
  data <- read_csv(idata)
  text <- content$title
  listOfVar <- content$list
  
  for (nbr in 1:length(listOfVar)) {
    listOfVar[[nbr]][["numbers"]] <- nbr
  }
  
  if(exists("Allfilterslist")) {
    Allfilterslist[[NS(id, "ilist")]] <<- listOfVar
  } else {
    Allfilterslist <<- list()
    Allfilterslist[[NS(id, "ilist")]] <<- listOfVar
  }
  
  filtersList <- lapply(listOfVar, function(var) {
    var_id = paste0("var", var$numbers)
    inputId <- NS(id, var_id)
    label <- var$label
    variable <- var$var
    choices <- unique(data[[variable]])
    selectizeInput(inputId, label, choices, selected = NULL, multiple = TRUE)
  })
  
  rbutton <- actionButton(NS(id, "reset"), "Reset", class = "btn btn-outline-secondary")
  filtersList <- append(filtersList, list(rbutton))
  
  if(ui_json$mode == "dash"){
  res <- tagList(
    hidden(selectInput(NS(id, "nvar"), "", choices = nbr, selected = nbr, multiple = FALSE)),
    bs4Dash::accordion(
      id = NS(id, "filters"),
      bs4Dash::accordionItem(
        title = text,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        !!!filtersList
      )
    )
  )
  }
  
  if(ui_json$mode == "nav"){
    res <- tagList(
      hidden(selectInput(NS(id,"nvar"),"", choices= nbr, selected= nbr, multiple=FALSE)),
      bslib::accordion(id = NS(id,"filters"),open = FALSE,
                multiple = TRUE,
                bslib::accordion_panel( id = id,title = text,
                                 !!!filtersList
                ))
    )
  }
  
  res
}

# SERVER PART

##Reset Filters 

#' resetFiltersServer
#' reset filters when button reset is activated
#' @param id 
resetFiltersServer <- function(id){
  moduleServer(id, function(input, output, session) {
    observeEvent(input$reset, {
      for (i in 1:as.numeric(input$nvar)) {
        input_name <- paste0("var", i)
        reset(input_name)
      }
    })
  }) 
}

# Resumé filtres selectionnés

#' filtersServer
#' to resume the filters options in a text output.
#' @param id 
#' @return a text with the selected filters by variables.
filtersServer <- function(id){
  moduleServer(id, function(input, output, session) {
    output$rslt_filters <- renderText ({
      filtersList <- Allfilterslist[[NS(id,"ilist")]]
      result <- ""
      for (i in 1:as.numeric(input$nvar)) {
        input_name <- paste0("var", i)
        input_value <- input[[input_name]]
        var_name <- filtersList[[i]]$label
        result <- paste(result, var_name, ":" ,input_value, ".")
      }
      result
    })
  })
}