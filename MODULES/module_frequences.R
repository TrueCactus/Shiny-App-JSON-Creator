# Frequency modules.

## UI SELECT routine varfreq
varfreqUI <- function (id,idata){
  id_list <<- append(id_list,id)
  
  routine_id <- list(id = id, type = "varfreq")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  
  data <- read_csv(idata)
  data_list <<- append(data_list,setNames(list(data), idata))
  cat_cols <- sapply(data, function(x) class(x) %in% c("factor", "character"))
  var_choices <- names(cat_cols)[cat_cols]

  tagList(
    # to hide idata from user and be able to pass it to server Module ( don't forget  useShinyjs())
    hidden(selectInput(NS(id,"idata"),"", choices=idata, selected=idata, multiple=FALSE)),
    #selectInput(NS(id, "varfreq"), "Variables", choices = var_choices)
    fluidRow(
      div(class = "d-flex align-items-center", 
          column(4, selectInput(NS(id, "varfreq"), "Variables", choices = var_choices))
          )
      )
    )
  
}


## UI TABLE 
varfreqTableUI <- function (id){
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "varfreqTable")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  (uiOutput((NS(id, "resultvarfreq"))))
}

## UI GRAPH
varfreqGraphUI <- function (id){
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "varfreqGraph")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  (uiOutput((NS(id, "resultvarGraphfreq"))))
}



## SERVER TABLE OUTPUT  
varfreqServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(data_list[input$idata])
        output$resultvarfreq <- renderUI ({
          
          data_frame <- as.data.frame(data())
          clean_names <- gsub(paste0(input$idata,"."), "", names(data_frame))
          names(data_frame) <- clean_names
          
          if(exists("Allfilterslist")){
            if(!is.null(Allfilterslist[[NS(id,"ilist")]])){
              dataFiltered <-   data_frame 
              for (i in 1:as.numeric(input$nvar)) {
                input_name <- paste0("var", i)
                input_value <- input[[input_name]]
                var_name <- Allfilterslist[[NS(id,"ilist")]][[i]]$var 
                if(!is.null(input_value)){
                  dataFiltered <- dataFiltered%>%filter( !!sym(var_name) == input_value)
                }
              }
            } else {
              dataFiltered <- data_frame
            }
          } else{ dataFiltered <- data_frame }
          
          
          var_name <- input$varfreq
          freq_table <- as.data.frame(table(dataFiltered[[var_name]]))
          gt_table <- gt::gt(as.data.frame(freq_table))
          res <- render_gt(gt_table)
     
          
          tagList(fluidRow(column(12,
                                  bslib::card(
                                    card_header(class = "bg-success", var_name)),
                                  card_body(res)
          )))
          
          
        })
      })
    }


## SERVER GRAPH OUTPUT
varfreqGraphServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(data_list[input$idata])
    output$resultvarGraphfreq <- renderUI ({
      
      data_frame <- as.data.frame(data())
      clean_names <- gsub(paste0(input$idata,"."), "", names(data_frame))
      names(data_frame) <- clean_names
      
      if(exists("Allfilterslist")){
        if(!is.null(Allfilterslist[[NS(id,"ilist")]])){
          dataFiltered <-   data_frame 
          for (i in 1:as.numeric(input$nvar)) {
            input_name <- paste0("var", i)
            input_value <- input[[input_name]]
            var_name <- Allfilterslist[[NS(id,"ilist")]][[i]]$var 
            if(!is.null(input_value)){
              dataFiltered <- dataFiltered%>%filter( !!sym(var_name) == input_value)
            }
          }
        } else {
          dataFiltered <- data_frame
        }
      } else{ dataFiltered <- data_frame }
      
      
      var_name <- input$varfreq
      freq_table <- as.data.frame(table(dataFiltered[[var_name]]))
     
      names(freq_table) <- c("Category", "Frequency")
      
      # 3. Créer un bar plot avec ggplot2
      res <- renderPlot(ggplot(freq_table, aes(x = Category, y = Frequency)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Frequency of Categories", x = "Category", y = "Frequency"))
      
      tagList(fluidRow(column(12,
                              bslib::card(
                                card_header(class = "bg-success", var_name)),
                              card_body(res)
      )


      ))
    })
  })
}