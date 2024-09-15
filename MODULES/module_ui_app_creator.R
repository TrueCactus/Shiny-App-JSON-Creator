# # ui_json_creator_ui.R


appCreatorUI <- function(id) {
  id_list <<- append(id_list, id)
  routine_id <- list(id = id, type = "appcreator")
  id_list_routine <<- append(id_list_routine, list(routine_id))
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6,
        # First parameters
        h3("First Parameters:"),
        textInput(ns("title"), "Application Title", value = "Example Title"),
        checkboxInput(ns("auth"), "Authentication", value = TRUE),
        selectInput(ns("mode"), "Mode", choices = c("nav", "dash")),
        hr(), 
        # Menu creation
        h3("Create Menu:"),
        textInput(ns("menu_name"), "Menu Name", value = "HOME1"),
        textInput(ns("menu_title"), "Menu Title", value = "Home"),
        actionButton(ns("add_menu"), "Add Menu"),
        hr(), 
        # Tabs creation
        h3("Create Tabs:"),
        textInput(ns("tab_name"), "Tab Name", value = "HOME1"),
        textInput(ns("tab_title"), "Tab Title", value = "Home"),
        actionButton(ns("add_tab"), "Add Tab"),
        p("Tab Contents:"),
        # Tab content
        selectInput(ns("content_type"), "Content Type", 
                    choices = c("space", "header", "showdata", "varfreq", "varfreqTable", 
                                "varfreqGraph", "crossvar", "crossvartable", 
                                "crossvargraph", "video", "image")),
        uiOutput(ns("dynamic_content_options")),
        actionButton(ns("add_content"), "Add Content")
      ),
      column(
        width = 6,
        # Area to display generated JSON
        verbatimTextOutput(ns("ui_json_output")),
        downloadButton(NS(id, "download_json"), "Download JSON", class = "btn-primary")
      )
    )
  )
}


ui_json_creator_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # List to store JSON elements
    ui_json <- reactiveVal(list(
      title = input$title,
      auth = ifelse(input$auth, "YES", "NO"),
      mode = input$mode,
      Menu = list(),
      tabs = list()
    ))
    
    # Update application information
    observe({
      ui_json(list(
        title = input$title,
        auth = ifelse(input$auth, "YES", "NO"),
        mode = input$mode,
        Menu = ui_json()$Menu,
        tabs = ui_json()$tabs
      ))
    })
    
    # Dynamic options based on selected content type
    output$dynamic_content_options <- renderUI({
      req(input$content_type)
      
      switch(input$content_type,
             "space" = tagList(
               numericInput(ns("content_width"), "Width", value = 4, min = 1, max = 12)
             ),
             "header" = tagList(
               numericInput(ns("content_width"), "Width", value = 4, min = 1, max = 12),
               textInput(ns("content_text"), "Text", value = "Welcome to the app creator")
             ),
             "showdata" = tagList(
               numericInput(ns("content_width"), "Width", value = 12, min = 1, max = 12),
               textInput(ns("content_id"), "ID", value = "showdata-IDX"),
               textInput(ns("content_data"), "Data", value = "data.csv"),
               textInput(ns("content_title"), "Title", value = "data")
             ),
             "varfreq" = tagList(
               numericInput(ns("content_width"), "Width", value = 6, min = 1, max = 12),
               textInput(ns("content_id"), "ID", value = "varfreq-IDX"),
               textInput(ns("content_data"), "Data", value = "data.csv")
             ),
             "varfreqTable" = tagList(
               numericInput(ns("content_width"), "Width", value = 6, min = 1, max = 12),
               textInput(ns("content_id"), "ID", value = "varfreq-IDX"),
               textInput(ns("content_data"), "Data", value = "data.csv"),
               textInput(ns("content_title"), "Title", value = "Table")
             ),
             "varfreqGraph" = tagList(
               numericInput(ns("content_width"), "Width", value = 6, min = 1, max = 12),
               textInput(ns("content_id"), "ID", value = "varfreq-IDX"),
               textInput(ns("content_data"), "Data", value = "data.csv"),
               textInput(ns("content_title"), "Title", value = "Graph")
             ),
             "crossvar" = tagList(
               numericInput(ns("content_width"), "Width", value = 6, min = 1, max = 12),
               textInput(ns("content_id"), "ID", value = "crossvar-IDX"),
               textInput(ns("content_data"), "Data", value = "data.csv")
             ),
             "crossvartable" = tagList(
               numericInput(ns("content_width"), "Width", value = 6, min = 1, max = 12),
               textInput(ns("content_id"), "ID", value = "crossvar-table-IDX"),
               textInput(ns("content_data"), "Data", value = "data.csv"),
               textInput(ns("content_title"), "Title", value = "CrossVar Table")
             ),
             "crossvargraph" = tagList(
               numericInput(ns("content_width"), "Width", value = 6, min = 1, max = 12),
               textInput(ns("content_id"), "ID", value = "crossvar-graph-IDX"),
               textInput(ns("content_data"), "Data", value = "data.csv"),
               textInput(ns("content_title"), "Title", value = "CrossVar Graph")
             ),
             "video" = tagList(
               numericInput(ns("content_width"), "Width", value = 6, min = 1, max = 12),
               textInput(ns("content_id"), "ID", value = "video-IDX"),
               textInput(ns("content_link"), "Video Link", value = "https://www.youtube.com/embed/sQjnvaf072M?si=v7OgXNCHTITHkCor")
             ),
             "image" = tagList(
               numericInput(ns("content_width"), "Width", value = 3, min = 1, max = 12),
               textInput(ns("content_id"), "ID", value = "image-IDX"),
               textInput(ns("content_link"), "Image Link", value = "image.jpeg")
             )
      )
    })
    
    # Add menu
    observeEvent(input$add_menu, {
      current_json <- ui_json()
      new_menu <- list(name = input$menu_name, title = input$menu_title)
      current_json$Menu <- append(current_json$Menu, list(new_menu))
      ui_json(current_json)
    })
    
    # Add tab
    observeEvent(input$add_tab, {
      current_json <- ui_json()
      new_tab <- list(name = input$tab_name, title = input$tab_title, content = list())
      current_json$tabs <- append(current_json$tabs, list(new_tab))
      ui_json(current_json)
    })
    
    # Add content to tab
    observeEvent(input$add_content, {
      current_json <- ui_json()
      
      # Retrieve current tab (the last added)
      tab <- current_json$tabs[[length(current_json$tabs)]]
      
      # Content to add
      new_content <- switch(input$content_type,
                            "space" = list(type = "space", width = input$content_width),
                            "header" = list(type = "header", width = input$content_width, content = input$content_text),
                            "showdata" = list(type = "showdata", width = input$content_width, id = input$content_id, data = input$content_data, title = input$content_title),
                            "varfreq" = list(type = "varfreq", width = input$content_width, id = input$content_id, data = input$content_data),
                            "varfreqTable" = list(type = "varfreqTable", width = input$content_width, id = input$content_id, data = input$content_data, title = input$content_title),
                            "varfreqGraph" = list(type = "varfreqGraph", width = input$content_width, id = input$content_id, data = input$content_data, title = input$content_title),
                            "crossvar" = list(type = "crossvar", width = input$content_width, id = input$content_id, data = input$content_data),
                            "crossvartable" = list(type = "crossvartable", width = input$content_width, id = input$content_id, data = input$content_data, title = input$content_title),
                            "crossvargraph" = list(type = "crossvargraph", width = input$content_width, id = input$content_id, data = input$content_data, title = input$content_title),
                            "video" = list(type = "video", width = input$content_width, id = input$content_id, link = input$content_link),
                            "image" = list(type = "image", width = input$content_width, id = input$content_id, link = input$content_link)
      )
      
      # Add content to tab
      tab$content <- append(tab$content, list(new_content))
      current_json$tabs[[length(current_json$tabs)]] <- tab
      ui_json(current_json)
    })
    
    # Display the generated JSON
    output$ui_json_output <- renderPrint({
      jsonlite::toJSON(ui_json(), pretty = TRUE, auto_unbox = TRUE)
    })
    
    output$download_json <- downloadHandler(
      filename = function() {
        paste(input$title,"ui_json",Sys.Date(), ".json", sep = "")
      },
      content = function(file) {
        write(jsonlite::toJSON(ui_json(), pretty = TRUE, auto_unbox = TRUE), file)
      }
    )
  })
}

