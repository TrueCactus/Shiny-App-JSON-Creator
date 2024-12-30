# # ui_json_creator_ui.R
XappCreatorUI <- function(id) {
  # Add module ID to global lists
  id_list <<- append(id_list, id)
  routine_id <- list(id = id, type = "appcreator")
  id_list_routine <<- append(id_list_routine, list(routine_id))
  
  ns <- NS(id)
  
  page_fluid(
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2C3E50",
      "enable-shadows" = TRUE,
      "card-border-radius" = "1rem",
      "input-border-radius" = "0.5rem"
    ),
    
    tags$head(
      tags$style(HTML("
        .card { 
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1) !important;
          margin-bottom: 1rem;
        }
        .card-header {
          background: linear-gradient(45deg, #2C3E50, #3498DB);
          color: white;
        }
        .btn-primary {
          background: linear-gradient(45deg, #2C3E50, #3498DB);
          border: none;
          padding: 0.5rem 1.5rem;
          transition: transform 0.2s;
        }
        .btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        }
        .accordion-button:not(.collapsed) {
          background: linear-gradient(45deg, #2C3E50, #3498DB);
          color: white;
        }
        #json_output {
          background-color: #f8f9fa;
          border-radius: 0.5rem;
          padding: 1rem;
          font-family: 'Monaco', 'Menlo', monospace;
        }
      "))
    ),
    
    card(
      class = "mb-4",
      card_header(
        div(
          class = "d-flex align-items-center",
          h2("Application JSON Creator", class = "card-title mb-0 flex-grow-1"),
          downloadButton(ns("download_json"), "Download JSON", 
                         class = "btn btn-light")
        )
      ),
      
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",
          
          # Left column - Inputs
          card(
            class = "h-100",
            card_body(
              accordion(
                multiple = TRUE,
                # Basic Settings
                accordion_panel(
                  "Basic Settings",
                  icon = bsicons::bs_icon("gear-fill"),
                  div(
                    class = "p-3",
                    textInput(ns("title"), "Application Title", value = "Example Title",
                              width = "100%"),
                    checkboxInput(ns("auth"), "Enable Authentication", value = TRUE),
                    selectInput(ns("mode"), "Application Mode", 
                                choices = c("Navigation" = "nav", "Dashboard" = "dash"),
                                width = "100%")
                  )
                ),
                
                # Menu Creation
                accordion_panel(
                  "Menu Creation",
                  icon = bsicons::bs_icon("list"),
                  div(
                    class = "p-3",
                    textInput(ns("menu_name"), "Menu Name", value = "HOME1",
                              width = "100%"),
                    textInput(ns("menu_title"), "Menu Title", value = "Home",
                              width = "100%"),
                    div(
                      class = "d-grid gap-2",
                      actionButton(ns("add_menu"), "Add Menu", 
                                   class = "btn-primary btn-lg mt-3",
                                   icon = icon("plus"))
                    )
                  )
                ),
                
                # Tab Creation
                accordion_panel(
                  "Tab Creation",
                  icon = bsicons::bs_icon("folder-plus"),
                  div(
                    class = "p-3",
                    textInput(ns("tab_name"), "Tab Name", value = "HOME1",
                              width = "100%"),
                    textInput(ns("tab_title"), "Tab Title", value = "Home",
                              width = "100%"),
                    div(
                      class = "d-grid gap-2",
                      actionButton(ns("add_tab"), "Add Tab", 
                                   class = "btn-primary btn-lg mt-3",
                                   icon = icon("plus"))
                    )
                  )
                ),
                
                # Content Creation
                accordion_panel(
                  "Content Creation",
                  icon = bsicons::bs_icon("building-add"),
                  div(
                    class = "p-3",
                    selectInput(ns("content_type"), "Content Type", 
                                choices = c(
                                  "Space" = "space",
                                  "Header" = "header",
                                  "Show Data" = "showdata",
                                  "Variable Frequency" = "varfreq",
                                  "Frequency Table" = "varfreqTable",
                                  "Frequency Graph" = "varfreqGraph",
                                  "Cross Variable" = "crossvar",
                                  "Cross Variable Table" = "crossvartable",
                                  "Cross Variable Graph" = "crossvargraph",
                                  "Video" = "video",
                                  "Image" = "image"
                                ),
                                width = "100%"),
                    uiOutput(ns("dynamic_content_options")),
                    div(
                      class = "d-grid gap-2",
                      actionButton(ns("add_content"), "Add Content", 
                                   class = "btn-primary btn-lg mt-3",
                                   icon = icon("plus"))
                    )
                  )
                )
              )
            )
          ),
          
          # Right column - JSON Output
          card(
            class = "h-100",
            card_body(
              h4("Generated JSON", class = "mb-4"),
              div(
                id = "json_output",
                verbatimTextOutput(ns("ui_json_output"), 
                                   placeholder = TRUE)
              )
            )
          )
        )
      )
    )
  )
}

XappCreatorUI <- function(id) {
  id_list <<- append(id_list, id)
  routine_id <- list(id = id, type = "appcreator")
  id_list_routine <<- append(id_list_routine, list(routine_id))
  
  ns <- NS(id)
  
  
  
  tagList(
    fluidRow(
      column(
        width = 6,
        card(
          class = "mb-4",
          card_body(
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
            actionButton(ns("add_menu"), "Add Menu", class = "btn-primary"),
            hr(),
            
            # Tabs creation
            h3("Create Tabs:"),
            textInput(ns("tab_name"), "Tab Name", value = "HOME1"),
            textInput(ns("tab_title"), "Tab Title", value = "Home"),
            actionButton(ns("add_tab"), "Add Tab", class = "btn-primary"),
            
            # Tab content
            p("Tab Contents:", class = "mt-3"),
            selectInput(ns("content_type"), "Content Type", 
                        choices = c("space", "header", "showdata", "varfreq", "varfreqTable", 
                                    "varfreqGraph", "crossvar", "crossvartable", 
                                    "crossvargraph", "video", "image")),
            uiOutput(ns("dynamic_content_options")),
            actionButton(ns("add_content"), "Add Content", class = "btn-primary")
          )
        )
      ),
      column(
        width = 6,
        card(
          class = "mb-4",
          card_body(
            # Area to display generated JSON
            h3("Generated JSON"),
            verbatimTextOutput(ns("ui_json_output")),
            card_header(
              div(
                class = "d-flex align-items-center",
                downloadButton(ns("download_json"), "Download JSON", 
                               class = "btn btn-light"),
                hr(),
                downloadButton(ns("download_app"), "Download APP", 
                               class = "btn btn-success")
              )
            )
          )
        )
      )
    )
  )
}

appCreatorUI <- function(id) {
  id_list <<- append(id_list, id)
  routine_id <- list(id = id, type = "appcreator")
  id_list_routine <<- append(id_list_routine, list(routine_id))
  
  ns <- NS(id)
  
  
  
  tagList(
    fluidRow(
      column(
        width = 6,
        card(
          class = "mb-4",
          card_body(
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
            actionButton(ns("add_menu"), "Add Menu", class = "btn-primary"),
            hr(),
            
            # Tabs creation
            h3("Create Tabs:"),
            textInput(ns("tab_name"), "Tab Name", value = "HOME1"),
            textInput(ns("tab_title"), "Tab Title", value = "Home"),
            actionButton(ns("add_tab"), "Add Tab", class = "btn-primary"),
            
            # Tab content
            p("Tab Contents:", class = "mt-3"),
            selectInput(ns("content_type"), "Content Type", 
                        choices = c("space", "header", "showdata", "varfreq", "varfreqTable", 
                                    "varfreqGraph", "crossvar", "crossvartable", 
                                    "crossvargraph", "video", "image")),
            uiOutput(ns("dynamic_content_options")),
            actionButton(ns("add_content"), "Add Content", class = "btn-primary")
          )
        )
      ),
      column(
        width = 6,
        card(
          class = "mb-4",
          card_body(
            # Area to display generated JSON
            h3("Generated JSON"),
            verbatimTextOutput(ns("ui_json_output")),
            card_header(
              div(
                class = "d-flex align-items-center",
                downloadButton(ns("download_json"), "Download JSON", 
                               class = "btn btn-light"),
                hr(),
                downloadButton(ns("download_app"), "Download APP", 
                               class = "btn btn-success")
              )
            )
          )
        )
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
               fileInput(ns("csv_file"), "Upload CSV File",
                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                         width = "100%"),
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
    
    
    # Ajouter l'observateur pour gérer l'upload du fichier
    observeEvent(input$csv_file, {
      req(input$csv_file)
      req(input$content_type == "showdata")
      
      # Créer le dossier data s'il n'existe pas
      if (!dir.exists("GENERATED/data")) {
        dir.create("GENERATED/data", recursive = TRUE)
      }
      
      # Copier le fichier dans le dossier APPTODOWNLOAD/data
      file.copy(
        input$csv_file$datapath,
        file.path("GENERATED/data", input$csv_file$name),
        overwrite = TRUE
      )
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
    
    output$download_app <- downloadHandler(
      filename = function() {
        app_title <- gsub("[^[:alnum:]]", "_", input$title)  # Remplace les caractères spéciaux par _
        paste0(app_title, ".zip")
      },
      content = function(file) {
        # Créer un dossier temporaire avec le nom de l'application
        temp_dir <- tempdir()
        app_title <- gsub("[^[:alnum:]]", "_", input$title)
        app_dir <- file.path(temp_dir, app_title)
        
        # Copier le contenu de GENERATEDAPP dans le nouveau dossier
        dir.create(app_dir, showWarnings = FALSE)
        file.copy(list.files("GENERATEDAPP", full.names = TRUE), 
                  app_dir, 
                  recursive = TRUE)
        
        # Sauvegarder le JSON actuel
        json_file <- file.path(app_dir, "app_config.json")
        writeLines(toJSON(ui_json(), pretty = TRUE, auto_unbox = TRUE), json_file)
        
        # Créer le zip avec le dossier renommé
        curr_wd <- getwd()
        setwd(temp_dir)  # Changer le répertoire de travail pour que le zip ait la bonne structure
        utils::zip(
          zipfile = file,
          files = app_title,
          flags = "-r9X"
        )
        setwd(curr_wd)  # Revenir au répertoire de travail initial
        
        # Nettoyer
        unlink(app_dir, recursive = TRUE)
      }
    )
  })
}

