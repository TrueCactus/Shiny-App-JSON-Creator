#server.R                                                                                      
options(bitmapType="cairo")
LOCAL = TRUE

server <- function(input, output, session) {
  
  if (LOCAL){
    credentials <- read_delim("credentials.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  } else {
    drv <- DBI::dbDriver("PostgreSQL")
    con <- dbConnect(drv , dbname = "dbname", host="127.0.0.1", port="5432", user="XXXX", password="XXXXXXXX")
    credentials <- dbGetQuery(con, "select * from moduleappuser")
    names(credentials)[names(credentials) == "username"] <- "user"
}
  auth <<- FALSE
  display <<- NULL

    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )
    
    # Create reactive values including all credentials
    creds_reactive <- reactive({
      reactiveValuesToList(res_auth)
    })
    
    observe({
      req(creds_reactive()$display)
      cat(file=stderr(), paste0("user :", creds_reactive()$user," ",Sys.Date(), Sys.time(), "\n"))
      if (!is.null(creds_reactive()$display)){
        display <- creds_reactive()$display
        var_display <<- display
        qid <<- creds_reactive()$portal_id
        auth <<- TRUE
      } 
    })
    
  observeEvent(res_auth,{
  if(auth == TRUE){
    
    # Read display according to user
    ui_json <- rjson::fromJSON(file = var_display)
    session$userData$ui_json <- ui_json
    # initialize the id for module UI et module server connexion.
    id_list <<- c()
    id_list_routine <<- list()
    
    data_list <<- list()
  
    if(ui_json$mode == "nav"){
      
      theme <- bs_theme(
        # Controls the default grayscale palette
        bg = "#FFF", fg = "#21759b",
        # Controls the accent (e.g., hyperlink, button, etc) colors
        primary = "#f1c31c", secondary = "#21759b",
        base_font = c("Grandstander", "sans-serif"),
        code_sfont = c("Courier", "monospace"),
        heading_font = "'Helvetica Neue', Helvetica, sans-serif",
        # Can also add lower-level customization
        "input-border-color" = "#f1c31c",
        "navbar-bg" ="#21759b",
        "nav-link-hover-color" = "#f1c31c !important",
        "nav-link-active-color" = "#f1c31c !important"
      )
    ui_app <- makeNavUi(ui_json,session,theme)
    }
    
    if(ui_json$mode == "dash"){
    ui_app <- MakeDashUi(ui_json,session)
    }
    
    output$MainUI <- renderUI({
      tagList(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
          tags$script(src = "custom.js"),
        ),
        ui_app
      )
    })
    
    # Call modules accordind to UI modules
    for (item in id_list_routine){
      # extract method
      method <- item$type
      id <- item$id
      # call the module
      switch(method,
             "updatedata" =  {updateDataServer(id)},
             "hist" =  {histogramServer(id)},
             "text" =  {TextServer(id)},
             "qfreq2" =  {qfreq2Server(id)},
             "varfreq" =  {varfreqServer(id)},
             "varfreqGraph" = {varfreqGraphServer(id)},
             "varfreqTable" = {varfreqServer(id)},
             "crossvargraph" = {CrossVarGraphServer(id)},
             "crossvartable" = {CrossVarTableServer(id)},
             "downloadFile" = {DownloadFileServer(id)},
             "showdata" = {ShowDataServer(id)},
             "showquota" = {ShowQuotaServer(id)},
             "apishowdata" = {ApiShowDataServer(id)},
             "selectquota" = {SelectQuotaServer(id)},
             "pivotdata" = {pivotDataServer(id)},
             "gwalkr" = {gwalkrServer(id)},
             "filters" = {filtersServer(id)},
             "advent" = {adventServer(id)},
             "appcreator"= {ui_json_creator_server(id)},
             "runcode" = {runcodeServer()}
             # Add others modules server :
             # "newtype" = {newTypeServer(id)}
      )
    }
  
  }
    }
  )
}