#module.R   
### valueBox 
valueBoxUI <- function(valuebox,session){
  
  style <- valuebox$style  
  
  
  id  <- valuebox$id
  routine_id <- list(id = id, type = "valuebox")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  ui_json <- session$userData$ui_json
  if(ui_json$mode == "nav"){
  res_box <- value_box(
    title = valuebox$title,
    value = eval(parse(text = valuebox$codeR)),
    showcase = bs_icon(valuebox$icon),
    style = style,
    full_screen = TRUE)
  }
  if(ui_json$mode == "dash"){
  res_box <- valueBox(
    value = eval(parse(text = valuebox$codeR)),
    subtitle = valuebox$title,
    color = "primary",
    width = 12
  )}
  
  res_box
}

#routine crossvar


#Image
imageUI <- function(id,link){
  tagList(
    img(src=link,height="100%", width="100%",  align = "center")
  )
}

#VIDEO
videoUI <- function(id,link){
  tagList(
    tags$iframe(width="560", height="315", src= link, frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
  )
}

# TEXT
textBoxUI <- function(id, text, background){
  tagList(
    box(
      width = 12, background = background, HTML(text)
    )
  )
}

### TABLE FROM LIST
#UI
tableFromListUI <- function (id,tableItem){
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "tablefromlist")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  tagList(
    hidden(
      selectInput(NS(id,"TABLE"),"", choices=tableItem, selected=tableItem, multiple=FALSE)
    ),
    bslib::card(
      bslib::card_body( uiOutput((NS(id, "tableFromList"))))
    )
  )
}
#SERVER
tableFromListServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$tableFromList <- renderUI ({
      Table <- input$TABLE
      Table = eval(parse(text = Table))
      res <- render_gt(Table$gt)
      res
    })
  })
}


# Download File Module
# UI
DownloadFileUI <-function(id,buttonTitle,pathFile){
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "downloadFile")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  tagList(
    hidden(
      selectInput(NS(id,"path"),"", choices=pathFile, selected=pathFile, multiple=FALSE)
    ),
    downloadButton(NS(id,"DownloadFile"),buttonTitle)
  )
}
# SERVER
DownloadFileServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$DownloadFile <- downloadHandler(
      filename = function(){input$path},
      content = function(fname){
        file.copy(input$path, fname)
      })
  })
}

# Example 1
histogramUI <- function(id) {
  id_list <<- append(id_list,id)
  
  routine_id <- list(id = id, type = "hist")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  
  
  tagList(
    selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}


histogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    
    output$hist <- renderPlot({
      xlabel <- paste("Histogramme de la variable",input$var )
      #hist(data(), breaks = input$bins, col = 'darkgray', border = 'white')
      
      ggplot(mtcars, aes(.data[[input$var]])) +
        geom_histogram(bins = input$bins,
                       fill = "steelblue3",
                       colour = "grey30") +
        xlab(xlabel) +
        theme_minimal()
    })
  })
}


#Example 2 
TextUI <- function(id) {
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "text")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  
  tagList(
    selectInput(NS(id, "var"), "Choix de Variable", choices = names(mtcars)),
    textOutput(NS(id, "text"))
  )
}


TextServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$text <- renderText(paste("la variable est ",input$var))
  })
}


actionButtonUI <- function(id,text) {
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "actionbutton")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  tagList(
  actionButton(NS(id,"-normalbutton"),text)
  )

}


adventUI <- function(id,img) { 
  # ajout dans la liste des modules appelés
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "advent")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  
  # img
  img_var <- img
  
  # définition liste des 24 jours.
  #nombre24 <- sample(1:24, 24, replace = FALSE)
  nombre24 <- c(9, 12, 3, 17, 16, 22, 24, 7, 20, 23, 10, 19, 13, 1, 5, 15, 21, 2, 8, 14, 11, 18, 4, 6)
  
  # Préparation de la liste de 24 items pour la création des 24 bouttons.
  buttonList <- list()
  for (i in 1:24){
    n <- as.character(nombre24[i])
    buttonList[n] = n
  }
  
  
  # Création de la card avec les 24 bouttons
  bslib::card( class = "bslib-card bslib-card-advent",
    fluidRow(
      tagList(
        tags$head(
          # style css intégrés directement dans le code pour les classes btn-advent/btn-star24
          tags$style(HTML(paste0("
          .btn.btn-advent  {
            border-radius: 0%;
            padding: 50%;
            text-align: center;
            border: 1px solid black;
            background-color: transparent;
            color: #ff0000;
          }

          .bslib-card.bslib-card-advent {
            background-image: url('", img_var, "');
            background-size: cover;
            background-repeat: no-repeat;
          }
          
          .btn-star24 {
        padding: 50%;
        border-radius: 50%;
        border: 1px solid red;
        background-color: transparent;
        color: #ff0000;
      }
        ")))
        ),
      # création des buttons et definition de leur classe selon si c'est le 24 ou pas !
        lapply(buttonList,function(case){
         if(case =="24")
         {
           buttonClass <- "btn  btn-star24"
         }else{
           buttonClass <- "btn btn-advent"
         }
        column(3,actionButton(NS(id,paste0("case",case,"-advent")),paste(case),class = buttonClass))
        })
      )
    ) 
    
  )
}


adventServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$"case24-advent", {
      showModal(modalDialog(
        title = "Case 24",
        tags$img(src="pere-noel.jpeg"),
        "Joyeux Noel !",
        easyClose = TRUE
      ))
    })
    
  })
}



pivotDataUI <-function(id,datapivot){
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "pivotdata")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  tagList(
    hidden(
      selectInput(NS(id,"datapivot"),"", choices=datapivot, selected=datapivot, multiple=FALSE)
    ),
        uiOutput((NS(id, "pivotdata")))
    )
  
}

pivotDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
        # observe({
        #   # Exécuter du code JavaScript pour manipuler le DOM
        #   session$sendCustomMessage(type = "runJs", message = list(action = "applyCustomStyle"))
        # })
    
    output$pivotdata <- renderUI ({
      try(data <- datasets[[as.numeric(input$datapivot)]])
      res <- renderRpivotTable(rpivotTable(data = data))
      res
    })
    
    
  })
}



gwalkrUI <-function(id,datapivot){
  id_list <<- append(id_list,id)
  routine_id <- list(id = id, type = "gwalkr")
  id_list_routine <<- append(id_list_routine,list(routine_id))
  tagList(
    hidden(
      selectInput(NS(id,"idatagwalkr"),"", choices=datapivot, selected=datapivot, multiple=FALSE)
    ),
    uiOutput((NS(id, "gwalkrdata")))
  )

}



gwalkrServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$gwalkrdata <- renderUI ({
      try(data <- datasets[[as.numeric(input$idatagwalkr)]])
      res <- renderGwalkr( gwalkr(data = data))
      res
    })
  })
}
