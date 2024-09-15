# Modules utilities to manage different arrangement

# Ne pas oublier de mettre à jour avec les nouveaux module UI
makeContent <- function(page,session){
  fluidRow(
    useShinyjs(),
    lapply(page$content, function(content) {
      switch(content$type,
             "header" = {column(as.numeric(content$width), h2(content$content))},
             "updatedata" = {column(as.numeric(content$width), updateDataUI(content$id))},
             "appcreator" = {column(as.numeric(content$width), appCreatorUI(content$id))},
             "hist" = {column(as.numeric(content$width), histogramUI(content$id))},
             "tablefromlist" = {column(as.numeric(content$width), tableFromListUI(content$id,content$Table))},
             "downloadFile" = {column(as.numeric(content$width), DownloadFileUI(content$id,content$buttonTitle,content$pathFile))},
             "showdata" = {column(as.numeric(content$width), ShowDataUI(content$id,content$data))},
             "apishowdata" = {column(as.numeric(content$width), ApiShowDataUI(content$id,content$data))},
             "pivotdata" = {column(as.numeric(content$width), pivotDataUI(content$id,content$data))},
             "gwalkr" = {column(as.numeric(content$width), gwalkrUI(content$id,content$data))},
             "text" = { column(as.numeric(content$width), TextUI(content$id))},
             "qfreq2" = {column(as.numeric(content$width), qfreq2UI(content$id,content$data))},
             "varfreq" = {column(as.numeric(content$width), varfreqUI(content$id,content$data))},
             "varfreqplotbox" = {column(as.numeric(content$width), varfreqplotboxUI(content$id,content$data,content$var))},
             "varfreqGraph" = {column(as.numeric(content$width), varfreqGraphUI(content$id))},
             "varfreqTable" = {column(as.numeric(content$width), varfreqTableUI(content$id))},
             "crossvar" = {column(as.numeric(content$width), crossvarUI(content$id,content$data))},
             "crossvartable" = {column(as.numeric(content$width), CrossVarUITable (content$id))},
             "crossvargraph" = {column(as.numeric(content$width), CrossVarUIGraph(content$id))},
             "filters" = {column(as.numeric(content$width), filtersUI(content,session))},
             "image" = {column(as.numeric(content$width), imageUI(content$id,content$link))},
             "video" = {column(as.numeric(content$width), videoUI(content$id,content$link))},
             "textbox" = {column(as.numeric(content$width), textBoxUI(content$id,content$text,content$background))},
             "advent" = {column(as.numeric(content$width), adventUI(content$id,content$img))},
             "actionButton" = {column(as.numeric(content$width), actionButtonUI(content$id,content$text))},
             "runcode" = {
               routine_id <- list(id = content$id, type = "runcode")
               id_list_routine <<- append(id_list_routine,list(routine_id))
               useShinyjs()
               column(as.numeric(content$width), runcodeUI())},
             "multinavbox" = {column(as.numeric(content$width),makeMultiNavBox(content,session) )},
             "multinavboxDash" = {column(as.numeric(content$width),makeMultiNavBoxDash(content,session) )},
             "multibox" = {column(as.numeric(content$width),makeMultiBox(content,session) )},
             "valuebox" = {column(as.numeric(content$width),valueBoxUI(content,session) )},
             "space" = {
               space <- paste0("col-sm-",content$width)
               div(class = space)}
             # add others modules:
             # "newtype" = {column(as.numeric(content$width), newTypeUI(content$id))}
      ) #switch
    }) #lapply
  )
}



makeMultiBox <- function(box,session){
  text =  HTML(box$title)
  OptionList <- lapply(box$boxcontent, function(content) {
    switch(content$type,
           "header" = {column(as.numeric(content$width), h2(content$content))},
           "hist" = {column(as.numeric(content$width), histogramUI(content$id))},
           "text" = { column(as.numeric(content$width), TextUI(content$id))},
           "pivotdata" = {column(as.numeric(content$width), pivotDataUI(content$id,content$data))},
           "downloadFile" = {column(as.numeric(content$width), DownloadFileUI(content$id,content$buttonTitle,content$pathFile))},
           "varfreq" = {column(as.numeric(content$width), varfreqUI(content$id,content$data))},
           "varfreqplotbox" = {column(as.numeric(content$width), varfreqplotboxUI(content$id,content$data,content$var))},
           "varfreqGraph" = {column(as.numeric(content$width), varfreqGraphUI(content$id))},
           "varfreqTable" = {column(as.numeric(content$width), varfreqTableUI(content$id))},
           "crossvar" = {column(as.numeric(content$width), crossvarUI(content$id,content$data))},
           "crossvartable" = {column(as.numeric(content$width), CrossVarUITable (content$id))},
           "crossvargraph" = {column(as.numeric(content$width), CrossVarUIGraph (content$id))},
           "filters" = {column(as.numeric(content$width), filtersUI(content,session))},
           "image" = {column(as.numeric(content$width), imageUI(content$id,content$link))},
           "video" = {column(as.numeric(content$width), videoUI(content$id,content$link))},
           "textbox" = {column(as.numeric(content$width), textBoxUI(content$id,content$text,content$background))},
           "advent" = {column(as.numeric(content$width), adventUI(content$id,content$img))},
           "runcode" = {
             routine_id <- list(id = content$id, type = "runcode")
             id_list_routine <<- append(id_list_routine,list(routine_id))
             useShinyjs()
             column(as.numeric(content$width), runcodeUI())},
           "multibox" = {column(as.numeric(content$width),makeMultiBox(content,session) )},
           "multinavbox" = {column(as.numeric(content$width),makeMultiNavBox(content,session) )},
           "valuebox" = {column(as.numeric(content$width),valueBoxUI(content,session) )},
           "space" = {
             space <- paste0("col-sm-",content$width)
             div(class = space)}
           # Ajouter d'autres types de modules UI :
           # "newtype" = {column(as.numeric(content$width), newTypeUI(content$id))}
    ) #switch
  }) #lapply
  
  fluidRow( 
    !!!OptionList
  )
  
}




## MultiNavbox
makeMultiNavBox <- function(box,session){
  text =  HTML(box$title)
  OptionList <- lapply(box$boxcontent, function(content) {
    switch(content$type,
           "header" = {nav_panel( HTML(content$title),  h2(content$content))},
           "showdata" = {nav_panel( HTML(content$title), ShowDataUI(content$id,content$data))},
           "pivotdata" = {nav_panel( HTML(content$title), pivotDataUI(content$id,content$data))},
           "hist" = {nav_panel( HTML(content$title), histogramUI(content$id))},
           "text" = {nav_panel( HTML(content$title),  TextUI(content$id))},
           "downloadFile" = {nav_panel( HTML(content$title), DownloadFileUI(content$id,content$buttonTitle,content$pathFile))},
           "varfreq" = {nav_panel( HTML(content$title),  varfreqUI(content$id,content$data))},
           "varfreqGraph" = {nav_panel( HTML(content$title), varfreqGraphUI(content$id))},
           "varfreqTable" = {nav_panel(HTML(content$title), varfreqTableUI(content$id))},
           "crossvar" = {nav_panel( HTML(content$title),  crossvarUI(content$id,content$data))},
           "crossvargraph" = {nav_panel( HTML(content$title),  CrossVarUIGraph (content$id))},
           "crossvartable" = {nav_panel( HTML(content$title),  CrossVarUITable (content$id))},
           "image" = {nav_panel( HTML(content$title),  imageUI(content$id,content$link))},
           "video" = {nav_panel( HTML(content$title),  videoUI(content$id,content$link))},
           "textbox" = {nav_panel( HTML(content$title),  textBoxUI(content$id,content$text,content$background))},
           "multibox" = {nav_panel( HTML(content$title), makeMultiBox(content,session) )},
           "advent" = {nav_panel( HTML(content$title), adventUI(content$id,content$img))}
    )
  }) #lapply
  
  navset_card_tab(
    full_screen = TRUE,
    title = text,
    !!!OptionList
  )
}


makeMultiNavBoxDash <- function(box,session){
  text =  HTML(box$title)
  OptionList <- lapply(box$boxcontent, function(content) {
    switch(content$type,
           "header" = {tabPanel( HTML(content$title),  h2(content$content))},
           "showdata" = {tabPanel( HTML(content$title), ShowDataUI(content$id,content$data))},
           "pivotdata" = {tabPanel( HTML(content$title), pivotDataUI(content$id,content$data))},
           "hist" = {tabPanel( HTML(content$title), histogramUI(content$id))},
           "text" = {tabPanel( HTML(content$title),  TextUI(content$id))},
           "downloadFile" = {tabPanel( HTML(content$title), DownloadFileUI(content$id,content$buttonTitle,content$pathFile))},
           "varfreq" = {tabPanel( HTML(content$title),  varfreqUI(content$id,content$data))},
           "varfreqGraph" = {tabPanel( HTML(content$title), varfreqGraphUI(content$id))},
           "varfreqTable" = {tabPanel(HTML(content$title), varfreqTableUI(content$id))},
           "crossvar" = {tabPanel( HTML(content$title),  crossvarUI(content$id,content$data))},
           "crossvartable" = {tabPanel( HTML(content$title), CrossVarUITable (content$id))},
           "crossvargraph" = {tabPanel( HTML(content$title), CrossVarUIGraph (content$id))},
           "image" = {tabPanel( HTML(content$title),  imageUI(content$id,content$link))},
           "video" = {tabPanel( HTML(content$title),  videoUI(content$id,content$link))},
           "textbox" = {tabPanel( HTML(content$title),  textBoxUI(content$id,content$text,content$background))},
           "multibox" = {tabPanel( HTML(content$title), makeMultiBox(content) )},
           "advent" = {tabPanel( HTML(content$title), adventUI(content$id,content$img))}
    )
  }) #lapply

  
  do.call(
    tabBox,
    c(
      title = text,  
      width = 12,                
      OptionList       
    )
  )
  
}