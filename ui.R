#ui.R                                                                          
# UI for nav auth

# Empty UI , UI is made by the servr according to the user.
ui <- renderUI(uiOutput("MainUI"))


# Shinymanager Part 
secure_app(ui,enable_admin = FALSE,
           
            
           
           tags_top =
             tags$div(
               tags$h4("WELCOME !"),
               tags$h5("The differents users are - appcreator /appcreatorpwd
   - example1nav or example1dash / example1pwd -
   - exampletitanic/exampletitanicpwd"),
               tags$img(
                 src = "https://avatars.githubusercontent.com/u/44371566?v=4", width = 150
               )
             ),
           
           
           tags_bottom = tags$div())
