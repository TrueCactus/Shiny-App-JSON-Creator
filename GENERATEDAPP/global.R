#global.R     
  
library(plotly)
library(xts)
library(shiny)
#library(shinydashboard)
library(shinyjs)
library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(htmltools)
library(knitr)
library(kableExtra)
library(DT)
library(RColorBrewer)
library(SnowballC)
library(data.table)
library(formattable)
library(grid)
library(highcharter)
library(lubridate)
library(magrittr)
library(openxlsx)
library(reshape2)
library(rlist)
library(scales)
library(tm)
library(vcd)
library(wordcloud)
library(ggpubr)
library(FactoMineR)
library(Hmisc)
library(gt)
library(leaflet)
library(rjson)
library(shinymanager)
library(bslib)
library(GWalkR)
library(bsicons)
library(rlang)
library(rpivotTable)

library(DBI)
library(RPostgres)
library("RPostgreSQL")
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)

for (f in list.files("./MODULES","*.R")) {
  source(paste0("./MODULES/",f))
}

library(shiny)
library(fresh) 
library(bs4Dash)
library(tidyverse)
library(graphics)
library(gt)
library(shinymanager)   

set_labels(
  language = "en",
  "Please authenticate" = "Please authenticate and try with the different users",
  "Username:" = "Username",
  "Password:" = "Password"
)


# ui fonction

#' makeNavUi
#' it makes the UI when the display is in mode "nav". It uses the description json
#' to construct the menu and pages.
#' @param ui_json 
#' @param session 
#' @param theme 
#' @return ui_app
makeNavUi <- function(ui_json,session,theme){
  # List : lien entre tabs du menu et  contenu des pages
  name_vector <- c()
  for (i in 1: length(ui_json$tabs)){
    name <- ui_json$tabs[[i]]$name
    name_vector <- append(name_vector,name)
  }
  name_list <- setNames(1:length(name_vector), name_vector)
  
  
  # Création de la navbar
  # tabs 
  tabs <- lapply(ui_json$Menu, function(tab) {
    if(is.null(tab$subitem)){
      page_index <- name_list[[tab$name]]
      page <- ui_json$tabs[[page_index]]
      tabContent <- card(makeContent(page,session),class = "custom-box")
      tabPanel(tab$title, tabContent)
    }
    else{
      subMenu <- tab$subitem
      subMenuTabs <-  lapply(subMenu, function(tab) {
        page_index <- name_list[[tab$name]]
        page <- ui_json$tabs[[page_index]]
        tabContent <- card(tabContent <- makeContent(page,session),width = 12,class = "custom-box")
        tabPanel(tab$title, tabContent)
      }
      )
      
      subMenuTabs_args <- c(list(title = tab$title), subMenuTabs)
      do.call(shiny::navbarMenu,subMenuTabs_args)
      
      
    }
  }) 
  
  
  navbar_args <- c(list(title = ui_json$title, theme = theme ), tabs)
  
  
  # Utiliser do.call pour créer l'ui
  ui_app <- do.call(navbarPage, navbar_args)
  
  return(ui_app)
}

#' makeDashUi
#' it makes the UI when the display is in mode "dash". It uses the description json
#' to construct the menu and pages.
#' @param ui_json 
#' @param session 
#' @return ui_app
MakeDashUi <- function(ui_json,session){
  ui_app <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = ui_json$title,
        color = "primary",
        href = ui_json$link,
        image = ui_json$image),
      disable = FALSE,
      skin = "light",
      status = "primary",
      border = TRUE,
      compact = FALSE,
      sidebarIcon = shiny::icon("bars"),
      controlbarIcon = shiny::icon("th")
    ),
    sidebar = dashboardSidebar(
      bs4SidebarMenu(
        id = "sidebar",
        lapply(ui_json$Menu, function(tab) {
          if (is.null(tab$subitem)) {
            bs4SidebarMenuItem(tab$title, tabName = tab$name, icon = icon(tab$status))
          } else {
            
            subT <- lapply(tab$subitem, function(subtab) {
              bs4SidebarMenuSubItem(subtab$title, tabName = subtab$name)
            })
            # Change by Aurelien (30/08/2024)
            subMenuTabs_args <- c(list(text = tab$title,startExpanded = FALSE,icon = icon(tab$status)),subT)
            #subMenuTabs_args <- c(list(text = tab$title,startExpanded = TRUE,icon = icon(tab$status)),subT)
            do.call(bs4SidebarMenuItem,subMenuTabs_args)
            
          }
        })
      )
    ),
    body = bs4Dash::bs4DashBody(
      useShinyjs(),
      tags$div(
        tagList(
          lapply(ui_json$tabs, function(tab) {
            bs4TabItem(
              tabName = tab$name,
              makeContent(tab,session)
            )
          })
        ),
        class = "tab-content"
      )
    ),
    controlbar = dashboardControlbar(),
    title = ui_json$title
  )
}