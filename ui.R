library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("CRaFT Grower Dashboard"),
  # add logout button UI
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  # add login panel UI function
  shinyauthr::loginUI(
    id = "login",
    title = "Log in with your Kobo credentials",
    user_title = "Username"
    ),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      uiOutput("contract_list")
    ),
    mainPanel = mainPanel(
      leafletOutput("map"),
      fluidRow(
        column(6, uiOutput("submissions")),
        column(6, tableOutput("table"))
      )
    )
  )
)

