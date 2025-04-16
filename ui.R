library(shiny)
library(leaflet)
library(bslib)

# ui <- fluidPage(
#   titlePanel("CRaFT Grower Dashboard"),
#   # add logout button UI
#   div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
#   # add login panel UI function
#   shinyauthr::loginUI(
#     id = "login",
#     title = "Log in with your Kobo credentials",
#     user_title = "Username"
#     ),
#   sidebarLayout(
#     sidebarPanel = sidebarPanel(
#       uiOutput("contract_list")
#     ),
#     mainPanel = mainPanel(
#       leafletOutput("map"),
#       fluidRow(
#         column(6, uiOutput("submissions")),
#         column(6, tableOutput("table"))
#       )
#     )
#   )
# )

ui <- page_sidebar(
  title = "CRaFT Data Dashboard",
  sidebar = sidebar(
    shinyauthr::loginUI(
      id = "login",
      title = NULL,
      user_title = "Username",
      pass_title = "Password"
    ),    
    #card(
      shinyauthr::logoutUI(id = "logout"),
      uiOutput("user_info"),
    #),
    uiOutput("contract_list"),
    open = F
  ),
  leafletOutput("map"),
  fluidRow(
    column(6, uiOutput("submissions")),
    column(6, tableOutput("table"))
  )
)