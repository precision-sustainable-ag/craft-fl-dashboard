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

vbs <- list(
  value_box(
    "Total acres planted",
    value = textOutput("acres_total"),
    theme = "primary",
    showcase = icon("seedling")
  ),
  value_box(
    "Enrollment over time",
    value = textOutput("acres_years_avg"),
    theme = "secondary",
    showcase = plotlyOutput("acres_years")
  ),
  card(
    plotlyOutput("rootstocks_pie")
  ),
  card(
    plotlyOutput("scions_pie")
  )
  # value_box(
  #   "Popular rootstocks",
  #   value = textOutput("top_rootstock"),
  #   theme = "secondary",
  #   showcase = plotlyOutput("rootstocks_pie"),
  #   showcase_layout = "top right"
  # )
)

ui <- page_sidebar(
  title = div(
    span("CRaFT Data Dashboard"),
    span(input_dark_mode(id = "theme"), style = "padding-left: 20px;")
    ),
  sidebar = sidebar(
    shinyauthr::loginUI(
      id = "login",
      title = NULL,
      user_title = "Username",
      pass_title = "Password"
    ),    
    
    shinyauthr::logoutUI(id = "logout"),
    uiOutput("user_info"),
    
    uiOutput("contract_list"),
    open = F,
    width = 350
  ),
  layout_column_wrap(
    !!!vbs
  ),
  leafletOutput("map")
  # fluidRow(
  #   column(6, uiOutput("submissions")),
  # )
)