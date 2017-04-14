dashboardPage(
  dashboardHeader(title="Custom Icons"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Light icons", tabName = "light"),
      menuItem("Dark icons", tabName = "dark")
    ),
    div(a(href=post, "Related blog post"), style="width: 80%; padding: 15px"),
    div(a(href=gist, "Github gist"), style="width: 80%; padding: 15px")
  ),
  dashboardBody(
    tags$head( # must include css
      tags$style(HTML("
        .img-local {
        }
        
        .small-box .img-local {
        position: absolute;
        top: auto;
        bottom: 5px;
        right: 5px;
        z-index: 0;
        font-size: 70px;
        color: rgba(0, 0, 0, 0.15);
        }"
      ))
    ),
    tabItems(
      tabItem(tabName = "light",
              fluidRow(valueBoxOutput("distLight", width=3)),
              fluidRow(
                box(plotOutput("hist1"),
                    br(),
                    h4("Some random values for the bottom six value boxes showing delta change:"),
                    verbatimTextOutput("vals1"), status="primary", width=6),
                box(uiOutput("vBoxesLight"), status="primary", width=6)
              )
      ),
      tabItem(tabName = "dark",
              fluidRow(valueBoxOutput("distDark", width=3)),
              fluidRow(
                box(plotOutput("hist2"), 
                    br(),
                    h4("Some random values for the bottom six value boxes\nshowing delta change:"),
                    verbatimTextOutput("vals2"), status="primary", width=6),
                box(uiOutput("vBoxesDark"), status="primary", width=6)
              )
      )
    )
  ),
  title="Custom icons"
)