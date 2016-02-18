

dashboardPage(title = "OECD",
  skin = "blue",
  dashboardHeader(title = "OECD", titleWidth=400),
  
  dashboardSidebar(
    includeCSS("custom.css"),
    uiOutput("a"),
    
    sidebarMenu(
      id = "sbMenu",
      
      menuItem(
        "Migration",tabName= "migration"
     
      ),
      
  #    menuItem("Info", tabName = "info", icon = icon("info")),
      

tags$hr(),
menuItem(text="",href="https://mytinyshinys.shinyapps.io/dashboard",badgeLabel = "All Dashboards and Trelliscopes (14)"),
tags$hr(),

tags$body(
     a(class="addpad",href="https://twitter.com/pssGuy", target="_blank",img(src="images/twitterImage25pc.jpg")),
     a(class="addpad2",href="mailto:agcur@rogers.com", img(src="images/email25pc.jpg")),
     a(class="addpad2",href="https://github.com/pssguy",target="_blank",img(src="images/GitHub-Mark30px.png")),
     a(href="https://rpubs.com/pssguy",target="_blank",img(src="images/RPubs25px.png"))
)

      
    
  )),
  dashboardBody(
    tabItems(
    tabItem("migration",
            fluidRow(
              column(width=6,
                     box(width=12,
                     status = "success",solidHeader = FALSE,
                     footer="Hover for Details. Click Country for Source Map",
            plotlyOutput('mapTo'))
            ),
            column(width=6,
                   box(width=12,
                       status = "success",solidHeader = FALSE,
                       footer="Hover for Details.",
                       plotlyOutput('mapFrom'))
            )
            )
            
    )#,
  #  tabItem("info", includeMarkdown("info.md"))
    
    
    
  ) # tabItems
  ) # body
  ) # page
  