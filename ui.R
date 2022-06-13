library(shiny)
library(shinythemes)
library(ggplot2)
library(rsconnect)
library(plotly)
library("sf")
library(dplyr)
library(shinyWidgets)
library(viridis)

year_name <- list(
  "2021" = 2021,"2020" = 2020,"2019" = 2019,"2018" = 2018, 
  "2017" = 2017,"2016" = 2016,"2015" = 2015,"2014" = 2014, 
  "2013" = 2013,"2012" = 2012,"2011" = 2011,"2010" = 2010,
  "2009" = 2009,"2008" = 2008,"2007" = 2007,"2006" = 2006,
  "2005" = 2005,"2004" = 2004,"2003" = 2003,"2002" = 2002,
  "2001" = 2001,"2000" = 2000
)

country_name <- list(
  "Australia" = "Australia",
  "Bangladesh" = "Bangladesh",
  "Belgium" = "Belgium",
  "Brunei Darussalam" = "Brunei Darussalam",
  "Cambodia" = "Cambodia",
  "Canada" = "Canada",
  "China" = "China",
  "Denmark" = "Denmark",
  "Egypt" = "Egypt",
  "France" = "France",
  "Germany" = "Germany",
  "India" = "India",
  "Indonesia" = "Indonesia",
  "Iran" = "Iran",
  "Iraq" = "Iraq",
  "Ireland" = "Ireland",
  "Italy" = "Italy",
  "Japan" = "Japan",
  "Kazakhstan" = "Kazakhstan",
  "Lao PDR" = "Lao PDR",
  "Myanmar" = "Myanmar",
  "Nepal" = "Nepal",
  "Netherlands" = "Netherlands",
  "New Zealand" = "New Zealand",
  "Norway" = "Norway",
  "Pakistan" = "Pakistan",
  "Philippines" = "Philippines",
  "Poland" = "Poland",
  "Russian Federation" = "Russian Federation",
  "Saudi Arabia" = "Saudi Arabia",
  "Singapore" = "Singapore",
  "South Africa" = "South Africa",
  "South Korea" = "South Korea",
  "Spain" = "Spain",
  "Sri Lanka" = "Sri Lanka",
  "Sweden" = "Sweden",
  "Switzerland" = "Switzerland",
  "Taiwan" = "Taiwan",
  "Thailand" = "Thailand",
  "Turkey" = "Turkey",
  "UAE" = "UAE",
  "UK" = "UK",
  "Ukraine" = "Ukraine",
  "USA" = "USA",
  "Vietnam" = "Vietnam")


ui <- navbarPage(
  title = div
        (img 
          (src = "my01.png",
            height = "7%",
            width = "7%"
            ),
          "Touri-Scope (M16)"
        ),
  theme = shinytheme("cosmo"), 
  setBackgroundColor(color = c('#F6F6EB','#D5D6EA'),
                     gradient = "linear",
                     direction = "bottom"),
  tabPanel(
    "Dashboard", 
    titlePanel(div(
      windowTitle = "Tourism of Malaysia",
      img(src = "my.jpg",height = 350, width = "100%", class = "bg")
    )
    ),
    
    tabsetPanel(type = "tabs",
                tabPanel(
                  "Map Overview", 
                  sidebarLayout(
                    sidebarPanel(
                      column(
                        width = 12,
                        selectInput(
                          "selectyear",
                          label = "Select Year",
                          choices = unlist(year_name)),
                        actionButton(
                          width = "100%",
                          inputId = "submit_loc1",
                          label = "Select All Country"),
                        actionButton(
                          width = "100%",
                          inputId = "submit_loc2",
                          label = "Unselect All Country"),
                        actionButton(
                          width = "100%",
                          inputId = "submit_loc",
                          label = "Plot")
                      ),
                      
                      column(
                        tags$br(),
                        width = 12,
                        checkboxGroupInput(
                        "selectregion",
                        label = "Select Country",
                        choices = unlist(country_name)
                      ),
                      )
                    ),
                   mainPanel(
                     plotlyOutput("plot1",height = '100%', width = '100%'),
                     tags$br(),
                     tags$br(),
                     h2("Top countries visiting Malaysia"),
                     DT::dataTableOutput("table1")
                   )
                  )
                ),
                tabPanel(
                  "Yearly Overview",
                  sidebarLayout(
                    sidebarPanel(
                      column(
                        width = 12,
                        selectInput(
                          "selectregion1",
                          label = "Select (1) Country",
                          choices = unlist(country_name)
                        ),
                        actionButton(
                          width = "100%",
                          inputId = "submit_loc3",
                          label = "Plot"
                        )
                      ),
                    ),
                    mainPanel(
                      plotlyOutput("plot2",height = '100%', width = '100%'),
                      tags$br(),
                      tags$br(),
                      plotlyOutput("plot3",height = '100%', width = '100%'),
                      tags$br(),
                      tags$br(),
                      plotlyOutput("plot5", height = "100%", width = "100%")
                    )
                  )
                ),
                tabPanel(
                  "Monthly Overview",
                  sidebarLayout(
                    sidebarPanel(
                      column(
                        width = 12,
                        selectInput(
                          "selectregion2",
                          label = "Select Country",
                          choices = unlist(country_name)),
                        checkboxGroupInput(
                          "selectyear1",
                          label = "Select Year",
                          choices = unlist(year_name)),
                        actionButton(
                          width = "100%",
                          inputId = "submit_loc4",
                          label = "Plot"),
                        actionButton(
                          width = "100%",
                          inputId = "submit_loc5",
                          label = "Select All Year"),
                        actionButton(
                          width = "100%",
                          inputId = "submit_loc6",
                          label = "Unselect All Year"),
                      )
                    ),
                    mainPanel(
                      plotlyOutput("plot4",height = '100%', width = '100%'),
                      tags$br(),
                      h2("World Events from Wikipedia"),
                      DT::dataTableOutput("table2")
                    )
                  )
                )
    )
  ),
  tabPanel(
    "User Manual", 
    fluidPage(htmlOutput("usermanual"))
  ),
  tabPanel(
    "Process", 
    fluidPage(htmlOutput("process"))
  ),
  tabPanel(
    "About", 
    fluidPage(htmlOutput("about"))
  ),
  
  tags$head(tags$style('.navbar-nav {width: 95%;}
                                    .navbar-nav :first-child{float:right}'))
)
