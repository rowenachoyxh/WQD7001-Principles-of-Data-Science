library(shiny)
library(shinythemes)
library(ggplot2)
library(rsconnect)
library(plotly)
library("sf")
library(dplyr)
library(viridis)

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

year_name <- list(
  "2021" = 2021,"2020" = 2020,"2019" = 2019,"2018" = 2018, 
  "2017" = 2017,"2016" = 2016,"2015" = 2015,"2014" = 2014, 
  "2013" = 2013,"2012" = 2012,"2011" = 2011,"2010" = 2010,
  "2009" = 2009,"2008" = 2008,"2007" = 2007,"2006" = 2006,
  "2005" = 2005,"2004" = 2004,"2003" = 2003,"2002" = 2002,
  "2001" = 2001,"2000" = 2000
)

yearly_data <- readRDS("tourism_yearly.rds")
monthly_data <- readRDS("tourism_monthly.rds")
world_event <- readRDS("world_event.rds")

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

server <- function (input,output,session){
  
  observe({
    if(input$submit_loc1 == 0) 
      return(NULL)
    else if (input$submit_loc1 > 0)
    {
      updateCheckboxGroupInput(session, "selectregion",
                               label = "Select Country",
                               choices = country_name, 
                               selected = unlist(country_name))
    }
  })
  
  observe({
    if(input$submit_loc2 == 0) 
      return(NULL)
    else if (input$submit_loc2 > 0)
    {
      updateCheckboxGroupInput(session, "selectregion",
                               label = "Select Country",
                               choices = country_name, 
                               selected = c())
    }
  })
  
  observe({
    if(input$submit_loc5 == 0) 
      return(NULL)
    else if (input$submit_loc5 > 0)
    {
      updateCheckboxGroupInput(session, "selectyear1",
                               label = "Select Year",
                               choices = year_name, 
                               selected = unlist(year_name))
    }
  })
  
  observe({
    if(input$submit_loc6 == 0) 
      return(NULL)
    else if (input$submit_loc6 > 0)
    {
      updateCheckboxGroupInput(session, "selectyear1",
                               label = "Select Year",
                               choices = year_name, 
                               selected = c())
    }
  })
  
  ################################################
  ####           Panel: Map Overview          ####
  ################################################
  
  country <- eventReactive(input$submit_loc, 
                           {
                             data1 = yearly_data[yearly_data$Country %in% input$selectregion, ]
                             data2 = data1[data1$Year %in% input$selectyear, ]
                             return (data2)
                           })
  
  output$plot1 <- renderPlotly({
    
    g <- list(
      showlakes = FALSE
    )
    
    df <- country()
    
    fig <- plot_ly(type = "choropleth", 
                   locations = df$CountryCode,
                   z = df$Arrivals, 
                   text = df$Country,
                   colors = turbo(50, alpha = 1, begin = 0, end = 1, direction = 1)) 
    
    fig <- fig %>% layout(geo = g,
                          title = paste('Map of Tourist Arrivals to Malaysia in year',input$selectyear))
                          
    fig <- fig %>% colorbar(title = "Number of Arrivals")
    
    fig
  })
  
  region_yearly_data <- eventReactive(input$submit_loc,
                                      {
                                        data1 = yearly_data[yearly_data$Country %in% input$selectregion, ]
                                        data2 = data1[data1$Year %in% input$selectyear, ]
                                        data3 = subset(data2, select = c('Country', 'Region','Arrivals'))
                                        data4 = arrange(data3,desc(Arrivals))
                                        return (data4)
                                      })
  
  output$table1 <- DT::renderDataTable(
    region_yearly_data(), rownames = FALSE
  )
  
  ################################################
  ####        Panel: Yearly Overview          ####
  ################################################
  
  country2 <- eventReactive(input$submit_loc3, 
                           {
                             data1 = yearly_data[yearly_data$Country %in% input$selectregion1, ]
                             return (data1)
                           })

  output$plot2 <- renderPlotly({
    
    fig <- country2()
    
    fig <- fig %>% accumulate_by(~Year)
    fig <- fig %>%
      plot_ly(
        x = ~Year, 
        y = ~Arrivals,
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines', 
        fill = 'tozeroy',
        fillcolor = 'rgba(128,0,128,0.5)',
        line = list(simplyfy = F)
      )
    fig <- fig %>% layout(
      xaxis = list(
        title = "Year",
        zeroline = T
      ),
      yaxis = list(
        title = "Number of Arrivals",
        zeroline = T
      )
    )
    fig <- fig %>% layout(
      title = paste('Arrivals from', input$selectregion1, '(Year 2000 to 2021)')
    )
    fig <- fig %>% animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE
    )
    fig <- fig %>% animation_slider(
      hide = T
    )
    fig <- fig %>% animation_button(
      x = 1, xanchor = "right", y = 2, yanchor = "top"
    )
    
    fig
  })
  
  output$plot3 <- renderPlotly({
    fig <- country2()
    fig1 <- as.data.frame(fig)
    fig1 <- fig1 %>% mutate(Percent = (Arrivals - lag(Arrivals))/lag(Arrivals) * 100)
    fig1 <- fig1[-1,]
  
    fig3 <- plot_ly(
      data = fig1,
      x = ~Year,
      y = ~Percent,
      type = 'bar',
      color = ~Percent,
      colors = c("red","orange","yellow","green","blue")
    )
    fig3 <- fig3 %>% layout(
      title = paste("Yearly percentage change from 2000 to 2021 for", input$selectregion1)
    )
    fig3 
    
  })
  
  output$plot5 <- renderPlotly({
    
    df <- country2()
    df1 <- as.data.frame(df)
    
    mean_value    <- mean(df1$Arrivals)
    min_value     <- min(df1$Arrivals)
    max_value     <- max(df1$Arrivals)
    sd_value      <- sd(df1$Arrivals)
    
    
    fig1 <- plot_ly (
      domain = list(x = c(0.1,0.4), y = c(0.5,0.8)),
      value = mean_value,
      type = "indicator",
      title = list(text = "Mean"),
      mode = "gauge+number"
    )

    fig2 <- plot_ly (
      domain = list(x = c(0.1,0.4), y = c(0,0.3)),
      value = min_value,
      type = "indicator",
      title = list(text = "Minimum"),
      mode = "gauge+number"
    )
   
    fig3 <- plot_ly (
      domain = list(x = c(0.6,0.9), y = c(0,0.3)),
      value = max_value,
      type = "indicator",
      title = list(text = "Maximum"),
      mode = "gauge+number"
    )
   
    fig4 <- plot_ly (
      domain = list(x = c(0.6,0.9), y = c(0.5,0.8)),
      value = sd_value,
      type = "indicator",
      title = list(text = "Standard Deviation"),
      mode = "gauge+number"
    )
    
    fig <- subplot(fig1,fig2,fig3,fig4,nrows = 4)
    
    fig <- fig %>% layout (
      title = paste("Statistical description for",input$selectregion1,"from year 2000 to 2021")
    )
    
    fig
  })
  
  ################################################
  ####        Panel: Monthly Overview         ####
  ################################################
  
  country3 <- eventReactive(input$submit_loc4, 
                           {
                             data1 = monthly_data[monthly_data$Country %in% input$selectregion2, ]
                             data2 = data1[data1$Year %in% input$selectyear1, ]
                             return (data2)
                           })
  
  output$plot4 <- renderPlotly({

    fig <- country3()
    
    fig$month <- match(fig$month,month.abb)
    
    fig <- fig %>% accumulate_by(~month)
    fig <- fig %>%
      plot_ly(
        x = ~month, 
        y = ~Arrivals,
        split = ~Year,
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F)
      )
    fig <- fig %>% layout(
      xaxis = list(
        title = "Month",
        zeroline = T,
        tickmode = 'linear'
      ),
      yaxis = list(
        title = "Number of Arrivals",
        zeroline = T
      )
    )
    fig <- fig %>% layout(
      title = paste("Monthly Arrivals from",input$selectregion2)
    )
    fig <- fig %>% animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE
    )
    fig <- fig %>% animation_slider(
      hide = T
    )
    fig <- fig %>% animation_button(
      x = 1, xanchor = "right", y = 2, yanchor = "top"
    )
    
    fig
  })
  
  world_event_data <- eventReactive(input$submit_loc4,
                                      {
                                        data1 = world_event[world_event$Year %in% input$selectyear1, ]
                                        return (data1)
                                      })
  
  output$table2 <- DT::renderDataTable(
   world_event_data(), rownames = FALSE
   
  )
  ################################################
  ####           Panel: User Manual           ####
  ################################################
  
  getPageDoc1 <- function(){
    return(includeHTML("User-Manual.html"))
  }
  output$usermanual <- renderUI({
    getPageDoc1()
  })
  
  ################################################
  ####             Panel: Process             ####
  ################################################
  
  getPagePro <- function() {
    return(includeHTML("process.html"))
  }
  output$process <- renderUI({
    getPagePro()
  })
  
  ################################################
  ####            Panel: About                ####
  ################################################
  
  getPageAbo <- function() {
    return(includeHTML("about.html"))
  }
  output$about <- renderUI({
    getPageAbo()
  })
  
  
}
