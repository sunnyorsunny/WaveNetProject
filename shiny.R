#dashboard
library(RODBC)
setwd('C:/Users/user/Desktop/shiny/wavenet')

channel <- odbcConnect('LAPTOP-R9B4RGIC', uid='LAPTOP-R9B4RGIC/Jenny', pwd='')
library(ggplot2)
library(shiny)
library(GGally)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(rsconnect)
library(RgoogleMaps)
library(png)
Sys.setlocale(locale="English")

#rsconnect::setAccountInfo(name='sammi20916', token='183F1F2A16D1B0E26D9BFB4D1BF24991', secret='39oTckvk+qw03Uqcd08Py/BOJVNfHA7CDn6BN//H')
#shiny
ui <- dashboardPage(skin="blue",
                    dashboardHeader(title = "Fundraising Platform Project Tracking"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Variable performance of projects", tabName = "projectstatus", icon = icon("money-check-alt")),
                        menuItem("information inquiry", tabName = "members", icon = icon("user"),
                                 badgeLabel = "new", badgeColor = "green"),
                        menuItem("Distribution of top10 contributing members", tabName = "taiwan", icon = icon("map-marker-alt"),
                                 badgeLabel = "new", badgeColor = "green")
                      )),
                    dashboardBody(
                      tabItems(
                        # first tab content
                        tabItem(tabName = "projectstatus",
                                # Boxes need to be put in a row (or column)
                                fluidRow(
                                  box(title = "Project Status's Scatter Plot"),
                                  mainPanel(plotlyOutput("plot", width = "1000px", height = "600px"),
                                            plotlyOutput("plot2", width = "1000px", height = "600px"),
                                            plotlyOutput("plot3", width = "1000px", height = "600px"),
                                            plotlyOutput("plot4", width = "1000px", height = "600px"))
                                )),
                        
                        # second tab content
                        tabItem(tabName = "members",
                                fluidRow(
                                  box(title = 'information inquiry'),
                                  mainPanel(tabsetPanel(tabPanel("Members contribution",
                                                                 br(),DT::dataTableOutput("mytable")),
                                                        tabPanel("Projects contribution", br(), DT::dataTableOutput("mytable2"))))
                                )),
                        
                        #third tab content
                        tabItem(tabName ="taiwan", 
                                fluidRow(
                                  box(title = "Where are the investors?"),
                                  mainPanel(imageOutput("image", height = 200)))
                      ))))

server <-  function(input, output, session) {
  members_data <- reactive({
    invalidateLater(600000)  #1000毫秒之后重新执行
    members <- sqlQuery(channel, "SELECT A.[member_id]
                                    ,[sex] 
                                	  ,min(cast(iif([register_date] = 'NULL', null, [register_date]) as datetime)) [register_date]
                                    ,max(cast(iif([last_login_date] = 'NULL', null, [last_login_date])as datetime)) [last_login_date]
                                	  ,max(iif([birth] = 'NULL', null, [birth])) [birth]
                                	  ,count(B.raise_funds) raise_funds_times
                                	  ,sum(raise_funds) raise_funds
                                  FROM [wavenet].[dbo].[members] A
                                  left join [wavenet].[dbo].[SponsorRecords] B
                                  on A.member_id = B.member_id
                                  group by A.member_id, sex
                                  order by raise_funds desc")
    members$birth <- gsub('-','',as.character(members$birth))
    members$birth <- as.numeric(substring(members$birth, 4, 8))
    members$birth <- ifelse(members$birth < 101 , 'unknown',
                            ifelse(members$birth < 121, 'Capricorn',
                                   ifelse(members$birth < 220, 'Aquarius',
                                          ifelse(members$birth < 321, 'Pisces',
                                                 ifelse(members$birth < 420, 'Aries',
                                                        ifelse(members$birth < 521, 'Taurus',
                                                               ifelse(members$birth < 622, 'Gemini',
                                                                      ifelse(members$birth < 723, 'Cancer',
                                                                             ifelse(members$birth < 823, 'Leo',
                                                                                    ifelse(members$birth < 923, 'Virgo',
                                                                                           ifelse(members$birth < 1024, 'Libra',
                                                                                                  ifelse(members$birth < 1122, 'Scorpio',
                                                                                                         ifelse(members$birth < 1221, 'Sagittarius','Capricorn')))))))))))))
    
    
    members$birth[is.na(members$birth)] <- 'unknown'
    members$register_date <- as.Date(members$register_date)
    members
  })
  projects_data <- reactive({
    invalidateLater(600000)  #1000毫秒之后重新执行
    ProjectsStatus <- sqlQuery(channel, "SELECT [project_id]
                                            ,cast(iif([projects_days] = 'NULL', null, [projects_days]) as int) [projects_days]
                                            ,[sponsors]
                                            ,[raise_funds]
                                            ,iif([project_status] = 4, 'fail', iif(project_status = 5, 'success', 'others')) project_status
                                        FROM [wavenet].[dbo].[ProjectsStatus]")
    ProjectsStatus
    })
  
  output$plot <- renderPlotly({
    ProjectsStatus <- sqlQuery(channel, "SELECT [project_id]
                                            ,cast(iif([projects_days] = 'NULL', null, [projects_days]) as int) [projects_days]
                                            ,[sponsors]
                                            ,[raise_funds]
                                            ,iif([project_status] = 4, 'fail', iif(project_status = 5, 'success', 'others')) project_status
                                        FROM [wavenet].[dbo].[ProjectsStatus]")
    ProjectsStatus10 <- ProjectsStatus[complete.cases(ProjectsStatus),]
    plot_ly(ProjectsStatus10, x = ~raise_funds, y = ~projects_days, color = ~project_status,
            size = ~projects_days) 
  })
  
  output$plot2 <- renderPlotly({
    # 變數間關係
    ggpairs(ProjectsStatus10, mapping = aes(color = project_status)) 
  })
  
  output$plot3 <- renderPlotly({
    ggplot(ProjectsStatus10, aes(x = raise_funds , y = projects_days, fill = as.factor(project_status), colour = as.factor(project_status))) +
      geom_point(position = "identity", alpha = 0.4)
  })
  
  output$plot4 <- renderPlotly({
    #
    ggplot(ProjectsStatus10, aes(x = projects_days , fill = as.factor(project_status))) +
      geom_histogram(position = "identity", alpha = 0.4, bins = 10)
  })
  
  output$image <- renderImage({
   
    
    list(src = "images/taiwan.png",
        contentType = "image/png",
        alt = "This is Taiwan.")
      
  }, deleteFile = FALSE)

  output$mytable <- DT::renderDataTable({
      datatable(members_data())  
  })
  
  output$mytable2 <- DT::renderDataTable({
    datatable(projects_data())  
  })
  
}

options(shiny.port=3705)
#options(shiny.host='123.205.12.53')
options(shiny.host='192.168.0.100')
shinyApp(ui, server)                    

