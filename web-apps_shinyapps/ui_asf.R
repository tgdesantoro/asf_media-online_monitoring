
#UI = user interface

install.packages('ggpubr') #facilitate graphic visualization from ggplot2
install.packages('shiny') #built interactive web application
install.packages("shinydashboard") #support shiny to make structured and elegant dashboard 
install.packages('leaflet') #make interactive and dynamic map 
install.packages('ggthemes') #serve additional themes and colour scale
install.packages('tidyverse') #create manipulation process, transform, and data visualisation
install.packages("DT")
install.packages("plotly")

library(ggplot2) #data visualization
library(ggpubr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf) #read spatial data
library(ggthemes)
library(tidyverse)
library(rvest)
library(DT) #display data interactive in console R
library(plotly)
library(readr)

asf_indonesia <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/ind/001_data-step_last-update/004-asf-selected-range21days_wa.csv")
asf_malaysia <- read.csv2("D:/010_african_swine_fever/003_media-monitoring_asf/data/mys/004-asf_news_raw_my250425_wa.csv")
View(asf_indonesia)
View(asf_malaysia)

# Now, define your UI

ui <- dashboardPage(
  skin = "red",
  
  dashboardHeader(title = "African Swine Fever in Southeast Asia", titleWidth = 650),
  
  dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                menuItem("Home", icon = icon("home"),
                         menuSubItem("About ASF", tabName = "about_asf", icon = icon("book-open")),
                         menuSubItem("Team", tabName = "team", icon = icon("users")),
                         menuSubItem("Method", tabName = "method", icon = icon("cogs")),
                         menuSubItem("Update", tabName = "update", icon = icon("newspaper"))
                ),
                menuItem("Statistic & Graphic", tabName = "chart2", icon = icon("chart-bar")),
                menuItem("Database", tabName = "db", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ==== About ASF ====
      tabItem(tabName = "about_asf",
              h3("About ASF"),
              box(width = 12, status = "primary", solidHeader = TRUE,
                  h4("Media Monitoring of African Swine Fever (ASF)"),
                  p("Media monitoring is an approach used to systematically track and analyze publicly available online news articles and reports related to ASF. 
                     This process allows us to gather valuable insights on the disease dynamics across different spatial and temporal scales."),
                  p("Through this method, we identify:"),
                  tags$ul(
                    tags$li("🗺️ Geographic distribution of ASF reports, down to subdistrict (kecamatan/mukim) level"),
                    tags$li("🐖 Species affected — primarily domestic pigs and wild boars"),
                    tags$li("📅 Time-based patterns — frequency and peak periods of outbreaks"),
                    tags$li("🔗 Source credibility and narrative tone of the media coverage")
                  ),
                  br(),
                  h4("Summary of Media Monitoring Results"),
                  p("The following table summarizes ASF-related reports from Indonesia and Malaysia, collected via online media monitoring."),
                  p("It highlights the number of cases, geographic scope, species affected, and overall report counts by country."),
                  tableOutput("summary_table_asf")
              )
      ),
      
      # ==== Team ====
      tabItem(tabName = "team",
              h3("Our Research & Development Team"),
              tags$img(src = "team_photo.jpg", height = "300px"),
              p("This working group brings together experts from across the region to collaboratively develop technical assistance materials to support an improved management plan to mitigate ASF’s impacts in wild pig species. The group will assist in the creation of low-cost and replicable surveillance systems to detect and control the disease; best practices and communications to increase multi-sectoral recognition; and complete a Red List re-assessment of one wild pig species in South-East Asia. Through the engagement of community partners, the group will develop technical assistance to test and equip stakeholders with new tools, technologies and approaches, ultimately capturing lessons learned in a science-based ASF management plan.")
      ),
      
      # ==== Method ====
      tabItem(tabName = "method",
              h3("Metodologi"),
              uiOutput("metode_text")
      ),
      
      # ==== Update ====
      tabItem(tabName = "update",
              h3("Latest ASF News"),
              tags$ul(
                tags$li(a(href = "https://rr-asia.woah.org/app/uploads/2024/12/3.1-FAO_ASF-Updates.pdf", target = "_blank", "Supporting management of African swine fever (ASF) in Asia Pacific")),
                tags$li(a(href = "https://www.oie.int/en/disease/african-swine-fever/", target = "_blank", "WOAH - ASF Info")),
                tags$li(a(href = "https://www.fao.org/animal-health/en/", target = "_blank", "FAO on ASF"))
              )
      ),
      
      # ==== Statistic & Graphic ====
      tabItem(tabName = "chart2",
              tabBox(id = "chart_tabbox", width = 12,
                     
                     # INDONESIA
                     tabPanel("Indonesia",
                              fluidRow(
                                box(title = "Filter", status = "warning", solidHeader = TRUE, width = 4,
                                    selectInput("province", "Select province:", choices = NULL),
                                    selectInput("regency", "Select regency:", choices = NULL),
                                    selectInput("subdistrict", "Select subdistrict:", choices = NULL),
                                    selectInput("species", "Select species:", choices = NULL),
                                    hr(),
                                    helpText("Filter berdasarkan wilayah dan spesies")
                                ),
                                box(title = "Jumlah Kasus Berdasarkan Filter", status = "success", solidHeader = TRUE, width = 8,
                                    plotOutput("filteredPlot", height = 300)
                                )
                              ),
                              fluidRow(
                                box(title = "Number of ASF Case (2019–2025)", status = "info", solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("speciesHistogram", height = 350)
                                )
                              )
                     ),
                     
                     # MALAYSIA
                     tabPanel("Malaysia",
                              fluidRow(
                                box(title = "Filter", status = "warning", solidHeader = TRUE, width = 4,
                                    selectInput("state", "Select state:", choices = NULL),
                                    selectInput("district", "Select district:", choices = NULL),
                                    selectInput("mukim", "Select mukim:", choices = NULL),
                                    selectInput("species_malaysia", "Select species:", choices = NULL),
                                    hr(),
                                    helpText("Filter berdasarkan wilayah dan spesies")
                                ),
                                box(title = "Jumlah Kasus Berdasarkan Filter", status = "success", solidHeader = TRUE, width = 8,
                                    plotOutput("filteredPlot_malaysia", height = 300)
                                )
                              ),
                              fluidRow(
                                box(title = "Number of ASF Case (2019–2025)", status = "info", solidHeader = TRUE,
                                    width = 12,
                                    plotOutput("speciesHistogram_malaysia", height = 350)
                                )
                              )
                     )
              )
      ),
      
      # ==== Database ====
      tabItem(tabName = "db",
              fluidRow(
                tabBox(
                  id = "tabchart1",
                  width = 12,
                  title = "Database ASF by Country",
                  
                  tabPanel("Indonesia",
                           h4("ASF data in Indonesia"),
                           downloadButton("download_indonesia", "Download CSV", class = "btn-primary"),
                           br(), br(),
                           DTOutput("Tab1")
                  ),
                  
                  tabPanel("Malaysia",
                           h4("ASF data in Malaysia"),
                           downloadButton("download_malaysia", "Download CSV", class = "btn-success"),
                           br(), br(),
                           DTOutput("Tab2")
                  )
                )
              )
      )
    )
  )
)
