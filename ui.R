library(shiny)
library(shinydashboard)
library(DT)
library(ggvis)
library(shinyWidgets)

shinyUI(tagList(
  dashboardPage(skin = "yellow",
                
                dashboardHeader(title = "BIAT"),
                dashboardSidebar(width = 240, sidebarMenu(
                  menuItem("Instructions", tabName = "genIns", icon = icon("info-circle")),
                  menuItem("Data", tabName = "uploadData", icon = icon("table")),
                  menuItem("Quality of portfolio", tabName = "taux",icon = icon("area-chart"))
                  
                  
                  
                  
                )),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "genIns",
                            fluidPage(
                              titlePanel("General Instruction will go here"),
                              textOutput("currentTime")
                           
                            
                            )
                    ),
                    # First tab content
                    tabItem(tabName = "uploadData",
                            
                            fluidRow(width=15,tabPanel(title="Upload Your Data", 
                                                       box(width = 15,
                                                           solidHeader = F,collapsible = TRUE,  status = "primary",background = "navy",
                                                           fluidPage(uiOutput('box1')),
                                                           
                                                           fileInput('file1', 'Choose CSV File',
                                                                     accept=c('text/csv',
                                                                              'text/comma-separated-values,text/plain',
                                                                              '.csv')),
                                                           
                                                           
                                                           checkboxInput('header', 'Header', TRUE),
                                                           
                                                           checkboxInput('timestamp', 'TIMESTAMP', TRUE),
                                                           
                                                           
                                                           radioButtons('sep', 'Separator',
                                                                        c(Comma=',',
                                                                          Semicolon=';',
                                                                          Tab='\t'),
                                                                        ';'),
                                                           radioButtons('quote', 'Quote',
                                                                        c(None='',
                                                                          'Double Quote'='"',
                                                                          'Single Quote'="'"),
                                                                        '')
                                                           
                                                           
                                                           
                                                           
                                                       ))),
                            fluidRow(
                              box(width = 15, solidHeader = F,collapsible = TRUE,  status = "primary",background = "navy",
                                  
                                  #actionButton("update", "Update View", icon = icon("refresh")),
                                  
                                  uiOutput("vars")
                                  
                                  
                                  
                              )),
                            fluidRow(
                              box(width = 15, status = "primary",background = "navy",
                                  uiOutput("rb"),uiOutput("etat"),
                                  uiOutput("eta"),
                                  uiOutput("column"), 
                                  uiOutput("colonne"),
                                  uiOutput("bilan")
                              )
                            ),
                            br(),
                            box(width = 15, status = "primary",background = "navy",
                              numericInput("obs", "Number of observations to view:", 200)),
                            br(),
                            fluidRow(
                              
                              
                              box( width = 30, status = "primary",
                                   div(style = 'overflow-x: scroll',dataTableOutput('cont'))))
                            
                    ),
                      tabItem(tabName = "taux",
                            fluidRow(
                           
                               
                              box(width=6,background = "navy",
                                  title = "Information on year N",status = "primary",solidHeader = TRUE,collapsible = TRUE,
                                  uiOutput("CTX_N"),uiOutput("IMP_N"), uiOutput("CLS_N"),uiOutput("CLSn_N")),
                              
                              box(width=6,background = "navy",
                                  title = "Information on year N1",status = "primary",solidHeader = TRUE,collapsible = TRUE,
                                  uiOutput("CTX_N1"),uiOutput("IMP_N1"),uiOutput("CLS_N1"),uiOutput("CLSn_N1")
                              )),
                            
                            box(width = 15,background = "navy",
                                title = "Choose the definition you want", status = "primary", solidHeader = T, collapsible = T,

                                uiOutput("askctx"),uiOutput("askimp"),uiOutput("askcls"),uiOutput("askclsn")),

                           
                         
                            fluidRow(
                              box(background = "navy",
                                  solidHeader = TRUE,collapsible = TRUE,  status = "primary",width=4,
                                  uiOutput("sector")),
                              box(solidHeader = TRUE,collapsible = TRUE,  status = "primary",width=4,background = "navy",
                                  uiOutput("control")),
                              box(solidHeader = TRUE,collapsible = TRUE,  status = "primary",width=4,background = "navy",
                                  uiOutput("biilan")
                              )
                            ),
                            
                            box(solidHeader = TRUE,collapsible = TRUE,  status = "primary" , width=15, background = "navy",
                                uiOutput("rbb")
                            ),
                            
                            
                            box(
                              title = "Evolution of portfolio quality",solidHeader = TRUE,collapsible = TRUE,  status = "primary",width=30,
                              fluidPage(tabBox(width=30,
                                               #tabPanel(title="rate per year",
                                               #  verbatimTextOutput("t")),
                                               tabPanel(title="Default rate",
                                                        div(style = 'overflow-x: scroll',dataTableOutput('ptauxd'))),
                                               tabPanel(width=30, 
                                                        title="Default rate plot",
                                                        plotlyOutput("ptaux"))
                                               
                              ))),
                            uiOutput("j"))
                       
                    
                    
                  )
                  
                )
                
  )
)
)