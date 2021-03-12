#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Overall", tabName = "overall", icon = icon("th")),
                menuItem("Nazionale", tabName = "nazione", icon = icon("th")),
                menuItem("Regioni", tabName = "regioni", icon = icon("th")),
                menuItem("Regioni a confronto", tabName = "regioni_confronto", icon = icon("th")),
                menuItem("Province", tabName = "province", icon = icon("th"))
            )
        ),
        dashboardBody(
            sliderInput("start.date",label = "Start date",value = max(dati.nazione.plus$data)-30, max = max(dati.nazione.plus$data), min = min(dati.nazione.plus$data), timeFormat="%d/%m/%Y"),
            tabItems(
                tabItem(tabName = "overall",
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers1", width = 12),
                                    valueBoxOutput("bigNumbers2", width = 12),
                                    valueBoxOutput("bigNumbers3", width = 12)
                                ),
                                plotlyOutput("plot1", height = "346px")
                        )),
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers4", width = 12),
                                    valueBoxOutput("bigNumbers5", width = 12),
                                    valueBoxOutput("bigNumbers6", width = 12)
                                ),
                                plotlyOutput("plot2")
                            )),
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers7", width = 12),
                                    valueBoxOutput("bigNumbers8", width = 12),
                                    valueBoxOutput("bigNumbers9", width = 12)
                                ),
                                plotlyOutput("plot3")
                            )),
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers19", width = 12),
                                    valueBoxOutput("bigNumbers20", width = 12),
                                    valueBoxOutput("bigNumbers21", width = 12)
                                ),
                                plotlyOutput("plot7")
                            )),
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers10", width = 12),
                                    valueBoxOutput("bigNumbers11", width = 12),
                                    valueBoxOutput("bigNumbers12", width = 12)
                                ),
                                plotlyOutput("plot4")
                            )),
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers22", width = 12),
                                    valueBoxOutput("bigNumbers23", width = 12),
                                    valueBoxOutput("bigNumbers24", width = 12)
                                ),
                                plotlyOutput("plot8")
                            )),
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers13", width = 12),
                                    valueBoxOutput("bigNumbers14", width = 12),
                                    valueBoxOutput("bigNumbers15", width = 12)
                                ),
                                plotlyOutput("plot5")
                            )),
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers25", width = 12),
                                    valueBoxOutput("bigNumbers26", width = 12),
                                    valueBoxOutput("bigNumbers27", width = 12)
                                ),
                                plotlyOutput("plot9")
                            )),
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers16", width = 12),
                                    valueBoxOutput("bigNumbers17", width = 12),
                                    valueBoxOutput("bigNumbers18", width = 12)
                                ),
                                plotlyOutput("plot6")
                            )),
                        fluidRow(
                            splitLayout(
                                verticalLayout(
                                    valueBoxOutput("bigNumbers28", width = 12),
                                    valueBoxOutput("bigNumbers29", width = 12),
                                    valueBoxOutput("bigNumbers30", width = 12)
                                ),
                                plotlyOutput("plot10")
                            ))
                ),
                tabItem(tabName = "province",
                    fluidPage(
                        titlePanel("Province"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("region",
                                            "Region:",
                                            choices = sort(unique(dati.province.plus$denominazione_regione)),
                                            selected = "Lombardia"),
                                radioButtons("metric", "Aggregation:", c("Overall", "Daily", "Weekly"), "Overall"),
                                checkboxInput("province.relative", "Per 1000 People")
                            ),
                            
                            mainPanel(
                                plotlyOutput("provincePlot")
                            )
                        )
                    )
                ),
                tabItem(tabName = "regioni",
                    fluidPage(
                        titlePanel("Regioni"),
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("regioni.agg", "Aggregation:", c("Overall", "Daily","Weekly"), "Overall"),
                                checkboxGroupInput("regione", "Region", sort(unique(dati.regione.plus$denominazione_regione)), "Lombardia")
                            ),
                            
                            mainPanel(
                                plotlyOutput("regioniPlot")
                            )
                        )
                    )
                ),
                tabItem(tabName = "regioni_confronto",
                        fluidPage(
                            titlePanel("Regioni a Confronto"),
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput("confronto.metric",
                                                "Metric:",
                                                choices = c(overall.vars,daily.vars,weekly.vars),
                                                selected = "settimanale_totale_casi")
                                ),
                                
                                mainPanel(
                                    plotlyOutput("regioniConfrontoPlot")
                                )
                            )
                        )
                ),
                tabItem(tabName = "nazione",
                    fluidPage(
                        titlePanel("Overall"),
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("nazione.agg", "Aggregation:", c("Overall", "Daily","Weekly"), "Overall")
                            ),
                            
                            mainPanel(
                                plotlyOutput("nazionePlot")
                            )
                        )
                    )
                )
            )
        )
    )
)
