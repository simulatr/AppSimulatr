
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

## Load Some Packages ------------------------------
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
sideBarWidth <- 300

## Dashboard Page Starts --------------------------------------------------------------------
dashboardPage(
  dashboardHeader(title = "Simulation NMBU", titleWidth = sideBarWidth),
  ## Sidebar Starts -----------------
  dashboardSidebar(width = sideBarWidth,
     div(h3(actionLink("newSeed", label = "New Seed", icon = icon("random"))), 
         class = "container-fluid text-center", width = '100%'),
     fluidRow(
       column(12, selectInput("model", label = "Model:", 
                              choices = c("Least Square Estimation",
                                          "Principal Component Regression",
                                          "Partial Least Square Regression",
                                          "Envelope MLE"), width = "100%")),
       bsTooltip("model", "Choose a model for consequent outputs and analysis result",
                 "top", "hover")
     ),
## Sidebar Menu starts -------------------------------------------------------------------------
     sidebarMenu(
       ## Paramter Settings --------------------------
       menuItem("Parameter Settings", icon = icon("cogs"), tabName = "settings", 
                fluidRow(
                  column(6, numericInput("n", label = "N: Train", value = 200, min = 10, step = 1)),
                  column(6, numericInput("n_test", label = "N: Test", value = 50, min = 5, step = 1))
                ),
                fluidRow(
                  column(6, numericInput("p", label = "N: Predictors", value = 15, min = 2, step = 1)),
                  column(6, textInput("q", label = "Rel.Pred", value = "5, 4"))
                ),
                fluidRow(
                  column(6, numericInput("m", label = "N: Response", value = 4, min = 2, step = 1)),
                  column(6, textInput("relpos", label = "RelPos.Comp", 
                                      value = "1, 2; 3, 4, 6"))
                ),
                fluidRow(
                  column(6, textInput("R2", label = "Coef. Determination", value = "0.8, 0.7")),
                  column(6, textInput("ypos", label = "Response Mixup", 
                                      value = "1, 3; 2, 4"))
                ),
                fluidRow(
                  column(12, sliderInput("gamma", "Gamma", 
                                         min = 0, max = 1, value = 0.6, step = 0.1, width = "100%"))
                )#,
                # div(h3(actionLink("update", label = "Update Now", icon = icon("refresh"))), 
                #     class = "container-fluid text-center")
        ),
       ## Menu Items - Overview Plots and Analysis ----------------------------
       menuItem("Overview", icon = icon("dashboard"), tabName = "overview"),
       menuItem("Plots and Analysis", icon = icon("line-chart"), tabName = "plots"),
       menuItem("Model Comparison", icon = icon("tasks"), tabName = "analysis"),
       div(downloadButton('downloadRData', label = "Download Data", 
                          class = 'btn btn-primary btn-lg btn-block active'), 
           style = "bottom:0px; position:absolute; width:100%")
     )
  ),
## Body Starts ------------------------------------------------------------------------------------------- 
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
    tabItem(tabName = "overview",
      ## SimrelPlot ------------------------------
      div(class = "col-md-6",
        box(plotOutput("simPlot1", height = '430px'), 
            solidHeader = FALSE, status = "primary", width = NULL),
        box(plotOutput("R2plot", height = '250px'),
            solidHeader = FALSE, status = "primary", width = NULL)
      ),
      div(class = "col-md-6",
        box(plotOutput("simPlot2", height = '340px'), 
            solidHeader = FALSE, status = "primary", width = NULL),
        box(plotOutput("simPlot3", height = '340px'), 
            solidHeader = FALSE, status = "primary", width = NULL)
      )
    ),
    tabItem(tabName = "plots",
          uiOutput("plot.ui")
    ),
    tabItem(tabName = "analysis",
            uiOutput("analysis.ui")
    )
  )
))