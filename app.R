## This is Shiny Simrel App -------------------------
##==============================

## Loading Packages and Functions -----------------------
# pkgs <- c("shinydashboard", "shiny", "shinyBS", "simrel", "envlp",
#           "leaflet", "shinyjs", "reshape2", "ggplot2", "pls")
# for (pkg in pkgs) require(pkg, character.only = TRUE)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(simrel)
library(envlp)
library(leaflet)
library(reshape2)
library(ggplot2)
library(pls)

source("global.R")
source("plot-function.R")
shinyjs::useShinyjs()

## Some Functions ----------------------------
parseText <- function(x) {
  evl <- function(x) as.numeric(unlist(strsplit(x, split = ",")))
  out <- lapply(strsplit(x, ";")[[1]], evl)
  if (length(out) > 1) return(out)
  return(out[[1]])
}

## Specify SideBar Width --------------------------------------------------------------
sideBarWidth <- 300

## Dashboard Page Starts --------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Simulation NMBU", titleWidth = sideBarWidth),

  ## Sidebar Starts -----------------
  dashboardSidebar(
    width = sideBarWidth,
    sidebarMenu(
      ## Paramter Settings --------------------------
      simUI('sim-id'),
      simSeedUI('seed-id', input_lbl = NULL, btn_lbl = "New Seed"),
      menuItem(
        "Parameter Settings",
        icon = icon("sliders"),
        tabName = "settings",
        width = sideBarWidth,
        simTypeUI('sim-type'),
        commonInputUI('common-parm'),
        conditionalPanel("input['sim-type-type'] == 'multivariate'", multivariateInputUI('multi-parm')),
        conditionalPanel("input['sim-type-type'] == 'bivariate'", bivariateInputUI('bi-parm'))
      )),
    conditionalPanel(
      "input['sim-id-update']",
      sidebarMenu(
        menuItem("Simulation Overview", icon = icon("dashboard"), tabName = "overview"),
        ## Estimation Start -----------
        menuItem(
          text = "Estimation",
          icon = icon("line-chart"),
          estMethodUI('estMthd'),
          menuSubItem("Summary", tabName = "estSummary"),
          menuSubItem("Plots", tabName = "estPlots")
        ),
        ## Comparison of Estimation Methods ------------------
        menuItem(
          text = "Method Comparison",
          icon = icon("tasks"),
          tabName = "estSummary"
        ),
        menuItem(
          text = "Flowchart",
          icon = icon("map-o"),
          tabName = "flowchart"
        ),
        ## Download Buttons ----
        div(
          column(4, downloadUI('simobj', "SimObj")),
          column(4, downloadUI('rdta', "RData")),
          column(4, downloadUI('json', "JSON")),
          style = "bottom:5px; position:absolute; width:100%"
        )
      )
    )
  ),

  ## Body Starts -------------------------------------------------------------------------------------------
  dashboardBody(
    tags$style("height:100vh;"),
    # tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    conditionalPanel(
      "!input['sim-id-update']",
      fluidRow(
        tags$style(
          type = "text/css",
          "#map {
              height: calc(100vh - 50px) !important;
              position: absolute;
              top: 50px;
              }"
        ),
        leafletOutput("map")
      )
    ),
    conditionalPanel(
      "input['sim-id-update']",
      tabItems(
        ## Simulation Overview --------------
        tabItem(
          tabName = "overview",
          fluidRow(
            div(class = "col-md-12 col-lg-4", style = "padding-bottom:10px;",
                simPlotUI('betaPlot', height = '400px')),
            div(class = "col-md-12 col-lg-4", style = "padding-bottom:10px;",
                simPlotUI('relComp', height = '400px')),
            div(class = "col-md-12 col-lg-4", style = "padding-bottom:10px;",
                simPlotUI('estRelComp', height = '400px'))
          ),
          conditionalPanel(
            condition = 'output.extraplot == "covplt" && output.type == "multivariate"',
            fluidRow(
              div(class = "col-sm-12 col-md-4", style = "padding-bottom:10px;",
                  covPlotUI('relpos', height = '400px')),
              div(class = "col-sm-12 col-md-4", style = "padding-bottom:10px;",
                  covPlotUI('rotation', height = '400px')),
              div(class = "col-sm-12 col-md-4", style = "padding-bottom:10px;",
                  covPlotUI('relpred', height = '400px'))
            )
          )
        ),
        ## Estimation Overview -----------------
        tabItem(
          tabName = "estSummary",
          fluidRow(
            conditionalPanel(
              '((input["estMthd-estMethod"] == "ols")) && (output.type != "univariate")',
              column(12, respUI('rsp'))
            ),
            box(estSummaryUI('smry'))
          )
        ),
        tabItem(
          tabName = "estPlots",
          fluidRow(
            box(estPlotUI('estPlts', height = '600px'), width = 6, height = "600px")
          )
        ),
        tabItem(
          tabName = "flowchart",
          fluidRow(
            includeHTML("flowchart.html")
          )
        )
      )
    )
  )
)


## ---- Server Function ----------------------------------------
server <- function(input, output, session) {
  ## Calling Modules ---------------------------
  type <- callModule(simType, 'sim-type')
  callModule(sim, 'sim-id')
  callModule(simSeed, 'seed-id')
  callModule(commonInput, 'common-parm')
  callModule(multivariateInput, 'multi-parm')
  callModule(bivariateInput, 'bi-parm')

  ## Observe  -----------------------------------
  observe({
    ## Estimation Module -----------
    callModule(estMethod, 'estMthd')
    which_resp <- callModule(resp, 'rsp', ncol(as.matrix(simObj()[["Y"]])))

    ## Model Summary and Plot Modules -------------
    callModule(estSummary, 'smry', simObj(), input[['estMthd-estMethod']], which_resp)
    callModule(estPlot, 'estPlts', simObj(), input[['estMthd-estMethod']], which_resp)

    ## Output the simulation type --------
    output$type <- reactive(simObj()[["type"]])
    outputOptions(output, "type", suspendWhenHidden = FALSE)
    ## Output if extraplot is needed or not
    output$extraplot <- eventReactive(input[['sim-id-update']], input[["multi-parm-extraplot"]])
    outputOptions(output, "extraplot", suspendWhenHidden = FALSE)
  })

  ## Make some simulations ----------------------------------------
  simObj <- eventReactive(input[['sim-id-update']], {
      set.seed(input[['seed-id-newSeed']])
      param <- list(
        n = input[['common-parm-n']],
        p = input[['common-parm-p']],
        q = parseText(input[['common-parm-q']]),
        relpos = parseText(input[['common-parm-relpos']]),
        gamma = input[['common-parm-gamma']],
        R2 = parseText(input[['common-parm-R2']]),
        ntest = input[['common-parm-n_test']],
        type = input[['sim-type-type']]
      )
      if (type() == "multivariate") {
        param$m <- input[['multi-parm-m']]
        param$ypos <- parseText(input[['multi-parm-ypos']])
      }
      if (type() == "bivariate") {
        param$rho = c(input[['bi-parm-rho1']], input[['bi-parm-rho2']])
      }
      do.call(simrel, param)
    })

  ## Update Parameter Input -------------------
  observe({
    ## Observe input of Parameters ------------
    if (all(identical(type(), "bivariate"))) {
      updateTextInput(session, "common-parm-relpos", value = "1,2,3;3,4,6")
      updateTextInput(session, "common-parm-q", value = "5, 5, 2")
      updateTextInput(session, "common-parm-R2", value = "0.8, 0.7")
    }
    if (all(identical(type(), "univariate"))) {
      updateTextInput(session, "common-parm-relpos", value = "2, 3, 4, 6")
      updateTextInput(session, "common-parm-q", value = "6")
      updateTextInput(session, "common-parm-R2", value = "0.9")
    }
    if (all(identical(type(), "multivariate"))) {
      updateTextInput(session, "common-parm-q", value = "5, 4")
      updateTextInput(session, "common-parm-R2", value = "0.8, 0.7")
      updateTextInput(session, "common-parm-relpos", value = "1,2; 3,4,6")
    }
  })

  ## Observe Some Event ----------------------------------------
  observeEvent(input[['seed-id-newSeedBtn']], {
    updateNumericInput(session, 'seed-id-newSeed', value = sample(9999, size = 1))
  })
  observeEvent(input[['sim-id-update']], {
    ## Simulation Plot Modules -----------
    callModule(simPlot, 'betaPlot', simObj(), 1)
    callModule(simPlot, 'relComp', simObj(), 2)
    callModule(simPlot, 'estRelComp', simObj(), 3)

    ## Download Button Modules ----------
    callModule(download, 'rdta', simObj(), "RData")
    callModule(download, 'csv', simObj(), "CSV")
    callModule(download, 'json', simObj(), "json")
    callModule(download, 'simobj', simObj(), "simobj")

    ## Covariance Plot Module ----------
    if (all(identical(type(), "multivariate"),
            "covplt" %in% input[["multi-parm-extraplot"]])) {
      callModule(covPlot, 'relpos', simObj(), "relpos", "relpos", TRUE)
      callModule(covPlot, 'rotation', simObj(), "rotation", "relpred")
      callModule(covPlot, 'relpred', simObj(), "relpred", "relpred")
    }
  })

  ## Home Page Map --------------------------------------------------
  output$map <- renderLeaflet({
    content <- paste(
      sep = "<br/>",
      "<h3><a href='http://simulatr.github.io/simrel/'>Welcome to Simrel</a></h3>",
      "<b><a href='http://www.nmbu.no'>Norwegian University of Life Science</a></b>",
      "Universitetstunet 3",
      "1433 Ås"
    )
    setView(
      addTiles(leaflet(), options = list(opacity = 0.75)),
      lng = 10.769563, lat = 59.666099, zoom = 17
    ) %>% addPopups(
      lng = 10.769563, lat = 59.666099, content,
      options = popupOptions(closeButton = FALSE)
    )
  })
}

## Run Simrel App ------------
shinyApp(ui, server)
