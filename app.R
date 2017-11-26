## This is Shiny Simrel App -----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(simrel)
library(envlp)
library(leaflet)
library(reshape2)
library(ggplot2)
library(plotly)
library(pls)
shinyjs::useShinyjs()

## Render About Page ----
rmarkdown::render("_partials/about.Rmd", quiet = TRUE)

## Some Functions ---------------
parseText <- function(x) {
  evl <- function(x) as.numeric(unlist(strsplit(x, split = ",")))
  out <- lapply(strsplit(x, ";")[[1]], evl)
  if (length(out) > 1) return(out)
  return(out[[1]])
}
source("global.R")

## Specify SideBar Width --------
sideBarWidth <- 350

## Dashboard Page Starts --------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Simulation NMBU",
    titleWidth = sideBarWidth),
  ## Sidebar Starts -----------------
  dashboardSidebar(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs("www/script.js"),
    width = sideBarWidth,
    collapsed = FALSE,
    tags$head(tags$link(rel="stylesheet", href="styles.css")),
    div(
      id = "except-download",
      sidebarMenu(
        id = "sidebar",
        ## Paramter Settings --------------------------
        menuItem(
          id = "settings",
          "Parameter Settings",
          icon = icon("sliders"),
          tabName = "settings",
          simUI('sim-id'),
          simSeedUI('seed-id',
                    input_lbl = NULL,
                    btn_lbl = "New Seed"),
          width = sideBarWidth,
          simTypeUI('sim-type'),
          commonInputUI('common-parm'),
          conditionalPanel("input['sim-type-type'] == 'multivariate'",
                           multivariateInputUI('multi-parm')),
          conditionalPanel("input['sim-type-type'] == 'bivariate'",
                           bivariateInputUI('bi-parm')),
          extraInputUI('extra-input')
        )),
      conditionalPanel(
        "input['sim-id-update']",
        sidebarMenu(
          menuItem("Simulation Overview",
                   icon = icon("dashboard"),
                   tabName = "overview"),
          ## Estimation Start -----------
          shinyjs::hidden(menuItem(
            text = "Estimation",
            icon = icon("line-chart"),
            estMethodUI('estMthd'),
            menuSubItem("Summary",
                        tabName = "estSummary"),
            menuSubItem("Plots",
                        tabName = "estPlots")
          ),
          ## Comparison of Estimation Methods ------------------
          menuItem(
            text = "Method Comparison",
            icon = icon("tasks"),
            tabName = "estSummary"
          ))
        )
      ),
      sidebarMenu(
        menuItem(
          text = "Parameters Details",
          icon = icon('info'),
          tabName = "parmdesc"
        ),
        menuItem(
          text = "Flowchart",
          icon = icon("map-o"),
          tabName = "flowchart"
        ),
        menuItem(
          text = "About",
          icon = icon("info"),
          tabName = "about"
        )
      )
    ),
    div(
      id = "sidebar-bottom",
      conditionalPanel(
        id = "download",
        "input['sim-id-update']",
        ## Download Buttons ----
        uiOutput("download")
      ),
      column(
        width = 12,
        id = "copyright",
        div(class="text-center",
            "Copyright © Raju Rimal, NMBU, Norway")
      )
    )
  ),

  ## Body Starts --------
  dashboardBody(
    tags$style("height:100vh;"),
    ## tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    ## Map Page ------
    conditionalPanel(
        "!input['sim-id-update']",
      fluidRow(
        tags$style(
               type = "text/css",
               paste0(
                 "#map {",
                 "height: calc(100vh - 50px) !important;",
                 "position: absolute;",
                 "top: 50px;",
                 "}"
               )
             ),
        leafletOutput("map")
      )
    ),
    tabItems(
      ## Simulation Overview --------------
      tabItem(
        tabName = "overview",
        conditionalPanel(
          "input['sim-id-update']",
          dashboardUI("dash")
        )
      ),
      tabItem(
        tabName = "parmdesc",
        paramPlusUI("parm-details")
      ),
      ## Estimation Overview -----------------
      tabItem(
        tabName = "estSummary"
      ),
      tabItem(
        tabName = "estPlots"
      ),
      tabItem(
        tabName = "flowchart",
        uiOutput("flowchart")
      ),
      tabItem(
        tabName = "about",
        htmlOutput("about")
      )
    )
  )
)

## ---- Server Function ---------
server <- function(input, output, session) {

  ## Calling Modules ---------------------------
  type <- callModule(simType, 'sim-type')
  callModule(sim, 'sim-id')
  callModule(simSeed, 'seed-id')
  callModule(commonInput, 'common-parm')
  callModule(multivariateInput, 'multi-parm')
  callModule(bivariateInput, 'bi-parm')
  callModule(extraInput, 'extra-input')
  callModule(paramPlus, 'parm-details', type)

  ## Observe  -----------------------------------
  observe({
    ## Estimation Module -----------
    callModule(estMethod, 'estMthd')
    which_resp <- callModule(resp, 'rsp', ncol(as.matrix(simObj()[["Y"]])))

    ## Model Summary and Plot Modules -------------
    callModule(estSummary, 'smry', simObj(),
               input[['estMthd-estMethod']], which_resp)
    callModule(estPlot, 'estPlts', simObj(),
               input[['estMthd-estMethod']], which_resp)

    ## Output the simulation type --------
    output$type <- reactive(simObj()[["type"]])
    outputOptions(output, "type", suspendWhenHidden = FALSE)

    ## Output if extraplot is needed or not
    output$extraplot <- eventReactive(input[['sim-id-update']],
                                      input[["extra-input-extraplot"]])
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
    updateNumericInput(session, 'seed-id-newSeed',
                       value = sample(9999, size = 1))
  })
  observeEvent(input[['sim-id-update']], {
    ## Call Dashboard Module ----
    callModule(dashboard, "dash", simObj(), type())
    ## Collapse Parameter input panel ----
    shinyjs::hide(selector = "ul.treeview-menu.menu-open", anim = TRUE)
    shinyjs::removeClass(selector = ".sidebar-menu>li.treeview.active",
                         class = "active")

    ## Simulation Plot Modules -----------
    callModule(simPlot, 'betaPlot', simObj(), 1)
    callModule(simPlot, 'relComp', simObj(), 2)
    callModule(simPlot, 'estRelComp', simObj(), 3)

    ## Simulation Plot Modules for tabPanel -----------
    callModule(simPlot, 'betaPlot1', simObj(), 1)
    callModule(simPlot, 'relComp1', simObj(), 2)
    callModule(simPlot, 'estRelComp1', simObj(), 3)

    ## Download Button Modules ----------
    callModule(download, 'rdta', simObj(), "RData")
    callModule(download, 'csv', simObj(), "CSV")
    callModule(download, 'json', simObj(), "json")
    callModule(download, 'simobj', simObj(), "simobj")

    ## Covariance Plot Module ----------
    if ("covplt" %in% input[["extra-input-extraplot"]]) {
      callModule(covPlot, 'relpos', simObj(), "relpos")
      callModule(covPlot, 'rotation', simObj(), "rotation")
      callModule(covPlot, 'relpred', simObj(), "relpred")

      ## Covariance Plots for tab panel ----
      callModule(covPlot, 'relpos1', simObj(), "relpos")
      callModule(covPlot, 'rotation1', simObj(), "rotation")
      callModule(covPlot, 'relpred1', simObj(), "relpred")
    }
  })

  ## About Page ----
  output$about <- renderUI({
    div(
      fluidRow(withMathJax(includeHTML("_partials/about.html")))
    )
  })

  ## Parameter Description ----
  output$parmdesc <- renderUI({
    shinyjs::runjs("$('#map').hide();")
    source("_partials/parameters-description.R")
    parm_desc(type = type())
  })

  ## Flowchart Page ------
  output$flowchart <- renderUI({
    div(
      id = "flowchart",
      fluidRow(includeHTML("_partials/flowchart.html"))
    )
  })

  ## Download Buttons and Copyright -------
  output$download <- renderUI({
    fluidRow(
      column(
        width = 12,
        column(4, downloadUI('simobj', "RData")),
        column(4, downloadUI('csv', "CSV")),
        column(4, downloadUI('json', "JSON"))
      )
    )
  })

  ## Home Page Map --------------------------------------------------
  output$map <- renderLeaflet({
    content <- paste(
      sep = "<br/>",
      "<h3><a href='http://simulatr.github.io/simrel/'>",
      "Welcome to Simrel</a></h3>",
      "<b><a href='http://www.nmbu.no'>",
      "Norwegian University of Life Science</a></b>",
      "Universitetstunet 3",
      "1433 Ås"
    )
    setView(
      addTiles(leaflet(options = leafletOptions(zoomControl = FALSE)),
               options = list(opacity = 0.75)),
      lng = 10.769563, lat = 59.666099, zoom = 17
    ) %>% addPopups(
      lng = 10.769563, lat = 59.666099, content,
      options = popupOptions(closeButton = FALSE)
    )
  })
}

## Run Simrel App --------------
shinyApp(ui, server)
