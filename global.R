## Estimation Plots
estPlotUI <- function(id, ...){
  ns <- NS(id)
  plotOutput(ns('coef'), ...)
}
estPlot <- function(input, output, session, sim_obj, method, which_resp) {
  fit <- callModule(estimation, 'est', sim_obj, method)
  switch(
    method,
    ols = {
      if (sim_obj$n < sim_obj$p) {
        output$coef <- renderPlot({
          plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(0, 0, label = "I can not be fitted with wide matrices", cex = 2)
        })
      } else {
        output$coef <- renderPlot({
          matplot(fit$coef, lty = 1, type = "b", pch = 16, lwd = 2)
        })
      }
    },
    pcr = {
      output$coef <- renderPlot({
        biplot(fit)
      })
    },
    pls = {
      output$coef <- renderPlot({
        biplot(fit)
      })
    },
    xenv = {
      if (sim_obj$n < sim_obj$p) {
        output$coef <- renderPlot({
          plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(0, 0, label = "I can not be fitted with wide matrices", cex = 2)
        })
      } else {
        output$coef <- renderPlot({
          matplot(fit$beta, lty = 1, type = "b", pch = 16, lwd = 2)
        })
      }
    }
  )
}

## Response Selection
respUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('respSelect'))
}
resp <- function(input, output, session, nresp) {
  ns <- session$ns
  output$respSelect <- renderUI(
    selectInput(
      ns("whichResp"),
      "Which Response",
      as.list(`names<-`(1:nresp, paste("Response", 1:nresp)))
    )
  )
  which_resp <- reactive(input$whichResp)
  return(which_resp)
}
## Summary Interface
estSummaryUI <- function(id){
  ns <- NS(id)
  verbatimTextOutput(ns('out'))
}
estSummary <- function(input, output, session, sim_obj, method, which_resp) {
  fit <- callModule(estimation, 'est', sim_obj, method)
  switch(
    method,
    ols = {
      if (sim_obj$n < sim_obj$p)
        output$out <- renderPrint("I can not be fitted with wide matrices")
      else {
        if (sim_obj$type != "univariate") {
          output$out <- renderPrint(summary(fit)[as.numeric(which_resp())])
        } else {
          output$out <- renderPrint(summary(fit))
        }
      }
    },
    pcr = {
      output$out <- renderPrint(summary(fit))
    },
    pls = {
      output$out <- renderPrint(summary(fit))
    },
    xenv = {
      if (sim_obj$n < sim_obj$p)
        output$out <- renderPrint("I can not be fitted with wide matrices")
      else
        output$out <- renderPrint(str(fit))
    }
  )
}

## Estimation on Simulation Data
estimationUI <- function(id) {
  ns <- NS(id)
}
estimation <- function(input, output, session, sim_obj, method) {
  Train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  fit <- switch(
    method,
    ols = {
      if (sim_obj$n < sim_obj$p) return(NULL)
      with(Train, lm(y ~ x))
    },
    pcr = {
      with(Train, pcr(y ~ x))
    },
    pls = {
      with(Train, plsr(y ~ x))
    },
    xenv = {
      if (sim_obj$n < sim_obj$p) return(NULL)
      with(Train, xenv(x, y, u = ncol(x)))
    }
  )
  return(fit)
}

## Estimation Method Selection
estMethodUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      selectInput(
        ns("estMethod"),
        label = "Estimation Method:",
        choices  = c(
           "Least Square Estimation" = "ols",
           "Principal Component Regression" = "pcr",
           "Partial Least Square Regression" = "pls",
           "Envelope MLE" = "xenv"
        ),
        width = "100%"
      )
    ),
    bsTooltip(ns("estMethod"),
              "Estimation method to apply on simulated Data",
              "top", "hover")
  )
}
estMethod <- function(input, output, session) {}

## Download Buttons
downloadUI <- function(id, label = 'RData', type = "primary") {
  ns <- NS(id)
  h4(downloadLink(
    outputId = ns('downloadFile'),
    label = span(icon("download"), label)),
    class = "text-primary")
}
download <- function(input, output, session, sim.obj, file_type = "RData") {
  dt.train <- data.frame(y = I(sim.obj$Y), x = I(sim.obj$X))
  dt.test <- if (sim.obj$type != "univariate") {
    data.frame(y = I(sim.obj$testY), x = I(sim.obj$testX))
  } else {
    data.frame(y = I(sim.obj$TESTY), x = I(sim.obj$TESTX))
  }
  out <- switch(
    tolower(file_type),
    csv = rbind(train = dt.train, test = dt.test),
    json = jsonlite::toJSON(list(train = dt.train, test = dt.test)),
    rdata = list(train = dt.train, test = dt.test),
    simobj = sim.obj
  )
  downloadFn <- function(data, type = "Rdata") {
    downloadHandler(
      filename <- function()
        ifelse(type == "simobj",
               paste("sim.obj.rdata"),
               paste("sim.obj", type, sep = ".")),
      content = function(file) {
        which.type <- tolower(type)
        switch(
          which.type,
          csv = write.csv(data, file = file),
          rdata = save(data, file = file),
          simobj = save(data, file = file),
          rda = save(data, file = file),
          rds = saveRDS(data, file = file),
          json = jsonlite::write_json(data, path = file)
        )
      }
    )
  }
  output$downloadFile <- downloadFn(data = out, type = file_type)
  return(output)
}

## Simulation Plts :: Plot 1
simPlotUI <- function(id, ...) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('plot'), ...)
  )
}
simPlot <- function(input, output, session, sim_obj, which) {
  output$plot <- renderPlot({
    ggsimrelplot(sim_obj, which = which)
  }, res = 118)
}

## Simulation Plots :: Plot 2
covPlotUI <- function(id, ...) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('plot'), ...)
  )
}
covPlot <- function(input, output, session, sim_obj, plot_type) {
  output$plot <- renderPlot({
    cov_plot(sim_obj, plot_type)
  }, res = 105)
}

## Param Menu Panel
paramMenuUI <- function(id) {
  ns <- NS(id)
  menuItemOutput(ns('paramMenu'))
}
paramMenu <- function(input, output, session) {
  callModule(simType, 'sim-type')
  callModule(commonInput, 'common-parm')
  callModule(multivariateInput, 'multi-parm')
  callModule(bivariateInput, 'bi-parm')
  output$paramMenu <- renderMenu(
    menuItem(
      "Parameter Settings", icon = icon("cogs"), tabName = "settings",
      commonInputUI("common-parm"),
      conditionalPanel("input['sim-type-type'] == 'multivariate'", multivariateInputUI('multi-parm')),
      conditionalPanel("input['sim-type-type'] == 'bivariate'", bivariateInputUI('bi-parm'))
    )
  )
}

## Simulation UI
simUI <- function(id, label = "Simulate Now", ...) {
  ns <- NS(id)
  column(
    12,
    div(
      h3(actionLink(
        ns("update"),
        label = label,
        icon = icon("refresh"),
        ...
      )), class = "text-center"
    )
  )
}
sim <- function(input, output, session) {}

## Type UI
simTypeUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      selectInput(
        inputId = ns("type"),
        label = "Type of simulation:",
        choices = c("Univariate Simulation" = "univariate",
                    "Bivariate Simulation" = "bivariate",
                    "Multivariate Simulation" = "multivariate"),
        selected = "multivariate",
        width = "100%"
      )
    ),
    bsTooltip(ns("type"), "Type of simulation you want to perform", "top", "hover")
  )
}
simType <- function(input, output, session) {
  return(reactive(input$type))
}

## Seed UI
simSeedUI <- function(id, input_lbl = "Seed", btn_lbl = "Seed") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             column(5, fluidRow(numericInput(ns('newSeed'), label = input_lbl, value = 123))),
             column(7, div(h3(actionLink(ns("newSeedBtn"), label = btn_lbl, icon = icon("random")))))
      )
    )
  )
}
simSeed <- function(input, output, session) {
  ns <- session$ns
  # observe(addClass('newSeed', 'input-lg'))
  return(reactive(input$newSeed))
}

## Common Input Panel
commonInputUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("commonUI"))
}
commonInput <- function(input, output, session) {
  output$commonUI <- renderUI({
    ns <- session$ns
    tagList(
      fluidRow(
        column(6, numericInput(ns("n"), label = "N: Train", value = 200, min = 10, step = 1)),
        column(6, numericInput(ns("n_test"), label = "N: Test", value = 50, min = 5, step = 1))
      ),
      fluidRow(
        column(6, numericInput(ns("p"), label = "N: Predictors", value = 15, min = 2, step = 1)),
        column(6, textInput(ns("q"), label = "Rel.Pred", value = "5, 4"))
      ),
      fluidRow(
        column(6, textInput(ns("R2"), label = "Coef. Determination", value = "0.8, 0.7")),
        column(6, textInput(ns("relpos"), label = "RelPos.Comp",
                            value = "1, 2; 3, 4, 6"))
      ),
      fluidRow(
        conditionalPanel(
          condition = "input['sim-type-type'] != 'multivariate'",
          column(12, sliderInput(ns("gamma"), "Gamma", min = 0, max = 2, value = 0.6, step = 0.1))
        )
      )
    )
  })
}

## Multivariate Input Panel
multivariateInputUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("mvrUI"))
}
multivariateInput <- function(input, output, session) {
  output$mvrUI <- renderUI({
    ns <- session$ns
    tagList(
      fluidRow(
        column(6, sliderInput(ns("gamma"), label = "gamma", value = 0.5, min = 0, step = 0.01, max = 2)),
        column(6, sliderInput(ns("eta"), label = "eta", value = 0, min = 0, step = 0.01, max = 2)),
        column(6, numericInput(ns("m"), label = "N: Response", value = 4, min = 2, step = 1)),
        column(6, textInput(ns("ypos"), label = "Response Mixup", value = "1, 3; 2, 4"))
      )
    )
  })
}

## Bivariate Input Panel
bivariateInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, div(em("Correlation between Response")), style = "text-align:center;"),
      column(6, sliderInput(ns("rho1"), "Without Given X",
                            min = -1, max = 1, value = 0.6, step = 0.1, width = "100%")),
      column(6, sliderInput(ns("rho2"), "With Given X",
                            min = -1, max = 1, value = 0.7, step = 0.1, width = "100%"))
    )
  )
}
bivariateInput <- function(input, output, session) {}

## Extra Input Panel
extraInputUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("extraInput"))
}
extraInput <- function(input, output, session) {
  output$extraInput <- renderUI({
    ns <- session$ns
    fluidRow(
      column(12, checkboxGroupInput(
        inputId = ns("extraplot"),
        label = "Display Extra Plots",
        choiceNames = c("Covariance Plot", "R-squared Plot"),
        choiceValues = c("covplt", "r2plt"),
        selected = "covplt",
        inline = TRUE,
      ))
    )
  })
}

## Dashboard UI
dashboardUI <- function(id){
  ns <- NS(id)
  navbarPage(
    theme = shinythemes::shinytheme("yeti"),
    inverse = TRUE,
    title = "Simulation Overview",
    position = "static-top",
    tabPanel(
      "Home",
      icon = icon("home"),
      uiOutput(ns("homepage"))
    ),
    tabPanel(
      "Plot Details",
      icon = icon("line-chart"),
      uiOutput(ns("plotDetails"))
    )
  )
}
dashboard <- function(input, output, session, sim_obj, type) {
  sobj <- sim_obj
  beta <- sobj[["beta"]]
  out <- lapply(1:ncol(beta), function(col) which(beta[, col] != 0))
  `names<-`(out, paste0("Response", seq_along(out)))
  
  ns <- session$ns
  output$homepage <- renderUI({
    div(
      class = "grid-container",
      id = "overview-container",
      ## Sidebar ----
      div(id = "simplot1",
          class = "grid-content",
          simPlotUI("betaPlot")),
      div(id = "simplot2",
          class = "grid-content",
          simPlotUI("relComp")),
      div(id = "simplot3",
          class = "grid-content",
          simPlotUI("estRelComp")),
      conditionalPanel(
        id = "covplot1",
        class = "grid-content",
        condition = "output.extraplot",
        covPlotUI("relpos")
      ),
      conditionalPanel(
        condition = "output.extraplot",
        id = "covplot2",
        class = "grid-content",
        covPlotUI("rotation")
      ),
      conditionalPanel(
        condition = "output.extraplot",
        id = "covplot3",
        class = "grid-content",
        covPlotUI("relpred")
      )
    )
  })
  output$betaDetails <- renderText({
    paste(
      "Here we can see that for the first response", paste0(out[[1]], collapse = ", "),
      "predictors are relevant and has non-zero regression coefficients.",
      "Since we have setup to have", sobj[["q"]][1],
      "predictors to be relevant for first response",
      ifelse(type == "multivariate", "component.", "variable."),
      strong("Bold Text")
    )
  })
  output$plotDetails <- renderUI({
    fillRow(
      flex = c(5, 3),
      id = "plot-details",
      ## Sidebar ----
      tabsetPanel(
        tabPanel(
          "Beta Coefficient",
          div(
            id = "beta-details-container",
            class = "details-container",
            style = "display: flex; flex-direction: column;",
            simPlotUI("betaPlot1"),
            div(
              id = "beta-details",
              class = "details",
              htmlOutput(ns("betaDetails"))
            )
          )
        ),
        tabPanel(
          "Relevant Components",
          div(
            style = "display: flex; flex-direction: column;",
            simPlotUI("relComp1"),
            div(
              style = "flex: 1;",
              "I am relevant components."
            )
          )
        ),
        tabPanel(
          "Estimated Relevant Components",
          simPlotUI("estRelComp1")
        )
      ),
      ## CovPlot ----
      tabsetPanel(
        tabPanel(
          "Relpos",
          covPlotUI("relpos1")
        ),
        tabPanel(
          "Rotation",
          covPlotUI("rotation1")
        ),
        tabPanel(
          "Relpred",
          covPlotUI("relpred1")
        )
      )
    )
  })
}

## Parameter Description
paramPlusUI <- function(id) {
  ns <- NS(id)
  navbarPage(
    theme = shinythemes::shinytheme("yeti"),
    inverse = TRUE,
    title = "Parameter Description",
    position = "static-top",
    tabPanel(
      "Parameters Details",
      icon = icon("info"),
      ## Main Container ----
      div(
        class = "grid-container",
        id = "parameters-details",
        uiOutput(ns("details_n")),
        uiOutput(ns("details_p")),
        uiOutput(ns("details_q")),
        uiOutput(ns("details_relpos")),
        uiOutput(ns("details_R2")),
        uiOutput(ns("details_gamma")),
        uiOutput(ns("details_ntest")),
        conditionalPanel(
          "input['sim-type-type'] == 'multivariate'",
          uiOutput(ns("details_ypos")),
          uiOutput(ns("details_m"))
        ),
        conditionalPanel(
          "input['sim-type-type'] == 'bivariate'",
          uiOutput(ns("details_rho"))
        )
      )
    )
  )
}
paramPlus <- function(input, output, session, type) {
  details <- function(name, notation = "\\(n\\)", description, details) {
    withMathJax(div(
      id = "n-desc",
      class = "grid-content",
      box(
        collapsible = TRUE,
        collapsed = FALSE,
        width = 12,
        status = "primary",
        title = div(
          div(
            class = "inner-desc-name",
            tags$span(class = "parm-name", name)
          ),
          div(
            class = "inner-desc-notation",
            span(
              class = "parm-notation",
              notation
            )
          ),
          div(
            class = "inner-desc-more",
            span(
              class = "parm-more",
              description
            )
          )),
        div(
          class = "inner-desc-details",
          span(
            class = "parm-details",
            HTML(details)
          )
        ))
    ))
  }
  output$details_n <- renderUI({
    shinyjs::runjs("$('#map').hide();")
    details(
      name = "n:",
      notation = "\\(n\\)",
      description = "Number of training samples",
      details = paste0("Number of training sample.",
                       "This can be larger than number of",
                       "predictor variables.")
    )
  })
  output$details_p <- renderUI({
    details(
      name = "p:",
      notation = "\\(p\\)",
      description = "Number of Predictor Variables",
      details = paste0("Number of predictor variables.",
                       "Simrel is capable simulating \\(p > n\\) model")
    )
  })
  output$details_q <- renderUI({
    details(
      name = "q:",
      notation = "\\(q\\)",
      description = "Number of relevant predictors",
      details = switch(
        type(),
        ## q: univariate ----
        "univariate" = paste(
          "Number of relevant predictor variables.",
          "it is a number specifying how many predictors",
          "are relevant for response."
        ),
        ## q: bivariate ----
        "bivariate" = paste(
          "Number of relevant predictor variables.",
          "It is a vector with three elements.",
          tags$ul(
                 tags$li(
                        paste(
                          "First one represents the number of predictors",
                          "relevant for first response variable."
                        )
                      ),
                 tags$li(
                        paste(
                          "Second one represents the number of predictors",
                          "relevant for second response variable."
                        )
                      ),
                 tags$li(
                        paste(
                          "Third one represents the number of",
                          "predictors relevant for both of the response variables."
                        )
                      )
               )
        ),
        ## q: multivariate ----
        "multivariate" = paste(
          "Number of relevant predictor variables.",
          "It is a vector where each element represents",
          "the number of predictors relevant for each response",
          "components. </br>For example, <code>c(15, 8)</code>",
          "gives us a dataset with 15 predictor variables relevant",
          "for the first response component and another 8 (not common) predictor",
          "variables relevant for the second response component. This will",
          "also refers that there are two latent dimension of response space",
          "that contains information while there can be more than two response",
          "variables which are obtained by combining these two informative",
          "response components with non-informative components through",
          "orthogonal rotation of covariance matrix."
        )
      )
    )
  })
  output$details_relpos <- renderUI({
    details(
      name = "relpos:",
      notation = "\\(\\mathcal{P}\\)",
      description = "Position index of relevant predictor components",
      details = paste0("")
    )
  })
  output$details_ypos <- renderUI({
    details(
      name = "ypos:",
      notation = "\\(\\mathcal{Q}\\)",
      description = "Index of response components to combine together",
      details = paste0("This is a list of index integer. Each element of index",
                       "can be a vector indicating how to combine the response",
                       "components while orthogonal rotation.",
                       br(), "For example:", code("list(c(1, 4), c(2, 3))"),
                       "shows that there are two informative components 1 and 2",
                       "the first response component is combined with uninformative response",
                       "component 4 and the second response component is combined with uninformative",
                       "response component 3. In total we will obtain 4 response components.",
                       "In this situation, we can expect to have first and fourth response variables",
                       "sharing same relevant predictors and second and third response variables",
                       "sharing same relevant predictors. In shiny application, we can input",
                       "the list as", code("1, 4; 2, 3"), "separating list elements by (;) and",
                       "vector elements by (,).")
    )
  })
  output$details_R2 <- renderUI({
    details(
      name = "R2:",
      notation = "\\(\\rho^2\\)",
      description = "Coefficient of Determination",
      details = paste0("")
    )
  })
  output$details_gamma <- renderUI({
    details(
      name = "gamma:",
      notation = "\\(\\gamma\\)",
      description = "Decay factor of eigenvalues of predictors",
      details = paste0("")
    )
  })
  output$details_rho <- renderUI({
    details(
      name = "rho:",
      notation = "\\(\\rho\\)",
      description = "Correlation between response variables with and without given \\mathbf{x}",
      details = paste0("")
    )
  })
  output$details_m <- renderUI({
    details(
      name = "m:",
      notation = "\\(m\\)",
      description = "Number of Response variables",
      details = paste0("")
    )
  })
  output$details_ntest <- renderUI({
    details(
      name = "ntest:",
      notation = "\\(n_\\text{test}\\)",
      description = "Number of test samples",
      details = paste0("Number of test (validation) sample.",
                       "This can be larger than number of",
                       "predictor variables.")
    )
  })
}
