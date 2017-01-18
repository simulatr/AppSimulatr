
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

## Load Packages -----------------

library(shiny)
library(pls)
library(shinyBS)
source("simrelM.R")
source("PlotSimrelM.R")

shinyServer(function(input, output, session) {
  parseText <- function(x) {
    evl <- function(x) as.numeric(unlist(strsplit(x, split = ",")))
    out <- lapply(strsplit(x, ";")[[1]], evl)
    if (length(out) > 1) return(out)
    return(out[[1]])
  }
  
  ## Simulation Object from Simrel Package ---------------
  simObj <- reactive({
    set.seed(input$newSeed)
    par <- list(n = input$n,
                p = input$p,
                m = input$m,
                q = parseText(input$q),
                relpos = parseText(input$relpos),
                gamma = input$gamma,
                R2 = parseText(input$R2),
                ntest = input$n_test,
                ypos = parseText(input$ypos))
    sim.obj <- do.call(simrelM, par)
    dt.train <- data.frame(y = I(sim.obj$Y), x = I(sim.obj$X))
    dt.test <- data.frame(y = I(sim.obj$testY), x = I(sim.obj$testX))
    if (input$model == "Least Square Estimation") {
      mdl <- lm(y ~ x, data = dt.train)
    } else if (input$model == "Principal Component Regression") {
      mdl <- pcr(y ~ x, data = dt.train, validation = "CV")
    } else if (input$model == "Partial Least Square Regression") {
      mdl <- plsr(y ~ x, data = dt.train, validation = "CV")
    } else if (input$model == "Envelope MLE") {
      mdl <- NA
    }
    list(sim.obj = sim.obj, dt.train = dt.train, dt.test = dt.test, mdl = mdl)
  })
  ## Description of Paramters ----------------------------------
  args <- structure(list(n = "The number of (training) samples to generate.", 
                         p = "The total number of predictor variables to generate.", 
                         m = "The number of relevant latent components to be used. See details below and Helland and Almøy (1994) for details on the concept of relevant components.", 
                         q = "The number of relevant predictor variables (as a subset of p).", 
                         relpos = "A vector indicating the position (between 1 and p) of the m relevant components, e.g. c(1,2) means that the first two latent components should be relevant. The length of relpos must be equal to m.", 
                         gamma = "A number defining the speed of decline in eigenvalues (variances) of the latent components. The eigenvalues are assumed to decline according to an exponential model. The first eigenvalue is set equal to 1.", 
                         R2 = "The theoretical R-squared according to the true linear model. A number between 0 and 1.", 
                         n_test = "The number of test samples to be generated (optional)."), 
                    .Names = c("n", "p", "m", "q", "relpos", "gamma", "R2", "n_test"))
  
  ## Creating Tooltips -----------------------------------
  # addTooltip(session, args["n", 'Arguements'], args["n", "Description"], 
  #                     placement = "top", trigger = "hover")
  # addTooltip(session, args["ntest", 'Arguements'], args["n_test", "Description"], 
  #                     placement = "top", trigger = "hover")
  addTooltip(session, "n_test", args[["n_test"]], placement = "left", trigger = "hover")
  addTooltip(session, "n", args[["n"]], placement = "bottom", trigger = "hover")
  addTooltip(session, "gamma", args[["gamma"]], placement = "top", trigger = "hover")
  addTooltip(session, "p", args[["p"]], placement = "top", trigger = "hover")
  addTooltip(session, "m", args[["m"]], placement = "top", trigger = "hover")
  addTooltip(session, "relpos", args[["relpos"]], placement = "top", trigger = "hover")
  
  ## Overview Simulation Plots ------------------------------------
  output$simPlot1 <- renderPlot({
    PlotSimrelM(simObj()[["sim.obj"]], which = "TrueBeta")[[1]] + coord_flip()
  })
  output$simPlot2 <- renderPlot({
    PlotSimrelM(simObj()[["sim.obj"]], which = "RelComp")[[1]]
  })
  output$simPlot3 <- renderPlot({
    PlotSimrelM(simObj()[["sim.obj"]], which = "EstRelComp")[[1]]
  })
  output$R2plot <- renderPlot({
    dta <- data.table(melt(simObj()$sim.obj$RsqY))[Var1 < Var2, value := NA]
    dta <- dta[, c("Var1", "Var2") := lapply(.SD, function(x) paste0("Y",x)), 
               .SDcols = c("Var1", "Var2")]
    plt <- ggplot((dta), aes(Var1, Var2, fill = value)) + 
      geom_tile() + geom_text(aes(label = value, size = 1 + abs(value)), na.rm = T) + 
      scale_fill_continuous("Cofficient of\nDetermination", 
                            low = "#16A085", high = "#F4D03F", 
                            space = "Lab", na.value = "white") + 
      theme_minimal() + coord_cartesian(expand = FALSE) + 
      theme(legend.position = c(0, 0.9), 
            legend.justification = "left", 
            legend.direction = "horizontal", 
            legend.key.width = unit(rel(1), "cm"),
            axis.text.y = element_text(angle = 90)) + 
      guides(fill = guide_legend(title.position = "top", 
                                 label.position = "bottom", 
                                 title.hjust = 0.5)) + 
      labs(x = '', y = '') + 
      scale_size(breaks = NULL)
    plt
  })
  
  ## Least Square Output and Plots ------------------------------
  output$lmsummary <- renderPrint({
    summary(simObj()$mdl)[[as.numeric(input$which_response)]]
  })
  output$lmCoef <- renderPlot({
    trueCoef <- melt(rbind(simObj()$sim.obj$beta0, simObj()$sim.obj$beta))
    estCoef <- melt(unname(simObj()$mdl$coefficients))
    coefMat <- rbindlist(list(true = trueCoef, estimated = estCoef), 
                         fill = TRUE, idcol = TRUE)
    setnames(coefMat, names(coefMat), c("CoefType", "variable", "response", "BetaCoefficients"))
    coefMat[, variable := as.numeric(variable) - 1]
    coefMat <- coefMat[response == as.numeric(input$which_response)]
    coefMat[, variable := as.character(variable)]
    coefMat[variable == '0', variable := "(I)"]
    plt <- ggplot(coefMat, aes(x = 1, y = BetaCoefficients, fill = CoefType)) + 
      geom_bar(width = 2, stat = "identity", position = "dodge") + 
      coord_flip() + 
      facet_grid(variable ~ ., shrink = TRUE) + 
      scale_fill_brewer(palette = 'Set2') + labs(x = '') + 
      scale_x_continuous(breaks = NULL) + 
      theme(legend.position = "top")
    plt
  })
  
  ## MVR model ------------------------------
  output$mvrVarExpl <- renderDataTable({
    y.varexpl <- round(t(drop(R2(simObj()$mdl, intercept = FALSE)$val)) * 100, 2)
    x.varexpl <- round(cumsum(explvar(simObj()$mdl)), 2)
    dt <- data.table(cbind(X = x.varexpl, y.varexpl), keep.rownames = TRUE)
    setnames(dt, "rn", "ncomp")
  }, options = list(dom = 't'))
  output$mvrRMSEP <- renderDataTable({
    rmsep <- dcast(melt(drop(round(RMSEP(simObj()$mdl, intercept = FALSE)$val, 3))), 
                   estimate + model ~ response)
    rmsep <- data.table(rmsep)[estimate == "CV"]
  }, options = list(dom = 't'))
  
## ---- Plot and Analysis Menu ---------------------------  
  output$plot.ui <- renderUI({
  if (input$model == "Least Square Estimation") {
      if (input$n <= input$p) {
        wellPanel(h1("It is not possible to use Mulitple Linear Regression when number of observations is less than number of variables", class = "text-center"))
      } else {
      tabItem(tabName = "plots",
        fluidRow(
          box(
            selectInput(
              "which_response", label = "Response:",
              choices = `names<-`(
                1:ncol(simObj()$mdl$coef), paste0("Response:", colnames(simObj()$mdl$coef))
              )
            ),
            column(9,
              verbatimTextOutput('lmsummary')
            ), 
            column(3,
              plotOutput('lmCoef', height = '600px')
            ),
            width = 12, title = "Least Square Summary", solidHeader = TRUE, status = "primary"
          )
        )
      )
    }}
  else if (any(input$model == "Principal Component Regression",
               input$model == "Partial Least Square Regression")) {
      tabItem(tabName = "plots",
        fluidRow(
          column(12,
            tabsetPanel(
              tabPanel(title = "Variation Explained (R2)",
                dataTableOutput("mvrVarExpl")
              ),
              tabPanel(title = "Validation(RMSEP)",
                dataTableOutput("mvrRMSEP")
              )
            )
          ),
          column(12,
            "Something"
          )
        )
      )
  } else if (input$model == "Envelope MLE") {
      
  } else {
      
    }
  })
  
## ---- Analysis Menu Contents -----------------------------------------------
  output$analysis.ui <- renderUI({
    if (input$model == "Least Square Estimation") {
      if (input$n <= input$p) {
        wellPanel(h1("It is not possible to use Mulitple Linear Regression when number of observations is less than number of variables", class = "text-center"))
      } else {
        tabItem(tabName = "analysis",
                wellPanel(h1("Some Analysis"))
        )
      }}
    else {
      tabItem(tabName = "analysis")
    }
  })
})
