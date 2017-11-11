homepage <- function(){
  navbarPage(
    theme = shinythemes::shinytheme("yeti"),
    inverse = TRUE,
    title = "Simulation Overview",
    position = "static-top",
    tabPanel(
      "Home",
      icon = icon("home"),
      ## Main Container ----
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
        div(id = "covplot1",
            class = "grid-content",
            covPlotUI("relpos")),
        div(id = "covplot2",
            class = "grid-content",
            covPlotUI("rotation")),
        div(id = "covplot3",
            class = "grid-content",
            covPlotUI("relpred"))
      )
    )
  )
}
