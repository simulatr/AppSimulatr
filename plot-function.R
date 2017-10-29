## ---- ggplot common legend -----------------
share_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  require(gridExtra)
  require(grid)
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(
    position,
    "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                           legend,
                           ncol = 1,
                           heights = unit.c(unit(1, "npc") - lheight, lheight)),
    "right" = arrangeGrob(do.call(arrangeGrob, gl),
                          legend,
                          ncol = 2,
                          widths = unit.c(unit(1, "npc") - lwidth, lwidth))
  )
  grid.newpage()
  grid.draw(combined)
  return(combined)
}

## ---- Prepare data for covariance plot ------------------------------
get.cov.df <- function(cov.mat, n.xvar, n.yvar, relpos, xvar.name = "X",  yvar.name = "Y", relpred = NULL, ordering = TRUE, ypos) {
  mat.names <- c(paste0(yvar.name, 1:n.yvar),
                 paste0(xvar.name, 1:n.xvar))
  rownames(cov.mat) <- colnames(cov.mat) <- mat.names
  abs.cov <- abs(cov.mat)
  lgl.cov <- abs.cov != 0
  abs.cov.df <- reshape2::melt(abs.cov)
  abs.cov.df$mat <- gsub("[[:digit:]]", "", paste(abs.cov.df$Var1, abs.cov.df$Var2, sep = ":"))
  abs.cov.df <- tidyr::separate(abs.cov.df, mat, c("facet1", "facet2"), sep = ":", remove = FALSE)

  flatten_list <- function(list, yvar.name, xvar.name) {
    out <- apply(melt(unname(list)), 1, function(l) paste0(c(xvar.name, yvar.name), l))
    `names<-`(out[2, ], out[1, ])
  }
  relpos.vec <- flatten_list(relpos, yvar.name, xvar.name)
  relpred.vec <- flatten_list(relpred, yvar.name, xvar.name)
  ypos.vec <- flatten_list(ypos, yvar.name, yvar.name)
  get_relvec <- function(df, relvec) {
    rpos1 <- relvec[df["Var1"]]
    rpos2 <- relvec[df["Var2"]]
    if (all(!is.na(c(rpos1, rpos2)))) {
      out <- if (as.numeric(df["value"]) == 0) "None" else rpos1
      return(out)
    } else {
      if (as.numeric(df["value"]) == 0) return("None")
      if (all(c(is.na(c(rpos1, rpos2)), as.numeric(df["value"]) != 0))) return("None")
      if (is.na(rpos1)) return(rpos2)
      if (is.na(rpos2)) return(rpos1)
    }
  }

  irrelpred.idx <- setdiff(paste0(xvar.name, 1:n.xvar), names(relpred.vec))
  irrelpred.vec <- `names<-`(rep(NA, length(irrelpred.idx)), irrelpred.idx)
  abs.cov.df$rotation <- apply(abs.cov.df, 1, get_relvec, relvec = c(relpred.vec, ypos.vec, irrelpred.vec))
  abs.cov.df$relpred <- apply(abs.cov.df, 1, get_relvec, relvec = c(relpred.vec, ypos.vec))
  abs.cov.df$relpos <- apply(abs.cov.df, 1, get_relvec, c(relpos.vec, ypos.vec[paste0(yvar.name, seq_along(ypos))]))

  abs.cov.df$facet1 <- factor(abs.cov.df$facet1, levels = c(yvar.name, xvar.name))
  abs.cov.df$facet2 <- factor(abs.cov.df$facet2, levels = c(yvar.name, xvar.name))

  # # Ordering of predpos ------------------------------
  relpred.numvec <- paste0(xvar.name, unname(unlist(relpred)))
  irrelpred.numvec <- setdiff(paste0(xvar.name, seq.int(n.xvar)), relpred.numvec)
  ypos.numvec <- paste0(yvar.name, unname(unlist(ypos)))
  if (ordering) {
    xy.idx <- c(ypos.numvec, c(relpred.numvec, irrelpred.numvec))
  } else {
    xy.idx <- c(paste0(yvar.name, seq.int(n.yvar)), paste0(xvar.name, seq.int(n.xvar)))
  }
  abs.cov.df$Var1 <- factor(abs.cov.df$Var1, levels = xy.idx)
  abs.cov.df$Var2 <- factor(abs.cov.df$Var2, levels = rev(xy.idx))

  abs.cov.df$relpos <- factor(abs.cov.df$relpos, levels = unique(sort(abs.cov.df$relpos, decreasing = T)))
  abs.cov.df$rotation <- factor(abs.cov.df$rotation, levels = unique(sort(abs.cov.df$rotation, decreasing = T)))
  abs.cov.df$relpred <- factor(abs.cov.df$relpred, levels = unique(sort(abs.cov.df$relpred, decreasing = T)))

  class(abs.cov.df) <- append("covdf", class(abs.cov.df))

  return(abs.cov.df)
}

## ---- Get covariance data for given simrel object ------------------------------
cov.df <- function(sim.obj, type = "relpos", ordering = NULL) {
  pars <- list(
    cov.mat = switch(
      type,
      relpos = sim.obj$SigmaWZ,
      relpred = sim.obj$Sigma,
      rotation = rbind(cbind(sim.obj$Yrotation, matrix(0, sim.obj$m, sim.obj$p)),
                       cbind(t(matrix(0, sim.obj$m, sim.obj$p)), sim.obj$Xrotation))
    ),
    n.xvar = sim.obj$p,
    n.yvar = sim.obj$m,
    relpos = sim.obj$relpos,
    relpred = sim.obj$relpred,
    ypos = sim.obj$ypos,
    ordering = ifelse(is.null(ordering), ifelse(type == "relpos", FALSE, TRUE), ordering),
    yvar.name = if (type == "relpos") "W" else "Y",
    xvar.name = if (type == "relpos") "Z" else "X"
  )
  do.call(get.cov.df, pars)
}

## ---- Plotting Function for covariance matrix ------------------------------
plot.covdf <- function(covdf.obj, type) {
  plt <- ggplot(covdf.obj, aes_string("Var1", "Var2", fill = type)) +
    geom_tile(color = "lightgray", aes(alpha = value ^ (1/4))) +
    facet_grid(facet2 ~ facet1, scales = "free", space = "free") +
    scale_alpha_continuous(range = c(0, 1), guide = "none") +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom", strip.text = element_blank(),
          ## panel.background = element_rect(color = "darkgray", fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(family = "mono"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "", y = "") +
    scale_fill_brewer("Relevant for:", palette = "Set2", na.value = "#ffeeee")
  return(plt)
}

## ---- Observation Plot ------------------------------
get_obsplot <- function(sim.obj, before.rot = TRUE) {

  ## Set X and Y names
  xvar.name <- if (before.rot) "Z" else "X"
  yvar.name <- if (before.rot) "W" else "Y"

  ## Function to flatten list into vector
  flatten_list <- function(lst, yvar.name = NULL, xvar.name = NULL) {
    out <- unname(t(melt(lst)))
    if (all(is.null(c(yvar.name, xvar.name))))
      out <- `names<-`(as.numeric(out[2, ]), out[1, ])
    else
      out <- `names<-`(paste0(yvar.name, out[2, ]), paste0(xvar.name, out[1, ]))

    return(out)
  }

  ## Create DataFrame out of data matrix
  dff <- melt(`colnames<-`(sim.obj[[xvar.name]],
                           paste0(xvar.name, 1:ncol(sim.obj[[xvar.name]]))))

  ## Identify the position of relevant predictors (components)
  ypos <- sim.obj$ypos
  relpos <- sim.obj[["relpos"]] ## Needed for coloring
  predpos <- unname(sim.obj[["relpred"]]) ## Needed for ordering

  ## Vector for coloring
  relpos.vec <- flatten_list(relpos, yvar.name, xvar.name)
  dff$relpos <- relpos.vec[as.character(dff$Var2)]
  dff$relpos[is.na(dff$relpos)] <- "None"

  ## Ordering
  predpos.vec <- flatten_list(predpos, yvar.name, xvar.name)
  predpos.idx <- c(names(predpos.vec),
                   setdiff(paste0(xvar.name, 1:sim.obj$p), names(predpos.vec)))
  dff$Var2 <- factor(as.character(dff$Var2), levels = predpos.idx)

  ## Adding Relevant Predictors vectors
  dff$predpos <- predpos.vec[as.character(dff$Var2)]
  dff$predpos[is.na(dff$predpos)] <- "None"

  ## Give sensible names
  names(dff) <- c("Obs.Idx", "Variables", "Obs.Value",
                  "RelevantBefore", "RelevantAfter")

  ## Tweaking some details
  fill_colour_aes <- if (before.rot) "RelevantBefore" else "RelevantAfter"
  dff$RelevantAfter <- factor(dff$RelevantAfter, levels = unique(sort(dff$RelevantAfter, decreasing = T)))
  dff$RelevantBefore <- factor(dff$RelevantBefore, levels = unique(sort(dff$RelevantBefore, decreasing = T)))

  ## Plottings
  ThePlot <- ggplot(dff, aes(Variables, Obs.Value)) +
    geom_point(size = rel(1), shape = 23,
               aes_string(color = fill_colour_aes)) +
    geom_boxplot(aes_string(fill = fill_colour_aes),
                 alpha = 0.5, size = 0.2, color = "darkgray") +
    labs(x = "", y = "") +
    theme_minimal(base_size = 8) +
    theme(panel.background = element_rect(fill = NA, color = "#ababab"),
          strip.text.x = element_text(angle = 90, hjust = 0.5, vjust = 1),
          legend.position = "bottom") +
    scale_fill_brewer("Relevant for:", palette = "Set2") +
    scale_color_brewer("Relevant for:", palette = "Set2")
  return(ThePlot)
}

## ---- Prediction Error Plot ----------------------------------------
get_pred_plot <- function(pred_err, type_of_error = "average") {
    ols_err <- pred_err %>%
      filter(Model == "OLS", comp == 1)

    pred_err <- pred_err %>% filter(Model != "OLS")
    predErr_smry <- pred_err %>%
    group_by(Model, design, comp) %>%
    summarise(minimum = min(pred_err), average = mean(pred_err), maximum = max(pred_err)) %>%
    gather(`Type of Error`, pred_err, -Model, -design, -comp)

  PE <- predErr_smry %>%
    filter(`Type of Error` == type_of_error) %>%
    group_by(Model, design) %>%
    summarise(comp = comp[which.min(pred_err)],
              pred_err = min(pred_err))

  winner <- PE %>%
    group_by(design) %>%
    summarise(
      Model = Model[which.min(pred_err)],
      comp = comp[which.min(pred_err)],
      pred_err = min(pred_err)
    ) %>% mutate(label = "Winner")

  runnerup <- PE %>%
    group_by(design) %>%
    summarize(
      order = order(pred_err, decreasing = F)[2],
      pred_err = pred_err[order],
      comp = comp[order],
      Model = Model[order],
      label = "RunnerUp"
    )

  # design_details <- pred_err %>%
  #   filter(Model == "OLS") %>%
  #   group_by(Model, design) %>%
  #   transmute(
  #     label = paste0("R2: ", R2, "\ngamma: ", gamma)
  #   ) %>% unique()

  plt <- qplot(comp, pred_err, group = id, geom = c("line", "point"), size = I(0.4),
               data = pred_err, facets = design ~ Model, color = I("grey")) +
    geom_line(data = predErr_smry, inherit.aes = FALSE, size = 0.4,
              aes(color = `Type of Error`, x = comp, y = pred_err)) +
    geom_point(data = predErr_smry, inherit.aes = FALSE, size = 0.4,
               aes(color = `Type of Error`, x = comp, y = pred_err)) +
    scale_x_continuous(breaks = seq(0, 10, 2)) +
    geom_point(data = PE, inherit.aes = FALSE, size = 0.8,
               aes(x = comp, y = pred_err), shape = 21, color = "black") +
    geom_text(data = PE, inherit.aes = FALSE, family = "mono", size = 2.7, y = -Inf, vjust = -1,
              aes(x = comp, label = paste0("(", comp, ", ", round(pred_err, 3), ")"))) +
    geom_text(data = winner, aes(label = label, x = 10, y = Inf), family = "mono",
              inherit.aes = FALSE, color = "red", size = 3, vjust = 1, hjust = 1) +
    geom_text(data = runnerup, aes(label = label, x = 10, y = Inf), family = "mono",
              inherit.aes = FALSE, color = "blue", size = 3, vjust = 1, hjust = 1) +
    labs(x = "Number of Components", y = "Prediction Error") +
    theme(legend.position = "bottom") +
    ggtitle("Minimum/Average/Maximum Prediction Error",
            subtitle = "The mimum error over all component is shown in brackets") +
    coord_cartesian(ylim = c(1.5, 2.3))

  plot(plt)
}
