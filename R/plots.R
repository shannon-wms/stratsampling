### PLOTTING FUNCTIONS ###

# Plot estimates and their (transformed) CIs against an x-axis, e.g. for hazard curves
# with confidence intervals against threshold
ests_plot <- function(x_var, ests, g = NULL,
                      plot_ci = FALSE, var_ests = NULL,
                      title = "", x_name = "", y_name = "",
                      x_labels = waiver(), y_labels = waiver(),
                      x_lim = NULL, y_lim = NULL, ...) {
  if (!is.null(g)) { # Transform estimates
    x <- ests
    g_ests <- eval(g)
    # Construct data frame for ggplot
    df <- data.frame(x_var = x_var, estimate = g_ests)
  } else df <- data.frame(x_var = x_var, estimate = ests)

  if (plot_ci) { # Get confidence intervals
    if (is.null(var_ests)) stop("var_ests required for plotting confidence intervals.")
    cis_list <- get_cis_list(ests, var_ests, g, ...)
    if (is.expression(g)) cis_list <- lapply(cis_list, `[[`, "g_ci")

    df$ci_lower <- unlist(lapply(cis_list, `[[`, 1))
    df$ci_upper <- unlist(lapply(cis_list, `[[`, 2))

    if (is.null(y_lim)) y_lim <- c(min(df$ci_lower[df$ci_lower > -Inf]), max(df$ci_upper))
  }

  if (is.null(x_lim)) x_lim <- c(min(x_var), max(x_var))

  p <- ggplot(df, aes(x = x_var, y = estimate)) +
    geom_line() +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    scale_x_continuous(name = x_name, labels = x_labels) +
    scale_y_continuous(name = y_name, labels = y_labels) +
    ggtitle(title)

  # Add CIs to plot
  if (plot_ci) p <- p + geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.7)

  return(p)
}

stratum_means_plot <- function(x_var, stratum_means, weights, stratum_names = NULL,
                               g = NULL, title = "", x_name = "", y_name = "",
                               legend_label = "", x_labels = waiver(), y_labels = waiver(),
                               x_lim = NULL, y_lim = NULL, ...) {
  n_strata <- length(weights)
  n_pts <- length(x_var)
  means <- unlist(stratum_means)
  if (is.null(stratum_names)) stratum_names <- 1:n_strata

  df <- data.frame(x_var = rep(x_var, each = n_strata),
                   stratum = rep(stratum_names, times = n_pts),
                   weights = rep(weights, times = n_pts))

  if (!is.null(g)) { # Transform estimates
    x <- means
    g_means <- eval(g)
    # Construct data frame for ggplot
    df$mean = g_means
  } else df$mean <- means

  p <- ggplot(df, aes(x = x_var, y = mean, colour = as.factor(stratum))) +
    geom_line() +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    scale_x_continuous(name = x_name, labels = x_labels) +
    scale_y_continuous(name = y_name, labels = y_labels) +
    labs(colour = legend_label)
    ggtitle(title)

  return(p)
}
