#' Functions for plotting stochastic and stratified sampling outputs.
#' @import ggplot2
#' @import scales


#' Plot estimates and their (transformed) CIs against an x-axis, e.g. for hazard curves
#' with confidence intervals against threshold.
#'
#' @param x_var Variable to plot on the x-axis.
#' @param ests Variable to plot on the y-axis, e.g. stratified sampling estimates.
#' @param g A transformation to be applied to the `ests`, provided as an expression of `x`.
#' @param plot_ci Whether to plot a ribbon of the confidence intervals.
#' @param title Plot title.
#' @param x_name x-axis title.
#' @param y_name y-axis title.
#' @param x_labels x-axis labels.
#' @param y_labels y-axis labels.
#' @param x_lim x-axis labels.
#' @param y_lim y-axis labels.
#' @param ... Additional inputs to be passed to `get_cis`.
#'
#' @return Plot of `ests` against `x_var`, with confidence intervals.
#' @export
ests_plot <- function(x_var, ests, g = NULL,
                      plot_ci = FALSE, var_ests = NULL,
                      title = "", x_name = "", y_name = "",
                      x_labels = waiver(), y_labels = waiver(),
                      x_lim = NULL, y_lim = NULL, ...) {
  if (length(x_var) != length(ests)) stop("Length of ests must match length of x_var.")

  if (!is.null(g)) { # Transform estimates
    x <- ests
    g_ests <- eval(g)
    # Construct data frame for ggplot
    df <- data.frame(x_var = x_var, estimate = g_ests)
  } else df <- data.frame(x_var = x_var, estimate = ests)

  if (plot_ci) { # Get confidence intervals
    if (is.null(var_ests)) stop("var_ests required for plotting confidence intervals.")

    cis_list <- get_cis_list(ests, var_ests, g, ...)

    # Get transformed CIs only
    if (is.expression(g)) cis_list <- lapply(cis_list, `[[`, "g_ci")

    df$ci_lower <- unlist(lapply(cis_list, `[[`, 1))
    df$ci_upper <- unlist(lapply(cis_list, `[[`, 2))

    if (is.null(y_lim)) y_lim <- c(min(df$ci_lower[df$ci_lower > -Inf]),
                                   max(df$ci_upper))
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

#' Plot stratum estimates and their (transformed) CIs against an x-axis.
#'
#' @param x_var Variable of length N to plot on the x-axis.
#' @param stratum_means List of length J containing N-vectors of stratum estimates.
#' @param weights Vector of stratum weights of length J. Must sum to one.
#' @param stratum_names Vector of length J of names of strata, to construct plot legend.
#' @param g A transformation to be applied to the `ests`, provided as an expression of `x`.
#' @param title Plot title.
#' @param x_name x-axis title.
#' @param y_name y-axis title.
#' @param legend_position Legend position.
#' @param legend_label Legend title.
#' @param facet Whether to apply `facet_wrap` to strata.
#' @param cols Number of columns in faceted plot.
#' @param rows Number of rows in faceted plot.
#' @param scales Scales of faceted plot (one of `"fixed"`, `"free"`, `"free_x"`,
#' `"free_y`).
#' @param x_labels x-axis labels.
#' @param y_labels y-axis labels.
#' @param x_lim x-axis labels.
#' @param y_lim y-axis labels.
#'
#' @return Plot of `stratum_means` against `x_var`.
#' @export
stratum_means_plot <- function(x_var, stratum_means, weights, stratum_names = NULL,
                               g = NULL, title = "", x_name = "", y_name = "",
                               legend_position = "right", legend_label = "",
                               facet = TRUE, cols = NULL, rows = NULL, scales = "fixed",
                               x_labels = waiver(), y_labels = waiver(),
                               x_lim = NULL, y_lim = NULL) {
  if (sum(weights) != 1) stop("Sum of weights must be unity.")

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
    labs(colour = legend_label) +
    ggtitle(title) +
    theme(legend.position = legend_position)

  if (facet) p <- p + facet_wrap(vars(stratum), ncol = cols, nrow = rows, scales = scales)

  return(p)
}
