#' @title plot_diamonds()
#' @description Forest plot showing the summary results of
#' the dataframe obtained from rma_list_to_summary_df().
#' @param coordinates_df A dataframe containing diamonds coordinates obtained
#' from metaPipeline::mining_diamonds().
#' @param group1 String containing the variable used in y axis.
#' @param group2 String containing the variable used in facets in case of
#' subgrouping. Default is "none".
#' @param vline_ref X axis position to plot the reference vertical line.
#' Default is "none".
#' @param title String containing the title of the plot. Default is "none".
#' @param xlab String containing the x axis title of the plot. Default is
#' "none".
#' @param show_grids_v Show vertical grids if TRUE. Default is FALSE.
#' @param show_grids_h Show horizontal grids if TRUE. Default is FALSE.
#' @param ci_interval If TRUE, plot data showing estimate, lower and upper
#' bounds and p-values. Default is FALSE.
#' @param n_col Number of columns in case group 2 is used. If ci_interval is
#' used with group2, a single column should be used.
#' @param adj1 Value controlling the distance between the graph area and
#' ci_interval.
#' @param adj2 Value controlling the total width of the plot.
#' @param order String similar to the one used in metapipeline::mining_
#' diamonds() order.
#' @param ... Other arguments for the geom_polygon().
#' @return An object of class ggplot.
#' @export
#' @examples
#' #' library(metafor)
#' order <- c("DFS", "OS")
#' diamonds <- metaPipeline::auto_rma_uni(dat.riley2003, group = "outcome") |>
#'    metaPipeline::rma_list_to_summary_df() |>
#'    metaPipeline::mining_diamonds(main_group = "outcome", order = order) |>
#'    metaPipeline::plot_diamonds(
#'      title = "dat.riley2003 from metafor package",
#'      show_grids_v = TRUE, xlab = "SMD", group1 = group_plot,
#'      order = order, vline_ref = 0, adj1 = 1.2, adj2 = 16, ci_interval = TRUE)
plot_diamonds <- function(coordinates_df, group1, group2 = "none",
                             vline_ref = "none", title = "none",
                             xlab = "none", show_grid_v = FALSE,
                             show_grid_h = FALSE, ci_interval = FALSE,
                             n_col = 3, adj1, adj2, order, ...) {

  required_columns <- c("x", "y", "lb", "ub",
                        "i2", "n", "pval", "estimate",
                        "group1", "group1_num", "index", "id")

  if (!all(required_columns %in% names(coordinates_df))) {
    stop("Check coordinates_df columns. Missing or wrong columns.")
  }

  diamond_plot <-
      ggplot2::ggplot(
        data = coordinates_df,
        mapping = ggplot2::aes(
          x = estimate, y = factor(group1, levels = order),
          group = id)) +
      ggplot2::geom_point() +
      ggplot2::geom_polygon(
        mapping = ggplot2::aes(
          x = x, y = y, group = id, fill = group1),
          inherit.aes = TRUE) +
      {if (group2 != "none")
      ggplot2::facet_wrap(
        facets = ~ group2, ncol = n_col)} +
      {if (xlab != "none")
      ggplot2::xlab(
        xlab)} +
      {if (title != "none")
      ggplot2::labs(
        title = title)} +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          hjust = 0.5, color = "darkgray", face = "bold"),
        strip.background = ggplot2::element_blank()) +
      {if (show_grid_v)
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(
          color = "gray", linetype = "dashed"))} +
      {if (show_grid_h)
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(
          color = "gray", linetype = "dashed"))} +
      {if (ci_interval)
      ggplot2::geom_text(
        x = max(coordinates_df$ub) + adj1,
        y = coordinates_df[[group1]],
        label = paste0(format(round(coordinates_df$estimate, 2), ndigits = 2),
                       " [", format(round(coordinates_df$lb, 2), ndigits = 2),
                       ", ", format(round(coordinates_df$ub, 2), ndigits = 2),
                       "], ", "p-val = ", format(round(coordinates_df$pval, 2),
                                                       ndigits = 2), " (I^2 = ",
                       format(round(coordinates_df$i2, 2),
                                    ndigits = 2), ", n = ",
                      coordinates_df$n, ")"),
        size = 4, color = "darkgray", hjust = 0)} +
      {if (ci_interval)
      ggplot2::scale_x_continuous(
        limits = c(-max(coordinates_df$estimate) - 1,
        max(coordinates_df$ub) + 1))} +
      {if (ci_interval)
      ggplot2::coord_cartesian(
        xlim = c(-max(coordinates_df$estimate) - 1,
        max(coordinates_df$estimate) + 1),
        clip = "off")} +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.border = ggplot2::element_rect(color = "black", fill = NA),
        legend.position = "bottom") +
      {if (ci_interval)
      ggplot2::theme(
        plot.margin = ggplot2::unit(
          c(1, max(coordinates_df$estim) + adj2, 1, 1),
        "lines"))} +
      {if (vline_ref != "none")
      ggplot2::geom_vline(
        xintercept = vline_ref,
        linetype = "dashed", color = "red",
        linewidth = 0.8)}

  return(diamond_plot)

}
