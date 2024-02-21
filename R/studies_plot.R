#' @title {studies_plot()}
#'
#' @description
#' Plot showing the summary results of the studies obtained from
#' the scalc object from the metafor package.
#'
#' @param {data}
#' {escalc object containing measured values.}
#' @param {measure}
#' {Variable containing scalc function results.}
#' @param {weigth_factor}
#' {Size of dots follow weigth values. Default is factor 1.}

# studies_plot <- function(data, group_names,
#                          sort_by = c("citation", "yi", TRUE), xlabel,
#                          vline_ref = "none", summary_heigth = 0.7,
#                          show_summary = T, show_grids_v = F,
#                          show_grids_h = F) {
#
#   require(ggplot2)
#   require(dplyr)
#   require(purrr)
#   require(rlang)
#
#   filtered_list <- data %>%
#     keep(~ any(.$data$.y %in% group_names))
#
#   rma_list <- list()
#
#   for (i in filtered_list) {
#     filtered_list_with_weights <- c(i, wei = list(metafor::weights.rma.uni(i)))
#     rma_list <- append(rma_list, list(filtered_list_with_weights))
#    }
#
#   print("debug 1")
#
#   plot_list <- list()
#   plot_number <- 1
#   sorting <- as.logical(sort_by[3])
#
#
#   print("debug 2")
#
#   for (i in rma_list){
#     df <- data.frame(i$data$.x)
#     df <- df %>%
#       mutate(ci_lb = as.numeric(format(round(yi - 1.96 * sqrt(vi), 2),
#                                        nsmall = 2)),
#              ci_ub = as.numeric(format(round(yi + 1.96 * sqrt(vi), 2),
#                                        nsmall = 2)),
#              yi = as.numeric(format(round(yi, 2), nsmall = 2)))
#     diamonds_df <- data.frame(x = c((i$ci.lb + i$ci.ub)/2,
#                                     i$ci.lb,
#                                     (i$ci.lb + i$ci.ub)/2,
#                                     i$ci.ub),
#                               y = c((-.8 - summary_heigth), -.8,
#                                     (-.8 + summary_heigth), -.8))
#     df$wei <- i$wei
#     plot_name <- paste0("plot_", group_names[plot_number])
#     list_plot <-
#       ggplot(data = df,
#              mapping = aes(x = yi,
#                            y = reorder(.data[[sort_by[1]]],
#                                        as.numeric(as.factor(.data[[sort_by[2]]])),
#                                        decreasing = sorting),
#                            size = wei)) +
#       geom_point(shape = 15, color = "gray10", alpha = 0.8) +
#       geom_errorbar(mapping = aes(xmin = ci_lb, xmax = ci_ub), linewidth = 0.8,
#                     color = "gray20") +
#       {if (vline_ref != "none")
#       geom_vline(xintercept = vline_ref, linetype = "dashed", color = "red",
#                  size = 0.8)} +
#       {if (!isFALSE(show_summary))
#       scale_y_discrete(expand = expansion(mult = c(0.06,0)))} +
#       xlab(xlabel) +
#       labs(title = group_names[plot_number]) +
#       {if (!isFALSE(show_grids_v))
#       theme(panel.grid.major.x = element_line(color = "gray40",
#                                               linetype = "dashed"))} +
#       {if (!isFALSE(show_grids_h))
#       theme(panel.grid.major.y = element_line(color = "gray40",
#                                               linetype = "dashed"))} +
#       theme_classic() +
#       theme(plot.title = element_text(size = 13, color = "gray30",
#                                       face = "bold", hjust = 0.5),
#             legend.position = "none",
#             axis.title.y = element_blank(),
#             axis.title.x = element_text(size = 12, face = "bold"),
#             axis.text.x = element_text(size = 10))
#
#     if (!isFALSE(show_summary)){
#       plot_list[[plot_name]] <- list_plot +
#         geom_polygon(mapping = aes(x = x, y = y),
#                      data = diamonds_df,
#                      size = 1, fill = "darkgray") +
#         annotate(geom = "text", x= -Inf, y = -.8, label = "Summary",
#                  fontface = "bold", hjust = 1.1, size = 5) +
#         coord_cartesian(clip = "off")
#     }
#     else {
#       plot_list[[plot_name]] <- list_plot
#       }
#
#     {if (ci_interval == TRUE)
#       plot_list <- plot_list +
#         geom_text(x = max(diamonds_df$ub) + adj1, y = diamonds_df[[group1]],
#                   label = paste0( format(round(diamonds_df$estimate, 2),
#                                          ndigits = 2), " [",
#                                 format(round(diamonds_df$lb, 2),
#                                        ndigits = 2), ", ",
#                                 format(round(diamonds_df$ub, 2),
#                                        ndigits = 2), "], ", "p-val = ",
#                                 format(round(diamonds_df$pval, 2),
#                                        ndigits = 2)),
#                   size = 4, color = "darkgray", hjust = 0) +
#       scale_x_continuous(limits = c(min(diamonds_df$lb) - 1,
#                                     max(diamonds_df$ub) + 1)) +
#       coord_cartesian(xlim = c(min(diamonds_df$lb) - 1,
#                                max(diamonds_df$ub) + 1), clip = "off") +
#       theme(panel.background = element_rect(fill = "white"),
#             panel.border = element_rect(color = "black", fill = NA),
#             legend.position = "bottom",
#             plot.margin = unit(c(1, max(diamonds_df$ub) + adj2, 1, 1),
#                                "lines"))}
#   #   else {
#   #     plot_list[[plot_name]] <- list_plot
#   #     }
#   #
#   #   plot_number <- plot_number + 1
#   # }
#
#   print("debug 3")
#
#   return(plot_list)
#
#   }
