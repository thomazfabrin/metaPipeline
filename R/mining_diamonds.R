#' @title mining_diamonds()
#' @description Calculates diamond coordinates for the
#' ggplot2::geom_polygon().
#' @param rma_summary A df from rma_list_to_summary_df().
#' @param diamond_height Height of the diamond. Default is 0.1.
#' @param subgroup Number of subgroups. Default is 1; maximum is 2.
#' @param main_group String of the column name
#' that indicates the subgroup.
#' @param second_level_grouping Column name of the second level
#' group in case of two groups. Default is FALSE.
#' @param order Order of the group factors (ex. c("a", "b", "c").
#' The same order must be provided in the function
#' metaPipeline::show_me_diamonds().
#' @return A dataframe containing diamond coordinates for the
#' ggplot2::geom_polygon().
#' @export
#' @examples
#' library(metafor)
#' order <- c("DFS", "OS")
#' diamonds <- metaPipeline::auto_rma_uni(dat.riley2003, group = "outcome") |>
#'    metaPipeline::rma_list_to_summary_df() |>
#'    metaPipeline::mining_diamonds(main_group = "outcome", order = order)
mining_diamonds <- function(rma_summary, diamond_height = 0.1,
                                 subgroup = 1,
                                 main_group, second_level_grouping = FALSE,
                                 order) {

  df_diamond <- data.frame(
    x = c(rma_summary$lb, rma_summary$ub),
    y = as.numeric(c(factor(rma_summary[[main_group]], levels = order),
                     factor(rma_summary[[main_group]], levels = order))),
    lb = c(rma_summary$lb, rma_summary$lb),
    ub = c(rma_summary$ub, rma_summary$ub),
    i2 = c(rma_summary$i2, rma_summary$i2),
    n = c(rma_summary$n_entrie, rma_summary$n_entries),
    pval = c(rma_summary$pval, rma_summary$pval),
    estimate = c(rma_summary$estimate, rma_summary$estimate),
    group1 = c(rma_summary[[main_group]], rma_summary[[main_group]]),
    group1_num = as.numeric(c(as.factor(rma_summary[[main_group]]),
                              as.factor(rma_summary[[main_group]]))))

  if (subgroup == 2) {
    df_diamond$group2 <- c(rma_summary[[second_level_grouping]],
                           rma_summary[[second_level_grouping]])
    df_diamond <- df_diamond |>
      dplyr::arrange(group2, y) |>
      dplyr::mutate(
        index = seq.int(nrow(df_diamond)),
        id = paste(y, group2),
        y = dplyr::if_else(
          index %% 2 != 0,
          y - diamond_height,
          y + diamond_height))
   } else {
    df_diamond <- df_diamond |>
      dplyr::arrange(group1, y) |>
      dplyr::mutate(
        index = seq.int(nrow(df_diamond)),
        id = paste(y, group1),
        y = dplyr::if_else(
          index %% 2 != 0,
          y - diamond_height,
          y + diamond_height))
  }

  coordinates_y <- df_diamond |>
    dplyr::mutate(y = as.numeric(factor(group1, levels = order)))
  coordinates_x <- df_diamond |>
    dplyr::mutate(x = (lb + ub) / 2)
  df_diamond_coordinates <- rbind(coordinates_x, coordinates_y) |>
    dplyr::arrange(index)

  return(df_diamond_coordinates)

}
