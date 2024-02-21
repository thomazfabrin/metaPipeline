#' @title rma_list_to_summary_df()
#' @description Summarize the list of rma results.
#' @param {rma_list} A list containing rma.uni results from
#' metaPipeline::auto_rma_uni() with groups.
#' @return A dataframe containing a summary of rma results.
#'
#' @export
#'
rma_list_to_summary_df <- function(rma_list) {

  df <- data.frame()
  for (i in seq_along(rma_list)) {

    df_for <- data.frame(
      group = unique(rma_list[[i]]$data$.y),
      estimate = rma_list[[i]]$b,
      se = rma_list[[i]]$se,
      lb = rma_list[[i]]$ci.lb,
      ub = rma_list[[i]]$ci.ub,
      tau2 = rma_list[[i]]$tau2,
      i2 = rma_list[[i]]$I2,
      pval = rma_list[[i]]$pval,
      method = rma_list[[i]]$method,
      n_entries = nrow(rma_list[[i]]$data$.x),
      index = i
    )
    rownames(df_for) <- NULL
    df <- dplyr::bind_rows(df, df_for)

  }

  return(df)

}
