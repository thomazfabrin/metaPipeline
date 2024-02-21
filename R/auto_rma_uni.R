#' @title auto_rma_uni()
#' @description List of rma.uni objects created according to subgroups.
#' @param data Object of class "escalc" from metafor package.
#' @param group Group column, if data should be grouped. Default is FALSE.
#' @param effect Model to calculate the effect according
#' to metafor::rma.uni. Default is "DL".
#' @return A list containing objects of class rma.uni
#' or a unique rma.uni object.
#' @export
#' @examples
#' library(metafor)
#' rma_uni_list <- auto_rma_uni(dat.riley2003, group = "outcome", effect = "DL")
auto_rma_uni <- function(data, group = FALSE, effect = "DL") {

  if (isFALSE(group)) {
      cat(paste0("No grouping. \n"))
      rma_list <- metafor::rma.uni(
        yi = data$yi, vi = data$vi, method = effect)
      cat(paste0("Returning rma.uni object. \n"))
      return(rma_list)
  } else {
      cat(paste0("You have selected '", group, "' as factor. Groups are: \n"))
      cat(paste0(unique(data[[group]]), collapse = ",\n"))
      cat("\n")
      rma_list_with_factors <- data |>
        dplyr::group_by((!!rlang::sym(group))) |>
        dplyr::group_map(
          ~ metafor::rma.uni(yi = .$yi, vi = .$vi, method = effect))
      cat("Returning rma.uni object grouped. \n")
      return(rma_list_with_factors)
    }

  }
