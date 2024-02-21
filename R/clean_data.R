#' @title clean_data()
#' @description Clean the dataframe before the analysis.
#' @param data Dataframe containing the data.
#' @param clean_column Column to be cleaned according to the
#' "case" parameter. Default is FALSE.
#' @param case Case type from janitor::make_clean_names().
#' Dafault is "snake".
#' @param allow_dupes Allow duplicated names. Default is FALSE.
#' @return Returns a cleaned dataset.
#' @export
#' @examples
#' df <- readr::read_csv(file.choose())
#' df_cleaned <- clean_data(df)
clean_data <- function(data, clean_column = FALSE, case = "snake",
                       allow_dupes = FALSE) {

  data_cleaned <- data
  to_clean <- clean_column

  if (clean_column) {

    for (column in to_clean) {
      data_cleaned[[column]] <- janitor::make_clean_names(
        data_cleaned[[column]],
        case = case,
        allow_dupes = allow_dupes)
    }

  }

  data_cleaned <- data_cleaned |>
    janitor::clean_names()

  return(data_cleaned)

}
