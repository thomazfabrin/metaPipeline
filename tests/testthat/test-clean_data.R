test_that("clean_data function works correctly", {
  # Create a dummy dataframe for testing
  df <- data.frame(
    "Column 1" = c("a", "b", "c"),
    "Column 2" = c("d", "e", "f"),
    check.names = FALSE
  )

  # Call the clean_data function
  df_cleaned <- metaPipeline::clean_data(df, clean_column = TRUE)

  # Check that the function returns a dataframe
  expect_is(df_cleaned, "data.frame")

  # Check that the column names have been cleaned
  expect_equal(colnames(df_cleaned), c("column_1", "column_2"))

  # Check that the specified column has been cleaned
  expect_equal(df_cleaned$column_1, c("a", "b", "c"))

})
