test_that("rma_list_to_summary_df function works correctly", {
    # Create a list of rma.uni objects for testing
    library(metafor)
    rma_list <- metaPipeline::auto_rma_uni(
        dat.riley2003, group = "outcome", effect = "DL")

    # Call the rma_list_to_summary_df function
    df_summary <- metaPipeline::rma_list_to_summary_df(rma_list)

    # Check that the function returns a dataframe
    expect_is(df_summary, "data.frame")

    # Check that the dataframe has the correct number of rows
    expect_equal(nrow(df_summary), length(rma_list))

    # Check that the dataframe has the correct columns
    expect_equal(
        colnames(df_summary),
        c("outcome", "estimate",
          "se", "lb", "ub", "tau2",
          "i2", "pval", "method",
          "n_entries", "index"))
})
