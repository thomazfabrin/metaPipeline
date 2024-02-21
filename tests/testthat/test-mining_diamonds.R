test_that("mining_diamonds function works correctly", {
    # Create a dummy summary dataframe for testing
    library(metafor)
    rma_summary <- metaPipeline::auto_rma_uni(
        dat.riley2003, group = "outcome") |>
        metaPipeline::rma_list_to_summary_df()

    # Call the mining_diamonds function
    df_diamond <- metaPipeline::mining_diamonds(
        rma_summary, main_group = "outcome", order = c("DFS", "OS"))

    # Check that the function returns a dataframe
    expect_is(df_diamond, "data.frame")

    # Check that the dataframe has the correct number of rows
    expect_equal(nrow(df_diamond), 4 * nrow(rma_summary))

    # Check that the dataframe has the correct columns
    expect_equal(
        colnames(df_diamond),
        c("x", "y", "lb", "ub",
          "i2", "n", "pval", "estimate",
          "group1", "group1_num", "index", "id"))
    }
)
