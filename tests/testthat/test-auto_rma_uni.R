# test-auto_rma_uni.R
test_that("auto_rma_uni function works correctly", {
    # Create a dummy dataframe for testing
    df <- data.frame(
        yi = c(1.2, 2.3, 3.4, 4.5, 5.6),
        vi = c(0.1, 0.2, 0.3, 0.4, 0.5),
        group = c("A", "A", "B", "B", "B"))

    # Call the auto_rma_uni function without grouping
    rma_uni <- metaPipeline::auto_rma_uni(df, group = FALSE, effect = "DL")

    # Check that the function returns an object of class rma.uni
    expect_is(rma_uni, "rma.uni")

    # Call the auto_rma_uni function with grouping
    rma_uni_list <- metaPipeline::auto_rma_uni(df, group = "group", effect = "DL")

    # Check that the function returns a list
    expect_is(rma_uni_list, "list")

    # Check that each element of the list is an object of class rma.uni
    for (rma_uni in rma_uni_list) {
        expect_is(rma_uni, "rma.uni")
    }
})
