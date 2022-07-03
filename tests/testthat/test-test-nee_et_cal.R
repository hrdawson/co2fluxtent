test_that("expexted result", {
  result <- nee_et_calc(path ="./inst/extdata", param = "nee")
  expect_equal(object = class(results), expected = "tbl")
})
