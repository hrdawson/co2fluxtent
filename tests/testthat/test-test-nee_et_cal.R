test_that("expexted result", {
  result <- read_files("./inst/extdata")
  expect_equal(object = class(result),
               expected = "list")
})
