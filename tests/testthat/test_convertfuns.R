test_that("convert_hour_to_meanminmax",{

  x <- load_test("hour")[1:48, c("date", "tmean")]
  x <- convert_df_to_srl(x)
  x <- convert_hour_to_meanminmax(x)
  x <- round(as.data.frame(x), 1)

  expect_equal(x[[1]], 21.4)
  expect_equal(x[[2]], 17.5)
  expect_equal(x[[3]], 18.8)
  expect_equal(x[[4]], 11.6)
  expect_equal(x[[5]], 24)
  expect_equal(x[[6]], 22)

})




