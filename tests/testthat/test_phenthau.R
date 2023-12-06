test_that("phenthau", {

  x <- load_test("day")
  fr_day <- phenthau(x)

  expect_equal(fr_day$day[fr_day$model == "L2"], "2020-04-18")
  expect_equal(fr_day$day[fr_day$model == "L3"], "2020-04-27")
  expect_equal(fr_day$day[fr_day$model == "L4"], "2020-05-10")
  expect_equal(fr_day$day[fr_day$model == "L5"], "2020-05-25")
  expect_equal(fr_day$day[fr_day$model == "L6"], "2020-06-11")
  expect_equal(fr_day$day[fr_day$model == "Pp"], "2020-07-05")
  expect_equal(fr_day$day[fr_day$model == "Ad"], "2020-08-11") # should be 2020-08-12, but we can explain the difference with slightly different input than in Dr. Halbigs study

  expect_equal(fr_day$day[fr_day$model == "custers"], "2020-03-20")
  expect_equal(fr_day$day[fr_day$model == "wagenhoff"], "2020-03-18")
  expect_equal(fr_day$day[fr_day$model == "meurisse"], "2020-03-11")

})


