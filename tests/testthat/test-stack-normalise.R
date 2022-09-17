test_that("stack normalise works", {

  expect_equal(stack_normalise(c(10, 30, 40)),
               c(0.125, 0.375, .5))
  expect_equal(stack_normalise(c(75, 0, 5, 20, NA)),
               c(0.75, 0, 0.05, 0.2, NA))
  expect_equal(stack_normalise(c(NA, NA, 10)),
               c(NA, NA, 1))
  expect_true(all(stack_normalise(rexp(30, 10)) >= 0))
  expect_true(all(stack_normalise(rexp(30, 10)) <= 1))

})
