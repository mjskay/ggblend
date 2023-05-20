
# basic copies --------------------------------------------------------------

test_that("basic copy operations", {
  expect_equal(copy_over(aes(x = 1), color = "red"), 1 + adjust(aes(x = 1), color = "red"))
  expect_equal(copy_under(aes(x = 1), color = "red"), adjust(aes(x = 1), color = "red") + 1)
})
