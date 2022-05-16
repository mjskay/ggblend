
# operation application ---------------------------------------------------

test_that("adjust works as an identity", {
  expect_equal_layer(adjust() * geom_line(), geom_line())
  expect_equal_layer(geom_line() * adjust(), geom_line())
  expect_equal_layer(geom_line() |> adjust(), geom_line())
})

test_that("adjusting params works", {
  expect_equal_layer(adjust(bins = 20) * stat_summary_bin(), stat_summary_bin(bins = 20))
  expect_equal_layer(stat_summary_bin() * adjust(fatten = 5), stat_summary_bin(fatten = 5))
  expect_equal_layer(stat_summary_bin(fatten = 3) |> adjust(fatten = 5), stat_summary_bin(fatten = 5))
})

test_that("adjusting aesthetics works", {
  expect_equal_layer(adjust(aes(color = z)) * stat_summary_bin(), stat_summary_bin(aes(color = z)))
  expect_equal_layer(stat_summary_bin() * adjust(mapping = aes(color = z)), stat_summary_bin(aes(color = z)))
  expect_equal_layer(stat_summary_bin(aes(color = g)) |> adjust(aes(color = z, fill = y)), stat_summary_bin(aes(color = z, fill = y)))
})


# self-multiplication ----------------------------------------------------------

test_that("adjust multiplied with itself merges into one adjust", {
  expect_equal(adjust() * adjust(), adjust())
  expect_equal(adjust(size = 2) * adjust(size = 3), adjust(size = 3))
  expect_equal(adjust(size = 2) * adjust(color = "red"), adjust(size = 2, color = "red"))
  expect_equal((1 + adjust(size = 2)) * adjust(color = "red"), adjust(color = "red") + adjust(size = 2, color = "red"))
})

