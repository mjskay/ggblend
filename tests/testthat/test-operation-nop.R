test_that("nop works", {
  expect_equal(nop() * geom_line(), geom_line())
  expect_equal(geom_line() * nop(), geom_line())
  expect_equal(geom_line() |> nop(), geom_line())
})
