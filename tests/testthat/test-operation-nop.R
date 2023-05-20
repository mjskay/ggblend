test_that("nop works", {
  expect_equal(nop() * nop(), nop())
  expect_equal(nop() * geom_line(), geom_line())
  expect_equal(geom_line() * nop(), geom_line())
  expect_equal(geom_line() |> nop(), geom_line())
  expect_equal(nop() * (nop() + nop()), nop() + nop())
  expect_equal(nop() * 2, nop() + nop())
  expect_equal((adjust() + blend()) * nop(), adjust() + blend())
})

# printing ----------------------------------------------------------------

test_that("format works", {
  expect_equal(format(nop()), "1")
})
