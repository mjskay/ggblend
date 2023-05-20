# printing ----------------------------------------------------------------

test_that("format works", {
  expect_equal(format(new_operation_composition(adjust(), blend())), "adjust() |> blend()")
})
