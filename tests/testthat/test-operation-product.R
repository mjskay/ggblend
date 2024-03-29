
# basic products --------------------------------------------------------------

test_that("basic products work", {
  expect_equal(
    adjust(color = "red") * blend(),
    new_operation_product(list(adjust(color = "red"), blend()))
  )
})

test_that("prod() works", {
  expect_equal(prod(adjust(), blend(), adjust(color = "red")), adjust() * blend() * adjust(color = "red"))
})

test_that("products of operations are applied correctly", {
  expect_equal(
    geom_point() * (adjust(aes(color = "red")) * blend("multiply")),
    geom_point() |> adjust(aes(color = "red")) |> blend("multiply")
  )
  expect_equal(
    geom_point() * adjust(aes(color = "red")) * blend("multiply"),
    geom_point() |> adjust(aes(color = "red")) |> blend("multiply")
  )
})


# casting -----------------------------------------------------------------

test_that("converting a list to an operation product works", {
  expect_equal(as(list(adjust(), blend()), "operation_product"), adjust() * blend())
})


# printing ----------------------------------------------------------------

test_that("format works", {
  expect_equal(format(new_operation_product(list())), "0")
  expect_equal(format(new_operation_product(list(adjust()))), "adjust()")
  expect_equal(format(new_operation_product(list(adjust(), blend()))), "adjust() * blend()")
})
