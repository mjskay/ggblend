

# operation algebra works (without layers) --------------------------------

test_that("distributive law works", {
  expect_equal(
    (adjust(color = "red") + 1) * adjust(size = 2),
    new_operation_sum(list(adjust(color = "red", size = 2), adjust(size = 2)))
  )

  expect_equal(
    adjust(size = 2) * (1 + adjust(color = "red")),
    new_operation_sum(list(adjust(size = 2), adjust(size = 2, color = "red")))
  )

  expect_equal(
    (adjust(color = "red") + 1) * (1 + adjust(size = 2)),
    new_operation_sum(list(
      adjust(color = "red"), adjust(color = "red", size = 2),
      nop(), adjust(size = 2)
    ))
  )
})

test_that("addition with 0 works", {
  expect_equal(0 + adjust(size = 2), new_operation_sum(list(adjust(size = 2))))
  expect_equal(adjust(size = 2) + 0, new_operation_sum(list(adjust(size = 2))))
  expect_equal(0 + adjust(size = 2) + 0, new_operation_sum(list(adjust(size = 2))))
})

test_that("multiplication by 0 works", {
  expect_equal(0 * adjust(size = 2), new_operation_sum(list()))
  expect_equal(adjust(size = 2) * 0, new_operation_sum(list()))
})

test_that("multiplication by 1 works", {
  expect_equal(1 * adjust(size = 2), new_operation_sum(list(adjust(size = 2))))
  expect_equal(adjust(size = 2) * 1, new_operation_sum(list(adjust(size = 2))))
})

test_that("multiplication by n > 1 works", {
  expect_equal(adjust(size = 2) * 2, new_operation_sum(list(adjust(size = 2), adjust(size = 2))))
  expect_equal(2 * adjust(size = 2), new_operation_sum(list(adjust(size = 2), adjust(size = 2))))
})

test_that("multiplying by a layer list works", {
  expect_equal(layer_list() * adjust(), layer_list())
})


# non-layers throw errors -----------------------------------------------

test_that("multiplying by a non-layer throws an error", {
  expect_error("a" * adjust(), r'(Cannot\s+convert\s+object.*to\s+a\s+layer-like)')
})

