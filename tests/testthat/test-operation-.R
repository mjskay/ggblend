

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


# casting -----------------------------------------------------------------

test_that("converting a list to an operation works", {
  expect_equal(as(list(1, adjust(), adjust() + blend()), "operation"), nop() + adjust() + adjust() + blend())
})


# non-layers throw errors -----------------------------------------------

test_that("multiplying by a non-layer throws an error", {
  expect_error("a" * adjust(), r'(Cannot\s+convert\s+object.*to\s+a\s+layer-like)')
})


# printing ----------------------------------------------------------------

test_that("print works", {
  expect_output(print(blend()), "<operation>: blend()")
})


# operation construction --------------------------------------------------

test_that("operation construction and printing works", {
  setClass("test_operation", slots = list(x = "ANY", y = "ANY"), contains = "operation")
  new_test_operation = function(x = 0, y = 0) {
    new("test_operation", x = x, y = y)
  }
  test_operation = make_operation("test_operation", new_test_operation, x)
  expect_equal(format(test_operation()), "test_operation(x = 0, y = 0)")
  expect_equal(format(test_operation(x = 3, y = 2)), "test_operation(x = 3, y = 2)")
  expect_equal(format(adjust() |> test_operation()), "adjust() |> test_operation(x = 0, y = 0)")
  expect_error(test_operation(0, x = 0), r'(Cannot\s+provide\s+both.*arguments)')
  expect_error(geom_blank() |> test_operation(), "Unimplemented layer operation")
  expect_error(geom_blank() * test_operation(), "Unimplemented layer operation")
})
