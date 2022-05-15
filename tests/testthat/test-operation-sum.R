
# basic sums --------------------------------------------------------------

test_that("basic sums work", {
  expect_equal(
    adjust(color = "red") + 1 + adjust(size = 2) + 0,
    new_operation_sum(list(adjust(color = "red"), nop(), adjust(size = 2)))
  )
})


# multiplication of sums --------------------------------------------------------------

test_that("multiplication of sums works", {
  expect_equal(
    (adjust(color = "red") + 1 + 1) * (adjust(size = 2) + 1),
    new_operation_sum(list(
      adjust(color = "red", size = 2), adjust(color = "red"),
      adjust(size = 2), nop(),
      adjust(size = 2), nop()
    ))
  )
})


# operation application ---------------------------------------------------

test_that("application of sums preserves structure of input", {
  input = list(
    geom_line(),
    geom_point(),
    list(
      geom_bar(),
      geom_col()
    ),
    list(
      geom_histogram()
    )
  )

  ref = layer_list(
    list(geom_line(color = "red"), geom_line(size = 2)),
    list(geom_point(color = "red"), geom_point(size = 2)),
    list(
      list(geom_bar(color = "red"), geom_bar(size = 2)),
      list(geom_col(color = "red"), geom_col(size = 2))
    ),
    list(
      list(geom_histogram(color = "red"), geom_histogram(size = 2))
    )
  )

  expect_equal_layer(
    input * (adjust(color = "red") + adjust(size = 2)),
    ref
  )

  # should be the same if we pre-calculate the operation
  op = force((adjust(color = "red") + adjust(size = 2)))
  expect_equal_layer(
    input * op,
    ref
  )
})

