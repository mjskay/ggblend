
# basic sums --------------------------------------------------------------

test_that("basic sums work", {
  expect_equal(
    adjust(color = "red") + 1 + adjust(size = 2) + 0,
    new_operation_sum(list(adjust(color = "red"), nop(), adjust(size = 2)))
  )
})

test_that("sum() works", {
  expect_equal(sum(nop(), adjust(), blend()), nop() + adjust() + blend())
})


# casting -----------------------------------------------------------------

test_that("converting a list to an operation sum works", {
  expect_equal(as(list(1, adjust(), adjust() + blend()), "operation_sum"), nop() + adjust() + adjust() + blend())
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
    geom_path(),
    list(
      geom_bar(),
      geom_col()
    ),
    list(
      geom_histogram()
    )
  )

  ref = layer_list(
    list(geom_line(color = "red"), geom_line(linewidth = 2)),
    list(geom_path(color = "red"), geom_path(linewidth = 2)),
    list(
      list(geom_bar(color = "red"), geom_bar(linewidth = 2)),
      list(geom_col(color = "red"), geom_col(linewidth = 2))
    ),
    list(
      list(geom_histogram(color = "red"), geom_histogram(linewidth = 2))
    )
  )

  expect_equal_layer(
    input * (adjust(color = "red") + adjust(linewidth = 2)),
    ref
  )
})


# printing ----------------------------------------------------------------

test_that("format works", {
  expect_equal(format(new_operation_sum(list())), "0")
  expect_equal(format(new_operation_sum(list(adjust()))), "adjust()")
  expect_equal(format(new_operation_sum(list(adjust(), nop()))), "(adjust() + 1)")
})
