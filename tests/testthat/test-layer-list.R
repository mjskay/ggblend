
# casting -----------------------------------------------------------------

test_that("basic casting works", {
  expect_equal(as_layer_list(geom_line()), layer_list(geom_line()))
  expect_equal(as_layer_list(list()), layer_list())
  expect_equal(as_layer_list(layer_list(geom_line())), layer_list(geom_line()))
  expect_equal(layer_list(geom_line()) + layer_list(geom_point()), layer_list(geom_line(), geom_point()))

  expect_error(as_layer_list(list("a")), "All objects in a layer_list must be layer-like")
})


# printing ----------------------------------------------------------------

test_that("basic printing works", {
  expect_output(print(layer_list(geom_blank())),
    paste(
      c("<layer_list>:", capture.output(print(list(geom_blank())))),
    collapse = "\n"),
    fixed = TRUE
  )
})
