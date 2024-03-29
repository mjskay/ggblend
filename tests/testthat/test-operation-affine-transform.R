test_that("transforming works", {
  expect_snapshot_plot("affine transform",
    data.frame(x = 1, y = 0.5) |>
      ggplot(aes(x, y)) +
      geom_point(size = 10, shape = 15, alpha = 0.5) +
      geom_point(size = 10, shape = 15, color = "red", alpha = 0.5) |>
        affine_transform(x = unit(10, "pt"), y = unit(20, "pt"), width = 0.5, height = 2, angle = 15) +
      guides(color = "none", shape = "none")
  )
})

test_that("transforming two layers", {
  expect_snapshot_plot("affine transform on two layers",
    data.frame(x = 1, y = 0.25) |>
      ggplot(aes(x, y)) +
      geom_point(size = 10, shape = 15, alpha = 0.5) +
      geom_point(aes(y = 0.75), size = 10, shape = 15, alpha = 0.5) +
      list(
        geom_point(size = 10, shape = 15, color = "red", alpha = 0.5),
        geom_point(aes(y = 0.75), size = 10, color = "red", shape = 15, alpha = 0.5)
      )|>
      affine_transform(x = unit(10, "pt"), y = unit(-10, "pt"), width = 1, height = 1, angle = 0) +
      guides(color = "none", shape = "none") +
      scale_y_continuous(limits = c(0, 1))
  )
})


# argument checks ---------------------------------------------------------

test_that("unit arguments are checked", {
  expect_error(affine_transform(x = "a"), r"(must\s+be\s+a\s+numeric\s+or\s+a\s+grid::unit)")
  expect_error(affine_transform(y = "a"), r"(must\s+be\s+a\s+numeric\s+or\s+a\s+grid::unit)")
})


# empty grobs -------------------------------------------------------------

test_that("transforming an empty grob works", {
  without_warnings({
    p = ggplot() + geom_blank() |> affine_transform(x = 1, y = 0, width = 0.5, height = 2, angle = 10)

    zg = defineGrob(zeroGrob())
    ref = list(grobTree(
      zg,
      grobTree(
        useGrob(zg$name),
        vp = viewport(
          x = unit(1.5, "npc"), y = unit(0.5, "npc"),
          width = 0.5, height = 2, angle = 10
        )
      )
    ))
    expect_equal_grob(layer_grob(p, 1), ref)
  })
})


# printing ----------------------------------------------------------------

test_that("format works", {
  expect_equal(format(affine_transform()), "affine_transform()")
  expect_equal(format(affine_transform(x = 0)), "affine_transform()")
  expect_equal(format(affine_transform(x = 1)), 'affine_transform(x = 1npc)')
  expect_equal(format(affine_transform(x = 1, y = unit(2, "pt"))), 'affine_transform(x = 1npc, y = 2points)')
})


# affine transform capabilities warning -----------------------------------

test_that("affine transform warning works", {
  with_old_graphics_device({
    expect_warning(layer_grob(ggplot() + geom_blank() |> affine_transform()),
      r"(Your\s+graphics\s+device.+reports\s+that\s+affine\s+transformations\s+are\s+not\s+supported)"
    )
  })
})
