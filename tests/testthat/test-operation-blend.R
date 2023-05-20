test_that("basic blending works", {
  expect_snapshot_plot("multiply blend",
    data.frame(x = c(1,2,2,3), g = c("a", "a", "b", "b")) |>
      ggplot(aes(x, x, color = g, shape = g)) +
      geom_point(size = 10) * blend("multiply") +
      guides(color = "none", shape = "none")
  )
})

test_that("blending without partition works", {
  expect_snapshot_plot("multiply blend without partition",
    data.frame(x = c(1,1.98,2,2.02,2.5,2.52,3), g = c("a", "a", "b", "b", "b", "b", "b")) |>
      ggplot(aes(x, x, color = g, shape = g)) +
      geom_point(size = 10) |> blend("multiply")+
      guides(color = "none", shape = "none")
  )
})

test_that("blending with partition works", {
  expect_snapshot_plot("multiply blend with partition",
    data.frame(x = c(1,1.98,2,2.02,2.5,2.52,3), g = c("a", "a", "b", "b", "b", "b", "b")) |>
      ggplot(aes(x, x, color = g, shape = g)) +
      geom_point(size = 10) |> partition(vars(g)) |> blend("multiply") +
      guides(color = "none", shape = "none")
  )
})

test_that("complex sequence of blends works", {
  set.seed(1234)
  df_a = data.frame(x = rnorm(100, 0), y = rnorm(100, 1), set = "a")
  df_b = data.frame(x = rnorm(100, 1), y = rnorm(100, 2), set = "b")

  df_ab = rbind(df_a, df_b) |>
    transform(order = "draw a then b")

  df_ba = rbind(df_b, df_a) |>
    transform(order = "draw b then a")

  df = rbind(df_ab, df_ba)

  expect_snapshot_plot("complex blend sequence",
    df |>
      ggplot(aes(x, y, color = set)) +
      list(
        # double blend here since it may not always work
        geom_point(size = 6) * (blend("lighten") + blend("multiply", alpha = 0.65)) |> blend() * blend(),
        geom_vline(xintercept = 0, color = "gray75", linewidth = 1.5),
        geom_hline(yintercept = 0, color = "gray75", linewidth = 1.5)
      ) |> blend("hard.light") +
      scale_color_brewer(palette = "Set2") +
      guides(color = "none", shape = "none") +
      facet_grid(~ order)
  )
})


# empty grobs -------------------------------------------------------------

test_that("blending an empty grob works", {
  without_warnings({
    p = ggplot() + geom_blank() |> blend("multiply")

    expect_equal_grob(layer_grob(p, 1), list(groupGrob(zeroGrob(), "multiply")))
  })
})

test_that("blending a list of empty grobs works", {
  without_warnings({
    p = ggplot() + list(geom_blank(), geom_blank()) |> blend("multiply")

    expect_equal_grob(layer_grob(p, 1), list(zeroGrob()))
    expect_equal_grob(layer_grob(p, 2), list(zeroGrob()))

    expect_equal_grob(layer_grob(p, 3),
      list(groupGrob(
        grobTree(
          groupGrob(zeroGrob()),
          groupGrob(zeroGrob())
        ),
        "multiply"
      ))
    )
  })
})


# printing ----------------------------------------------------------------

test_that("format works", {
  expect_equal(format(blend()), "blend()")
  expect_equal(format(blend("over")), "blend()")
  expect_equal(format(blend("multiply")), 'blend("multiply")')
  expect_equal(format(blend(alpha = 0.1)), 'blend(alpha = 0.1)')
  expect_equal(format(blend("multiply", alpha = 0.1)), 'blend("multiply", alpha = 0.1)')
})


# blend capabilities warning ----------------------------------------------

test_that("blend warning works", {
  with_old_graphics_device({
    expect_warning(layer_grob(ggplot() + geom_blank() |> blend("multiply")),
      r"(Your\s+graphics\s+device.+reports\s+that\s+blend.+is\s+not\s+supported)"
    )
  })
})
