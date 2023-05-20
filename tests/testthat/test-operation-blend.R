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
