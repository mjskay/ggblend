# based on code from expect_snapshot_file() example
save_png = function(fig, width = 400, height = 400) {
  path = tempfile(fileext = ".png")
  png(path, width = width, height = height, type = "cairo")
  on.exit(dev.off())

  print(fig)

  path
}

expect_snapshot_plot = function(name, fig) {
  skip_on_cran()

  name = paste0(gsub("\\.", "_", make.names(name)), ".png")

  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  path = save_png(fig)
  expect_snapshot_file(path, name)
}
