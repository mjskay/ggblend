# based on code from expect_snapshot_file() example
save_png = function(fig, width = 400, height = 400) {
  path = tempfile(fileext = ".png")
  png(path, width = width, height = height, type = "cairo")
  on.exit(dev.off())

  print(fig)

  path
}

expect_snapshot_plot = function(name, fig) {
  os = tolower(Sys.info()[["sysname"]])
  supported_oses = c("windows", "linux")

  name_for_os = function(os) {
    paste0(gsub("\\.", "_", make.names(name)), "-", os, ".png")
  }

  # Announce the files for all platform before skipping and before touching `fig`.
  # This way, if `fig` unexpectedly fails or skips (or is not run on this platform),
  # testthat will not auto-delete the corresponding snapshot file.
  oses = c(setdiff(supported_oses, os), os) # current os must be announced last
  for (os in oses) {
    announce_snapshot_file(name = name_for_os(os))
  }

  skip_on_cran()
  skip_if_not(os %in% supported_oses)

  path = save_png(fig + theme_test())
  name = name_for_os(os)
  expect_snapshot_file(path, name)
}
