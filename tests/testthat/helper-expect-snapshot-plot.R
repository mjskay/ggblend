save_png = function(fig, width = 500, height = 500) {
  path = tempfile(fileext = ".png")

  # use Liberation Sans and Symbola to avoid platform-specific font differences
  liberation_sans = fontquiver::font_styles("Liberation", "Sans")
  symbola = fontquiver::font("Symbola", "Symbols", "Regular")
  sysfonts::font_add(
    "Liberation Sans",
    regular = liberation_sans$Regular$ttf,
    bold = liberation_sans$Bold$ttf,
    italic = liberation_sans$Italic$ttf,
    bolditalic = liberation_sans$`Bold Italic`$ttf,
    symbol = symbola$ttf
  )

  png(path, width = width, height = height, type = "cairo")
  showtext::showtext_begin()
  on.exit({
    showtext::showtext_end()
    grDevices::dev.off()
  })

  print(fig)

  path
}

expect_snapshot_plot = function(title, fig) {
  # os = tolower(Sys.info()[["sysname"]])
  # supported_oses = c("windows", "linux")
  # non_ci_oses = "linux"

  # name_for_os = function(os) {
    # paste0(gsub("\\.", "_", make.names(name)), "-", os, ".png")
  # }

  # Announce the files for all platform before skipping and before touching `fig`.
  # This way, if `fig` unexpectedly fails or skips (or is not run on this platform),
  # testthat will not auto-delete the corresponding snapshot file.
  # oses = c(setdiff(supported_oses, os), os) # current os must be announced last
  # for (os in oses) {
    # announce_snapshot_file(name = name_for_os(os))
  # }
  file_name = paste0(gsub("\\.", "_", make.names(title)), ".png")
  announce_snapshot_file(file_name)

  skip_on_cran()
  skip_if_not_installed("fontquiver")
  skip_if_not_installed("sysfonts")
  skip_if_not_installed("showtext")
  # skip_if_not(os %in% supported_oses)
  # if (os %in% non_ci_oses) skip_on_ci()

  path = save_png(fig + theme_void() + theme(panel.border = element_rect(fill = NA), strip.text = element_blank()))
  # title = name_for_os(os)
  expect_snapshot_file(path, file_name)
}
