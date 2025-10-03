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
  # Announce the files for before skipping and before touching `fig`.
  # This way, if `fig` unexpectedly fails or skips (or is not run on this platform),
  # testthat will not auto-delete the corresponding snapshot file.
  file_name = paste0(gsub("\\.", "_", make.names(title)), ".png")
  announce_snapshot_file(file_name)

  skip_on_cran()
  skip_on_os("mac")  # these tests only work on windows and linux
  skip_if_not_installed("fontquiver")
  skip_if_not_installed("sysfonts")
  skip_if_not_installed("showtext")

  # void theme + no panel labels to reduce the amount of potential text /
  # elements which may be more likely to vary by OS
  path = save_png(
    fig + 
      theme_void() + 
      theme(panel.border = element_rect(fill = NA, color = NA), strip.text = element_blank())
  )
  expect_snapshot_file(path, file_name)
}
