rlang::on_load(rlang::local_use_cli(inline = TRUE))

.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
}
