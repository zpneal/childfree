.onAttach <- function(lib,pkg) {
  local_version <- utils::packageVersion("childfree")
  packageStartupMessage("N   ■─┬─o   childfree v",local_version)
  packageStartupMessage(" O    │     CITE: Neal, Z. P. and Neal, J. W., (2024). Childfree: An R package for aggregating")
  packageStartupMessage("  K   ■─┬─o and harmonizing childfree demographic data. GitHub.")
  packageStartupMessage("   I    │   HELP: type vignette(\"childfree\"); email zpneal@msu.edu; github zpneal/childfree")
  packageStartupMessage("    D   X   BETA: type devtools::install_github(\"zpneal/childfree\", ref = \"devel\")")
}
