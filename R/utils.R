.onAttach <- function(lib,pkg) {
  local_version <- utils::packageVersion("childfree")
  packageStartupMessage("N   ■─┬─o   childfree v",local_version)
  packageStartupMessage(" O    │     Cite: Neal, Z. P. and Neal, J. W., (2024). Childfree: An R package to generate and harmonize")
  packageStartupMessage("  K   ■─┬─o demographic data to study childfree individuals.")
  packageStartupMessage("   I    │   Help: type vignette(\"childfree\"); email zpneal@msu.edu; github zpneal/childfree")
  packageStartupMessage("    D   X   Beta: type devtools::install_github(\"zpneal/childfree\", ref = \"devel\")")
}
