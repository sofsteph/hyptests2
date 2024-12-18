# Creating vignette

install.packages('usethis')
require(usethis)
use_vignette("hyptests2")

devtools::build_vignettes()

# test vignette
browseVignettes(package = "hyptests2")

