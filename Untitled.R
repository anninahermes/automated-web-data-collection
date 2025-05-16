library(tidygeocoder)
library(tidyverse)

geocode(.tbl = tibble(addr = c("800 Peachtree Street NE, Atlanta, Georgia")), address = addr, method = "census", verbose = TRUE)
