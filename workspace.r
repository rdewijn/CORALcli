library(CORALcli)

library(readr)
library(stringr)
df <- read_delim(system.file("extdata", "ptk_stk_combined.txt", package = "CORALcli"), show_col_types = FALSE)

plot_tree(df, "blavsbla", -1.5, 1.5)
