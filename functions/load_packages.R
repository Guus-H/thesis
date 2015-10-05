# ==== Global Variables ====
project <<- 'smart-bridge-91709' # Google bigquery poject name
memory.limit(size = 30000) # Increase memory size

# ==== Packages ====
require("bigrquery")
require("ggplot2")
require("data.table")
require("Hmisc")
require("notifyR")
require("pbapply")
require(sqldf)
require(devtools)
source_gist(4676064)
require(reshape2)
require(reshape)
require(plyr)
require(MASS)
require(scales)
require(ggthemes)
require(igraph)

