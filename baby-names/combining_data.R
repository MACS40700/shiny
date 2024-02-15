# see why RDS here: https://bookdown.org/csgillespie/efficientR/input-output.html
# this code from: https://github.com/ronammar/baby_names_shiny_appy/blob/master/merge_ssa_files.R


##### The following section improves reproducibility when scripting ------------

# Clear the current session, to avoid errors from persisting data structures
rm(list=ls())

# Free up memory by forcing garbage collection
invisible(gc())

# Manually set the seed to an arbitrary number for consistency in reports
set.seed(1234)

# Do not convert character vectors to factors unless explicitly indicated
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------

library(stringr)

babyNames <- data.frame()
here::here()

for (y in 1880:2015) {
  yearNames <- read.csv(file=paste0("baby-names/data/names/yob", y, ".txt"), header=FALSE)
  yearNames$year <- rep(y, nrow(yearNames))
  babyNames <- rbind(babyNames, yearNames)
}

colnames(babyNames) <- c("name", "sex", "count", "year")

babyNames$name <- str_to_lower(babyNames$name)

saveRDS(babyNames, "baby-names/data/baby_names.rds")
