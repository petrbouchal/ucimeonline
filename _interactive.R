# script for quickly loading

source("_packages.R")
source("R/utils.R")

ts <- as.list(tar_manifest(fields = name)[["name"]])
names(ts) <- ts
