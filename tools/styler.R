#!/usr/bin/env Rscript
library("styler")

wd <- getwd()

style_dir(file.path(wd, "functions/"))
style_file(file.path(wd, "ui.R"))
style_file(file.path(wd, "server.R"))
