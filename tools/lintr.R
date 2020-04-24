#!/usr/bin/env Rscript
wd <- getwd()

lintr::lint(file.path(wd, "ui.R"))
lintr::lint(file.path(wd, "server.R"))
lintr::lint(file.path(wd, "functions", "display_by_mode.R"))
lintr::lint(file.path(wd, "functions", "calculator_staff_needs.R"))
