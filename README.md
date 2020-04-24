[![COVID Staffing Project](https://koroid.com/wp-content/uploads/2020/04/logo-covid-staffing-project-@2x.png)](https://www.covidstaffing.org)

# Staff Demand Calculator

The COVID staffing demand calculator can be use to determine your hospital’s immediate staffing needs, based on your current inpatient census. Please visit [covidstaffing.org](https://www.covidstaffing.org/) for more information on our staffing tools.

## Table of Contents

- [Getting Started](#getting-started)
- [Prerequisites](#prerequisites)
- [Development Environment](#development-environment)
- [Running](#running)
    - [Using RStudio](#using-rstudio)
    - [Using shinyServer](#using-shinyserver)
- [File Structure](#file-structure)
- [Further Information](#further-information)
- [License](#license)

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for **development** and **testing** purposes. See **contributing** for more information on how you can help us build the COVID staffing demand calculator. See **deployment** for notes on how to deploy the project on a live system.

### Prerequisites

This application is written in [R](https://www.r-project.org/) and uses [Shiny](https://github.com/rstudio/shiny) to build and deploy the web application.  We assume you have basic knowledge of both.

## Development Environment

This should detail how to setup a local environment that enables developers to keep all packages version segregated from their local machine. This will make it easy to setup and avoid conflicts due to dependency version miss-matches between development environments.   

### Running

The following packages should be added to an install script, along with their intended version, and references from there.

Example
```
install.packages(c("package1", "package2"))
```

Imports
- shiny
- tidyverse
- highcharter
- rhandsontable
- shinyjs
- plotly
- shinyWidgets

Depends
- DT
- gsheet
- openxlsx
- forcats
- stringr
- dplyr
- purrr
- readr
- tidyr
- tibble
- ggplot2
- stats
- graphics
- grDevices
- utils
- datasets
- methods
- base

#### Using RStudio
To run the application locally, you can install the packages above in RStudio, and use the function `runApp()` to start.

Note: You may need to install **devtools** and **tidyext** from the console manually:
```
install.package("devtools")
devtools::install_github('m-clark/tidyext')
```

#### Using shinyServer
Instructions on how to run the application on a shiny server.

## File Structure

ShinyApp searches for the following in the root folder:
- server.R
    - Contains shinyServer function for server side functions
    - Renders once per user
- ui.R
    - Contains shinyUI function for front end rendering 
    - Renders once per R session
- Covid_staffing_calculator.Rproj
    - What is this for?
- Staff_projections.R
    - What is this for?
- formula_for_cal.R
    - What is this for?
- functions/
    - What is the main idea for this folder?
- data/
    - What is the main idea for this folder?

## Further Information

The COVID-19 pandemic poses an unprecedented challenge to health care systems and their staff
In response, our [multi-institutional collaborative](https://www.covidstaffing.org/) is developing tools to support your hospital in:

- 1) Projecting your frontline workforce needs;
- 2) Redeploying your clinical teams; and
- 3) Protecting the health and well-being of your providers.

We are software developers, administrators, engineers, big data analysts, social scientists, and clinicians dedicated to supporting hospitals and front line providers during the COVID-19 crisis.

Our goal is not to duplicate work that others have already begun, but rather to combine our work with that of others. We welcome your feedback and suggestions as we navigate this difficult time together.

If you are also working in this domain and interested in collaborating, please [contact us](mailto:info@covidstaffing.org).

## License

This project is licensed under the MIT license.
