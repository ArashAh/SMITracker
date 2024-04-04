########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "SMITracker", # The Name of the package containing the App
  pkg_title = "Single-molecule Interaction Tracker", # The Title of the package containing the App
  pkg_description = "This package is an interactive analysis platform that enables the users to detect and track
  fluorescently-labeled single molecules scanning a linear substrates. In a typical experiment to collect a few
  number of short-lived interactions, large number of frames are recorded. Finding the spatio-temporal address of
  those events of target is the challenge that this package helps solving. Prior to analysis using this platform,
  all the existing signals in the recorded images need to be localized in 'FIJI' using a single molecule localization
  plug-in called 'ThunderSTORM'.", # The Description of the package containing the App
  author_first_name = "Arash", # Your First Name
  author_last_name = "Ahmadi", # Your Last Name
  author_email = "arash.ahmadi@dscience.uio.no", # Your Email
  repo_url = "https://github.uio.no/dScience/SMITracker.git", # The URL of the GitHub Repo (optional),
)

## Set {golem} options ----
golem::set_golem_options()

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----

usethis::use_mit_license("Arash Ahmadi")
usethis::use_readme_rmd(open = FALSE)
# devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
# usethis::use_code_of_conduct(contact = "Arash Ahmadi")
usethis::use_lifecycle_badge("Experimental")
#usethis::use_news_md(open = FALSE)

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
