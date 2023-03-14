# reproducibility and scaling tools ---------------------------------------

# renv: save packages and versions ----------------------------------------

library(renv)
# renv::init()
# Update all the packages
renv::snapshot()
# Install only one package
# renv::install('shinycssloaders')
# Remove packages!
# renv::remove('here')

# dockerfile generator ----------------------------------------------------

# Create dockerfile from renv
library(dockerfiler)
# Create a dockerfile template
my_dock <- dock_from_renv(
  lockfile = 'renv.lock',
  distro = "focal",
  FROM = "rocker/verse"
)
my_dock


