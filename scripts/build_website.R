on_ci <- isTRUE(as.logical(Sys.getenv("CI")))

pkgload::load_all()

stop_if_makeinfo_not_installed()

##### Process Manuals ----------------
#
# This downloads source files and creates the intermediate structure
# and files to build the books
#
# folder structure:
#
# - manuals
#   - r-exts
#     - data
#     - prep
#     - books
#     - docs
#   - r-intro
#     - data
#     - prep
#     - books
#     - docs
#   - etc.
#
# - books
#   - r-intro
#   - r-data
#   - r-admin
#   - r-exts
#   - r-lang
#   - r-ints
#

# Determine the correct sequence for navbar links, etc.

all_manuals <- c(
  "r-intro",
  "r-data",
  "r-admin",
  "r-exts",
  "r-lang",
  "r-ints",
  NULL
)

# Determine which manuals to build.
# Comment out some of these lines for quick testing.
# For production, should be identical to all_manuals.
manuals <- c(
  "R-intro.texi",
  "R-data.texi",
  "R-admin.texi",
  "R-exts.texi",
  "R-lang.texi",
  "R-ints.texi",
  NULL
)

purrr::walk(manuals, process_manual, .quicktest = FALSE)

# Build books

build_books(manuals = manuals, all_manuals = all_manuals)

# Build website --------------
#
# Builds the main website with a shared navbar with all the books
# Final output is in `website/_site`

build_main_website(all_manuals = all_manuals)

# if (!on_ci) servr::httd("website/_site")
