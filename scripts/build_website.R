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

# Which version of R to build the manuals for.
# Set the R_MANUALS_REF environment variable to override the default:
#   - unset / ""      -> latest released version of R (default)
#   - "trunk"         -> R-devel ("Under development")
#   - "R-4-6-branch"  -> latest patched 4.6.x
#   - "tags/R-4-6-1"  -> exact 4.6.1 release
r_ref <- Sys.getenv("R_MANUALS_REF", unset = "")
if (!nzchar(r_ref)) r_ref <- latest_r_release_ref()
cli::cli_alert_info("Building manuals for R ref: {.val {r_ref}}")

purrr::walk(manuals, process_manual, .quicktest = FALSE, r_ref = r_ref)

# Build books

build_books(manuals = manuals, all_manuals = all_manuals)

# Build website --------------
#
# Builds the main website with a shared navbar with all the books
# Final output is in `website/_site`

build_main_website(all_manuals = all_manuals)

# if (!on_ci) servr::httd("website/_site")
