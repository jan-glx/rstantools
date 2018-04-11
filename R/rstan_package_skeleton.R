# This file is part of rstantools
# Copyright (C) 2015, 2016, 2017 Trustees of Columbia University
#
# rstantools is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# rstantools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#

#' Create the skeleton of a new \R package with Stan programs
#'
#' @description
#'   \if{html}{\figure{stanlogo.png}{options: width="50px" alt="http://mc-stan.org/about/logo/"}}
#'   The \code{rstan_package_skeleton} function helps get you started developing
#'   \R packages that interface with Stan via the \pkg{rstan} package.
#'   As of \pkg{rstantools} v1.5.0, \code{rstan_package_skeleton}
#'   calls \code{usethis::create_package} (instead of \code{utils::package.skeleton})
#'   and then makes necessary adjustments so that the package can include Stan Programs that can be built into binary versions
#'   (i.e., pre-compiled like \pkg{rstanarm}).
#'
#'   See the \strong{See Also} section below for links to recommendations for
#'   developers and a step by step walkthrough of what to do after running
#'   \code{rstan_package_skeleton}.
#'
#' @export
#' @param path A relative or absolute path to the new package to be created
#'   (terminating in the package name).
#' @param fields,rstudio,open See \code{usethis::create_package}.
#' @param stan_files Optionally, a character vector with paths to \code{.stan}
#'   files to include in the package (these files will be included in the
#'   \code{src/stan_files} directory). If not specified then the \code{.stan}
#'   files for the package can be manually placed into the appropriate directory
#'   later.
#' @param travis Optionally, should a \code{.travis.yml} file be added to the
#'   package directory? The file has some settings already set to help with
#'   compilation issues, but we do not guarantee that it will work on
#'   \href{https://travis-ci.org/}{travis-ci} without manual adjustments.
#'
#' @note This function downloads several files from \pkg{rstanarm} package's
#'   \href{http://github.com/stan-dev/rstanarm}{GitHub repository} to facilitate
#'   building the resulting package. Note that \pkg{\link[rstanarm]{rstanarm}}
#'   is licensed under the GPL >= 3, so package builders who do not want to be
#'   governed by that license should not use the downloaded files that contain
#'   \R code (that said, \pkg{Rcpp} is GPL, so not using the \pkg{rstanarm}
#'   files is not the only thing impeding use of other licenses). Otherwise, it
#'   may be worth considering whether it would be easier to include your
#'   \code{.stan} programs and supporting \R code in the \pkg{rstanarm} package.
#'
#' @seealso
#' \itemize{
#'   \item \href{https://github.com/stan-dev/rstanarm}{The \pkg{rstanarm} repository on GitHub.}
#' }
#' @template seealso-dev-guidelines
#' @template seealso-get-help
#' @template seealso-useR2016-video
#'
#' @importFrom utils download.file packageVersion available.packages
#'
#'
rstan_package_skeleton <-
  function(path,
           fields = getOption("devtools.desc"),
           rstudio = TRUE,
           open = TRUE,
           stan_files = character(),
           travis = FALSE) {

    if (!requireNamespace("usethis", quietly = TRUE)) {
      stop("Please install the 'usethis' package to use this function.")
    }
    if (!requireNamespace("rstudioapi", quietly = TRUE)) {
      stop("Please install the 'rstudioapi' package to use this function.")
    }
    if (!requireNamespace("roxygen2", quietly = TRUE)) {
      stop("Please install the 'roxygen2' package to use this function.")
    }

    rstudio <- rstudio && rstudioapi::isAvailable()
    if (file.exists(path)) {
      stop("Directory ", normalizePath(path), " already exists.")
    }
    name <- basename(path)
    message("Creating package skeleton for package: ", name, domain = NA)

    if (length(stan_files) > 0 && !all(grepl("\\.stan$", stan_files))) {
      stop(
        "All files named in 'stan_files' must end with a ",
        "'.stan' extension."
      )
    }

    message("Running usethis::create_package ...", domain = NA)
    usethis::create_package(
        path = path,
        fields = fields,
        rstudio = rstudio,
        open = FALSE
      )

    DIR <- normalizePath(path)
    if (open && rstudio) {
      on.exit(rstudioapi::openProject(DIR, newSession = TRUE))
    }


    # tools directory
    usethis::use_directory("tools")
    download.file(
      .rstanarm_path("tools/make_cc.R"),
      destfile = file.path(DIR, "tools", "make_cc.R"),
      quiet = TRUE
    )

    # src directory
    usethis::use_directory("src")
    download.file(
      .rstanarm_path("src/Makevars"),
      destfile = file.path(DIR, "src", "Makevars"),
      quiet = TRUE
    )
    download.file(
      .rstanarm_path("src/Makevars.win"),
      destfile = file.path(DIR, "src", "Makevars.win"),
      quiet = TRUE
    )

    # register cpp (src/init.cpp)
    init_cpp(name, path = DIR)

    usethis::use_directory(file.path("src", "stan_files"))
    STAN_FILES <- file.path(DIR, "src", "stan_files")
    file.copy(stan_files, STAN_FILES)

    usethis::use_directory(file.path("src", "stan_files", "chunks"))
    download.file(
      .rstanarm_path("src/stan_files/pre/license.stan"),
      destfile = file.path(STAN_FILES, "chunks", "license.stan"),
      quiet = TRUE
    )
    system2(
      "sed",
      args = paste0(
        "-i.bak 's@rstanarm@", name, "@g' ",
        file.path(STAN_FILES, "chunks", "license.stan")
      ),
      stdout = FALSE,
      stderr = FALSE
    )
    file.remove(file.path(STAN_FILES, "chunks", "license.stan.bak"))


    # inst directory
    usethis::use_directory(file.path("inst", "include"))
    usethis::use_template(
      "meta_header.hpp",
      save_as = file.path("inst", "include", "meta_header.hpp"),
      package = "rstantools"
    )

    # R directory
    download.file(
      .rstanarm_path("R/stanmodels.R"),
      destfile = file.path(DIR, "R", "stanmodels.R"),
      quiet = TRUE
    )
    system2(
      "sed",
      args = paste0("-i.bak 's@rstanarm@", name, "@g' ",
                    file.path(DIR, "R", "stanmodels.R")),
      stdout = FALSE,
      stderr = FALSE
    )
    file.remove(file.path(DIR, "R", "stanmodels.R.bak"))

    usethis::use_template(
      "zzz.R",
      save_as = file.path("R", "zzz.R"),
      package = "rstantools"
    )
    usethis::use_template(
      "main_package_R_file.R",
      save_as = file.path("R", paste0(name, "-package.R")),
      data = list(package = name, rstan_reference = .rstan_reference()),
      package = "rstantools"
    )

    # finish up
    usethis::use_template("Read-and-delete-me", package = "rstantools")
    .update_description_file(DIR)
    if (file.exists(file.path(DIR, "NAMESPACE"))) {
      file.remove(file.path(DIR, "NAMESPACE"))
    }

    if (travis) {
      # (experimental feature)
      # get usethis to spit out message about writing travis file but then
      # replace it with ours
      usethis::use_template(
        "travis.yml",
        save_as = ".travis.yml",
        ignore = TRUE,
        package = "usethis"
      )
      file.remove(file.path(DIR, ".travis.yml"))
      .create_travis_file(DIR)
    }
    suppressMessages(roxygen2::roxygenise(package.dir = DIR, clean = TRUE))

    message("\nFinished skeleton for package: ", name)
    message(
      domain = NA,
      sprintf(
        "Further steps are described in '%s'.",
        file.path(DIR, "Read-and-delete-me")
      )
    )

    invisible(TRUE)
  }



# internal ----------------------------------------------------------------
.rstanarm_path <- function(relative_path) {
  base_url <- "https://raw.githubusercontent.com/stan-dev/rstanarm/master"
  file.path(base_url, relative_path)
}

.update_description_file <- function(dir) {
  available_pkgs <- available.packages(repos = "https://cran.rstudio.com/")[, c("Package", "Version")]
  available_pkgs <- data.frame(available_pkgs, stringsAsFactors = FALSE)
  .pkg_dependency <- function(pkg, last = FALSE) {
    ver <- available_pkgs$Version[available_pkgs$Package == pkg]
    paste0(pkg, " (>= ", ver, ")", if (!last) ", ")
  }

  cat(
    paste0("Depends: R (>= 3.4.0), ",
           .pkg_dependency("Rcpp"),
           "methods"),
    paste0("Imports: ",
           .pkg_dependency("rstan"),
           .pkg_dependency("rstantools", last=TRUE)),
    paste0("LinkingTo: ",
           .pkg_dependency("StanHeaders"),
           .pkg_dependency("rstan"),
           .pkg_dependency("BH"),
           .pkg_dependency("Rcpp"),
           .pkg_dependency("RcppEigen", last=TRUE)),
    "SystemRequirements: GNU make",
    "NeedsCompilation: yes",
    file = file.path(dir, "DESCRIPTION"),
    sep = "\n",
    append = TRUE
  )

  DES <- readLines(file.path(dir, "DESCRIPTION"))
  DES[grep("^License:", DES)] <- "License: GPL (>=3)"
  cat(
    DES,
    file = file.path(dir, "DESCRIPTION"),
    sep = "\n",
    append = FALSE
  )
}

.rstan_reference <- function() {
  has_version <- utils::packageDescription("rstan", fields = "Version")
  version_year <- substr(utils::packageDescription("rstan", fields = "Date"), 1, 4)
  paste0(
    "Stan Development Team (", version_year,"). ",
    "RStan: the R interface to Stan. ",
    "R package version ", has_version, ". ",
    "http://mc-stan.org"
  )
}

.create_travis_file <- function(dir) {
  pkgname <- basename(dir)
  download.file(
    .rstanarm_path(".travis.yml"),
    destfile = file.path(dir, ".travis.yml"),
    quiet = TRUE
  )

  travis <- readLines(file.path(dir, ".travis.yml"))
  travis <- travis[!grepl("covr::codecov|/covr|r_github_packages", travis)]
  travis <- travis[!grepl("r_build_args|r_check_args", travis)]
  cat(
    gsub("rstanarm", pkgname, travis),
    file = file.path(dir, ".travis.yml"),
    sep = "\n",
    append = FALSE
  )
  cat(
    "^\\.travis\\.yml$",
    file = file.path(dir, ".Rbuildignore"),
    sep = "\n",
    append = TRUE
  )
}
