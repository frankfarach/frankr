#' Initialize a raw data folder and R scripts for setting up a package and munging the raw data.
#'
#' @param name String naming an R script for package initialization
#' @param data_title String naming an R script for loading raw data
#' @return A \code{data-raw} folder and two files with names based on the arguments supplied to the function. The data loading script will be opened for editing.
#' @examples
#' \dontrun{
#' init_data_raw()
#' }
#' @export
init_data_raw <-
  function(name = "pkg-devops", data_title = "01-load") {
    attempt::stop_if_any(list(name, data_title),
                purrr::negate(is.character),
                "Please use a character vector")

    name <- glue::glue("data-raw/{name}.R")

    usethis::use_data_raw(name = data_title, open = FALSE)
    file.create(name)
    utils::file.edit(name)
}


#' Initialize documentation files for an R package, including tests, license, news, and roxygen markdown.
#'
#' @param auth_name Character string naming the package author
#' @param testthat Logical flag indicating whether to include testing
#' @param license Logical flag indicating whether to include a \code{LICENSE} file
#' @param news Logical flag indicating whether to include \code{NEWS.md}
#' @param roxygen_md Logical flag indicating whether to use the \code{roxygen_md} package
#' @return The return values depend on the flags that are TRUE.
#' @examples
#' \dontrun{
#' init_docs()
#' }
#' @export
init_docs <- function(auth_name = "Frank Farach",
                      testthat = TRUE,
                      license = TRUE,
                      news = TRUE,
                      roxygen_md = TRUE) {
  attempt::stop_if_not(auth_name, is.character, "Please use a character vector")
  attempt::stop_if_any(
    list(testthat, license, news, roxygen_md),
    purrr::negate(is.logical),
    "All arguments must have a logical value"
  )

  usethis::use_readme_rmd(open = FALSE)

  if (license) {
    usethis::use_mit_license(name = auth_name)
  }
  if (testthat) {
    usethis::use_testthat()
  }
  if (roxygen_md) {
    usethis::use_roxygen_md()
  }
  if (news) {
    usethis::use_news_md(open = FALSE)
  }
}



#' Initialize DESCRIPTION.
#'
#' @param pkgname Name of the package
#' @param email Email address for the package author
#' @param title Package title
#' @param description Description of the R package.
#' @return DESCRIPTION file populated with default and user-supplied values.
#' @examples
#' \dontrun{
#' fill_desc("mypackage", "example@example.com", "An example R package",
#' "This is an example R package.")
#' }
#' @export
fill_desc <- function(pkgname, email, title, description) {
  attempt::stop_if_any(
    list(pkgname, email, title, description),
    purrr::negate(is.character),
    "Please use a character vector"
  )

  unlink("DESCRIPTION")
  my_desc <- desc::description$new("!new")
  my_desc$set("Package", pkgname)
  my_desc$set(
    "Authors@R",
    glue::glue(
      "person('Frank', 'Farach',
              email = '{email}', role = c('cre', 'aut'))"
    )
  )
  my_desc$del("Maintainer")
  my_desc$set_version("0.0.0.9000")
  my_desc$set("Title", title)
  my_desc$set("Description", description)
  my_desc$del("URL")
  my_desc$del("BugReports")
  my_desc$write(file = "DESCRIPTION")
}


# Script ------------------------------------------------------------------

# Order is important:
# init_raw_data()
# fill_desc()
# init_docs()
