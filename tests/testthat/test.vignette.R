context('vignette -  code chunks stay runnable against the current package')
testthat::skip_on_cran()
test_that('my-vignette.Rmd tangles and sources without error', {
  skip_on_cran()  # this runs every fitting function; slow, and CRAN's
  # examples/vignette build already exercises the package

  # Locate the vignette source relative to the package root regardless of
  # where tests are invoked from (R CMD check, devtools::test(), etc.).
  vignette_path <- testthat::test_path("..", "..", "vignettes", "my-vignette.Rmd")
  skip_if_not(file.exists(vignette_path), "my-vignette.Rmd not found -- skipping drift check")

  tangled <- tempfile(fileext = ".R")
  on.exit(unlink(tangled))

  # knit::purl() extracts every chunk's code regardless of eval = FALSE --
  # only purl = FALSE excludes a chunk. The vignette's illustrative
  # binary/count section is marked purl = FALSE for exactly this reason;
  # if that marking is ever removed, this test will fail on the
  # placeholder "some_binary_outcome" column instead of on a real
  # signature drift, which is a signal to re-check that chunk rather than
  # a bug in this test.
  suppressMessages(
    knitr::purl(input = vignette_path, output = tangled, documentation = 0, quiet = TRUE)
  )

  # Source into a fresh environment, on a null graphics device, so this
  # doesn't pop up plot windows or leak objects into the test environment.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(source(tangled, local = new.env()), NA)
})
