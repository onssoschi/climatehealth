Contribution guidelines
=======================

`climatehealth` is an R package maintained and developed by the ONS Climate and Health Team. We welcome contributions from our project partners. 

## General guidance

> **NOTE: Do not upload any data to this repository, only code.**
* `main` is the production branch and is protected. 
* `dev` is the development branch and is the default branch.
* Create a new branch from `dev` for any change you want to make, and commit your work on that branch.
* When changes are ready to be merged into `dev`, please follow the [pull request process](#pull-request-process) below.

## Adding indicator code

Guidance on the process:

* Create a new branch from `dev`, giving the new branch a clear name (e.g. `vector_borne_disease_functions`).
* Add scripts containing R functions for your indicator in the `R` folder.
* Add or update documentation for any new exported functions using roxygen comments.
* Add or update tests in `tests/testthat` for new logic, bug fixes, and end-to-end indicator workflows where practical.
* If you add files under `inst/`, make sure they are necessary package assets and not local machine output.
* When developing R functions in these scripts:
    * Use logical atomic commits. Keep commit messages short but informative (e.g. "add plot function").
    * Ensure any new functions have unit tests and are properly documented (with roxygen).
* Before opening a pull request, run the relevant local checks where possible:
    * `devtools::document()`
    * `testthat::test_dir("tests/testthat")` or the relevant subset
    * `devtools::check()`

## Pull request process

**Pull requests must be reviewed before feature branches can be merged into the `dev` branch.**

Once changes are ready to be merged into `dev`, please follow these steps:

1. Open a pull request targeting the `dev` branch.
2. Give a succinct, clear summary of the changes made.
3. Link any relevant issues, checks, or background context.
4. Resolve any merge conflicts and keep the branch up to date with `dev` while the pull request is open.
5. Respond to review comments and push follow-up commits as needed.
6. Once the pull request is approved, it can be merged into `dev`.

The Climate and Health Team reviews pull requests as soon as capacity allows.

## Request a feature or fix

To request a feature or bug fix to be implemented by the Climate and Health Team, please create an issue clearly describing the feature or fix.

* Be clear whether the issue is a request for a new feature or a bug fix.
* **If you find a security vulnerability, do NOT open an issue. Email climate.health@ons.gov.uk instead.**

## Current maintainers

Current maintainer:

* Kenechi Omeke (@kenechiomeke-ONS on GitHub)

## Code style

Please follow the coding style for this project. 
* Use tidyverse packages where possible.
* Employ coding best practice to ensure code is clean and readable:
    * [Tidyverse style guide for R](https://style.tidyverse.org/index.html)
    * [Quality assurance of code for analysis and research](https://best-practice-and-impact.github.io/qa-of-code-guidance/readable_code.html)
* In general, we do not load whole packages using the library() function. We reference the namespace directly e.g. `dplyr::mutate()`.
* Keep each line of code under 80 characters.
* Use comments sparingly (e.g. to explain *why* the code does what it does, not *what* the code does).
