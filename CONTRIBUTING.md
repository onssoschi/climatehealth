Contribution guidelines
=======================

`climatehealth` is an R package maintained and developed by the ONS Climate and Health Team. We welcome contributions from our project partners. 

## General guidance

* **Do not upload any data to this repository, only code.**
* `main` is the production branch and is protected. 
* `dev` is the development branch and is the default branch. Branch off from `dev` and do changes in the new branch. **Any additions/changes you wish to make to code must be committed on your own branch. When changes are ready to be merged into the `dev` branch (e.g. on completion of a working version of the functions), please follow the [merge request process](#merge-request-process) below.**

## Adding indicator code

Guidance on the process:

* Create a new branch off of `dev`, giving the new branch a clear name (e.g. `vector_borne_disease_functions`). Do changes in this new branch:
    * Use logical atomic commits. Keep commit messages short but informative (e.g. "add plot function").
    * Ensure any new functions have unit tests and are properly documented (with roxygen).
* Add scripts containing R functions for your indicator in the `R` folder.
  
## Merge request process

**Merge requests (also called pull requests) must be reviewed by a maintainer before feature branches can be merged into the `dev` branch**

Once changes are ready to be merged into `dev`, please follow the following steps:

* Create a new merge request into the `dev` branch. 
    * Give a succinct, clear summary of the changes made.
    * Assign a reviewer (see list of maintainers below).
    * Select "Delete source branch when merge request is accepted".
    * Do **not** select "Squash commits when merge request is accepted".
    * Resolve any merge conflicts after creating the request.
* Implement reviewer suggestions. Once the reviewer is happy with the changes, they will accept the merge request and the feature branch will be merged into `dev`.

The Climate and Health Team reviews merge requests on a regular basis.

## Request a feature or fix

To request a feature or bug fix to be implemented by the Climate and Health Team, please create an issue clearly describing the feature or fix.

* Be clear whether the issue is a request for a new feature or a bug fix.
* **If you find a security vulnerability, do NOT open an issue. Email climate.health@ons.gov.uk instead.**

## Current maintainers

Please assign one of the following maintainers to review a new merge request:

* Antony Brown (@antonymbrown on GitHub)
* Charlie Browning (@CBROWN-ONS on GitHub)

## Code style

Please follow the coding style for this project. 
* Use tidyverse packages where possible.
* Employ coding best practice to ensure code is clean and readable:
    * [Tidyverse style guide for R](https://style.tidyverse.org/index.html)
    * [Quality assurance of code for analysis and research](https://best-practice-and-impact.github.io/qa-of-code-guidance/readable_code.html)
* In general, we do not load whole packages using the library() function. We reference the namespace directly e.g. `dplyr::mutate()`.
* Keep each line of code under 80 characters.
* Use comments sparingly (e.g. to explain *why* the code does what it does, not *what* the code does).
