Contribution guidelines
=======================

`climatehealth` is maintained and developed by the ONS Climate and Health Team. Currently contributions can only be made internally.

We intend to open-source this project in future, and will welcome contributions from external users and developers. 

## General guidance

* Do not upload any sensitive data to this repository.
* `main` is the production branch. This branch is protected. The `dev` branch is merged into `main` periodically.
* `dev` is the development branch. This branch is the default branch. Branch from `dev` to develop new features. 

## Request a feature or fix

To request a feature or bug fix to be implemented by the Climate and Health Team, please create an issue clearly describing the feature or fix.

* Be clear whether the issue is a request for a new feature or a bug fix.
* **If you find a security vulnerability, do NOT open an issue. Email climate.health@ons.gov.uk instead.**
* Assign a current maintainer as the `Assignee` (see list of current maintainers below).

## Merge request process

To implement a new feature or fix a bug, please follow the process outlined below.

> 1. Create an issue clearly describing the feature or fix.
> 2. Create a feature branch from `dev` and do changes in that branch. 
        * Use logical atomic commits. Keep commit messages short but informative (e.g. "add plot function").
        * Ensure any new functions have unit tests and are properly documented (roxygen).
        * Update the README following any major changes.
        * Branches are ready to merge once the feature or fix is completed. Avoid pushing broken code or unfinished features / fixes.
> 3. When the branch is ready to be merged:
        1. Create a new merge request into the `dev` branch. 
            * Give the merge request a succinct, clear summary of the new feature or fix.
            * Assign a reviewer (see list of maintainers).
            * Select "Delete source branch when merge request is accepted".
            * Do not select "Squash commits when merge request is accepted".
        2. Resolve any merge conflicts.
        3. Reviewer comments / suggestions must be implemented before merging

The Climate and Health Team reviews merge requests on a regular basis.

## Current maintainers

Please assign one of the following maintainers to review a new merge request:

* Euan Soutter (@soutte)
* Antony Brown (@browna6)
* Paul Slocombe (@slocop)

### Code style

Please follow the coding style for this project. 
* Use tidyverse packages where possible.
* Employ coding best practice to ensure code is clean and readable:
    * [Tidyverse style guide for R](https://style.tidyverse.org/index.html)
    * [Quality assurance of code for analysis and research](https://best-practice-and-impact.github.io/qa-of-code-guidance/readable_code.html)
    * [PEP8 style guide for Python](https://peps.python.org/pep-0008/)
* In general, we do not load whole packages using the library() function. We reference the namespace directly e.g. `dplyr::mutate()`.
* Keep each line of code under 80 characters.
* Use comments sparingly.
