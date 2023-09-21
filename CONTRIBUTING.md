Contribution guidelines
=======================

`climatehealth` is maintained and developed by the ONS Climate and Health Team. Currently contributions can only be made internally.

We intend to open-source this project in future, and will welcome contributions from external users and developers. 

## General guidance

* Do not upload any sensitive data to this repository.
* `main` is the production branch. This branch is protected. The `dev` branch is merged into `main` periodically.
* `dev` is the development branch. This branch is the default branch. Branch from `dev` to develop new features. 

## Current maintainers

* Euan Soutter (@soutte)
* Antony Brown (@browna6)
* Paul Slocombe (@slocop)

## Merge request process

To implement a new feature or fix a bug, please follow the process outlined below.

> 1. Create an issue clearly describing the feature or fix.
> 2. Create a feature branch from `dev` and do changes in that branch. 
> 3. When the branch is ready to be merged:
        1. Create a new merge request into the `dev` branch. 
            * Assign a reviewer (see list of maintainers).
            * Select "Delete source branch when merge request is accepted".
            * Do not select "Squash commits when merge request is accepted".
        2. Resolve any merge conflicts.
        3. Reviewer comments / suggestions must be implemented before merging

## Additional guidance

* Ensure any new functions have unit tests and are properly documented (roxygen).
* Update the README following any major changes


### Merge requests 

* Merge requests must contain a succinct, clear summary of what the user need is driving this feature change. 
* Ensure your branch contains logical atomic commits before sending a pull request - follow the [alphagov Git styleguide](https://github.com/alphagov/styleguides/blob/master/git.md)
* Keep commits small and commit messages should clearly describe what has been changed (e.g. "Add plotting function")

### 
* Whose responsibility is it to resolve merge conflicts? Person that requested the merge - "Resolve any merge conflicts before submitting the merge request"
* Do not push broken code or unfinished features / fixes. 

## Request a feature or fix

* If you would like the development team to implement new features, fix bugs etc. Please create and issue. Create issues for any major changes and enhancements that you wish to make. 
* **If you find a security vulnerability, do NOT open an issue. Email climate.health@ons.gov.uk instead.**
* Rules for raising an issue - name / clear which branch
* Be clear whether the issue is a 
>> request for a new feature
>> bug fix
* Assign a maintainer as a reviewer 

## Code review process

* The Climate and Health Team reviews merge requests on a regular basis.

### Code style

Please follow the coding style for this project
* tidyverse
* Employ coding best practice to ensure code is clean and readable. Style guides tidyverse style guide for R / PEP8 for python
* Snake case
* In general, we do not load packages with library(). We reference the namespace directly e.g. `dplyr::mutate()`
* 80 character line width
* Keep comments to a minimum. Only to explain why code does what it does. 


