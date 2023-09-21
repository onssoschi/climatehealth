Contributing guidelines
=======================

`climatehealth` is maintained and developed by the Climate and Health Team. Currently contributions can only be made internally.

We intend to open-source this project, and will welcome contributions. 

# Contributing

## General guidance

* We use Reproducible Analytical Pipeline (RAP) principles. 
* Aim to use tidyverse packages
* Do not upload any sensitive data to the repository
* Main is production / dev is development branch

### Code style

* Employ coding best practice to ensure code is clean and readable. Style guides tidyverse style guide for R / PEP8 for python
* Snake case
* In general, we do not load packages with library(). We reference the namespace directly e.g. `dplyr::mutate()`
* 80 character line width
* Keep comments to a minimum. Only to explain why code does what it does. 

### Git workflow

* We use git-flow: 
> 1. create a feature branch from `dev`, do changes in that branch. Give the branch a short and informative name.
> 2. Do the changes in your branch
> 3. When the branch is ready to be merged:
    * Be sure you have followed the code style for the project.
    * Create a new merge request.
* Merge requests must contain a succinct, clear summary of what the user need is driving this feature change. 
* Ensure your branch contains logical atomic commits before sending a pull request - follow the [alphagov Git styleguide](https://github.com/alphagov/styleguides/blob/master/git.md)
* Keep commits small and commit messages should clearly describe what has been changed (e.g. "Add plotting function")
* You may rebase your branch after feedback if it's to include relevant updates from the develop branch. We prefer a rebase here to a merge commit as we prefer a clean and straight history on develop with discrete merge commits for features
* merge requests into the dev branch only
* Do not squash commits when merging.
* Ensure that any new functions have unit tests.
* Main is protected 
* Whose responsibility is it to resolve merge conflicts? Person that requested the merge - "Resolve any merge conflicts before submitting the merge request"
* Reviewer comments / suggestions must be implemented before merging.
* Update the README following any major changes
* Do not push broken code or unfinished features / fixes. 

## Request a feature or fix

* If you would like the development team to implement new features, fix bugs etc. Please create and issue. Create issues for any major changes and enhancements that you wish to make. 
* **If you find a security vulnerability, do NOT open an issue. Email climate.health@ons.gov.uk instead.**
* Rules for raising an issue - name / clear which branch
* Be clear whether the issue is a 
>> request for a new feature
>> bug fix
* Assign a maintainer as a reviewer 

# Code review process

* The Climate and Health Team reviews merge requests on a regular basis.


