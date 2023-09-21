Contributing guidelines
=======================

`climatehealth` is currently maintained and developed by the Climate and Health Team. 

We intend to open-source this project, and will welcome contributions. 

# Contributing

## General guidance

* We use Reproducible Analytical Pipeline (RAP) principles. 
* Aim to use tidyverse packages
* Best practice coding. Clean and readable code (80 character line width) - link to guides. PEP8 / tidyverse style guide
* Do not upload any sensitive data to the repository
* Main is production / dev is development branch

### Git workflow

* We use git-flow: 
> 1. create a feature branch from `develop`, do changes in that branch
> 2. Do the changes in your fork
> 3. If you like the change and think the project could use it:
    * Be sure you have followed the code style for the project.
    * Send a pull request.
* Merge requests must contain a succinct, clear summary of what the user need is driving this feature change. 
* Ensure your branch contains logical atomic commits before sending a pull request - follow the [alphagov Git styleguide](https://github.com/alphagov/styleguides/blob/master/git.md)
* Keep commits small and commit messages should clearly describe what has been changed (e.g. "Add plotting function")
* You may rebase your branch after feedback if it's to include relevant updates from the develop branch. We prefer a rebase here to a merge commit as we prefer a clean and straight history on develop with discrete merge commits for features
* merge requests into the dev branch only
* Do not squash commits when merging.
* Ensure that any new functions have unit tests.
* Main is protected 
* Whose responsibility is it to resolve merge conflicts? Person that requested the merge - "Resolve any merge conflicts before submitting the merge request"

## Request a feature or fix

* If you would like the development team to implement new features, fix bugs etc. Please create and issue. Create issues for any major changes and enhancements that you wish to make. 
* **If you find a security vulnerability, do NOT open an issue. Email climate.health@ons.gov.uk instead.**
* Rules for raising an issue - name / clear which branch
* Be clear whether the issue is a 
>> request for a new feature
>> bug fix
* Assign a maintainer as a reviewer 

# Code review process

* The Climate and Health Team reviews merge requests on a regular basis

