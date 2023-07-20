# Introduction

> Thank you for considering contributing to <climatehealth>.

> Following these guidelines helps to communicate that you respect the time of the developers managing and developing this open source project. In return, they should reciprocate that respect in addressing your issue, assessing changes, and helping you finalize your pull requests.

### What to Contribute

> <climatehealth> is an open source project and we love to receive contributions from our community — you! There are many ways to contribute, from writing tutorials or blog posts, improving the documentation, submitting bug reports and feature requests or writing code which can be incorporated into Elasticsearch itself.

# Ground Rules

> Responsibilities
> * Ensure cross-platform compatibility for every change that's accepted. Windows, Mac, Debian & Ubuntu Linux.
> * Create issues for any major changes and enhancements that you wish to make. Discuss things transparently and get community feedback.
> * Don't add any classes to the codebase unless absolutely needed. Err on the side of using functions.
> * Keep feature versions as small as possible, preferably one new feature per version.
> * Be welcoming to newcomers and encourage diverse new contributors from all backgrounds. 
> Beginner issues - issues which should only require a few lines of code, and a test or two.
> Help wanted issues - issues which should be a bit more involved than beginner issues.
> Both issue lists are sorted by total number of comments. While not perfect, number of comments is a reasonable proxy for impact a given change will have.

> Working on your first Pull Request? You can learn how from this *free* series, [How to Contribute to an Open Source Project on GitHub](https://egghead.io/series/how-to-contribute-to-an-open-source-project-on-github).

# Getting started

### Git workflow

* We use git-flow - create a feature branch from `develop`. If the feature corresponds to a Jira sub-task, prefix the branch name with the Jira issue key (e.g. `WTCHF-254-feature-name`)
* Merge requests must contain a succinct, clear summary of what the user need is driving this feature change. 
* Ensure your branch contains logical atomic commits before sending a pull request - follow the [alphagov Git styleguide](https://github.com/alphagov/styleguides/blob/master/git.md)
* You may rebase your branch after feedback if it's to include relevant updates from the develop branch. We prefer a rebase here to a merge commit as we prefer a clean and straight history on develop with discrete merge commits for features

### How to submit a Contribution.

>1. Create your own fork of the code
>2. Do the changes in your fork
>3. If you like the change and think the project could use it:
    * Be sure you have followed the code style for the project.
    * Send a pull request.

# How to report a bug

> If you find a security vulnerability, do NOT open an issue. Email climate.health@ons.gov.uk instead.

> When filing an issue, make sure to answer the questions in the Bug template.

# Code review process

> The Climate and Health Team looks at Pull Requests on a regular basis but cannot unfortunately guarantee prompt implementation of Pull Requests.
