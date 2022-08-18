# earlycause-tools

This repository provides a way to share tools that support the [EarlyCause Portal](https://portal.earlycause.eu/).

For any questions please contact us at [ena-earlycause@ebi.ac.uk](mailto:ena-earlycause@ebi.ac.uk).

## Glossary

- *Repository* - a filesystem folder tracked by a version control system (VCS) such as Git.
- *Forking* - copying a main repository to your own GitHub account.
- *Cloning* - downloading a repository to your local machine.
- *Committing* - saving local changes to the repository.
- *Pushing* - pushing local committed changes to the remote repository.
- *Pull request* - process of merging changes from your repository to the main repository.

In the following diagram you can find how the above concepts interact:

<img src="media/instructions-general.png?raw=true" width="650">

## Submission process

Before submitting your tools/scripts you have to create a [GitHub](https://github.com/) account, if you haven't already.

### 1. Fork the repository

In order to upload your files you have to "fork" this repository.
This means that GitHub will create a **copy** of the repository under your personal account.

To do this click on the **Fork** button in the top left:

![Forking](media/instructions-fork.png?raw=true "Forking")

### 2. Install GitHub Desktop

In order to make changes/add files and submit them to the forked repository, you need to install [GitHub Desktop](https://desktop.github.com/).

Once installed, sign-in to GitHub:

![Sign-in to GitHub](media/instructions-gh-desktop-sign-in.png?raw=true "Sign-in to GitHub")

### 3. Clone the repository

Now you can clone (download) the forked repository by searching it through GitHub Desktop and clicking "Clone earlycause-tools":

![Clone repository](media/instructions-gh-desktop-clone.png "Clone repository")

Once the repository is cloned, you will be prompted on how you would like to use the forked repository; please choose the first option, as shown below:

![Fork usage](media/instructions-gh-desktop-fork-use.png "Fork usage")

### 4. Add tools

Now you can add your tools/scripts to the repository.
There should be a folder for your institution in the repository. If this isn't present, please contact [ena-earlycause@ebi.ac.uk](mailto:ena-earlycause@ebi.ac.uk) for further details.

Once you add files to your institution folder you will see the changes in GitHub Desktop as follows:

![Commit](media/instructions-gh-desktop-commit.png "Commit")

Once you're happy with the changes click on "Commit to main" - optioanlly you can provide a description of the changes using the appropriate box.

### 5. Submit for approval

Now the changes you have made are local. In order to push them to your remote forked repository, click "Push origin":

![Push](media/instructions-gh-desktop-push.png "Push")

Now you have to create a pull request for the EarlyCause team to review. You can do so in two ways:

- On GitHub Desktop, in the menu bar, click on "Branch" then "Create pull request".
- On the GitHub website, in your forked repository, click on "Contribute" then "Open pull request" (see below)

![PR](media/instructions-gh-pr.png "PR")

Once you create the pull request, the EarlyCause team will be notified and will be able to review the changes. Once approved, these will be merged in the parent repository.
