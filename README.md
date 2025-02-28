# d2mr-assessment

**Test change for github troubleshooting!**

This is a centralized directory for D2MR mini-project assessment materials. The purpose of this repository is to provide a single location for all mini-projects, which will allow you to easily access and complete them and your instructors to efficiently grade them.

Working with this repository will help you practice using git and GitHub, which are explicit learning objectives for the course!

Dr. Dowling owns the main repository, and you will create a clone of this repository to complete your mini-projects. This will create a copy of the repository in your GitHub account that you control and can modify without affecting the main repository or being visible to other students.

# Using this repo and completing assessments {#using-repo}

## Setup instructions {#setup}

~~Fork the repository.~~ Nope! Not doing that anymore. We're going to mimic forking with a clone. If you see any reference to your "forked" repo here or on the course website, please email Dr. Dowling so we can correct it.

View the full guide for how to set up your clone [here](https://nrdowling.com/d2mr/resources/guides/origin-upstream-repos.html). The guide has instructions for setting up a clone from scratch or from migrating from a fork if you already have one set up.

Briefly, you'll need to first set up your clone to assign both an "origin" (the remote GitHub repository for the clone that you own) and an "upstream" (the remote GitHub repository for the class repo that Dr. Dowling owns). You only need to do this once.

## In-class materials

Code demos and data we use in class or supplement the lectures are added to the `00_in-class-materials` directory. These materials are not part of the mini-project assessments, but they are here for your reference and practice.

## Completing mini-projects {#completing-projects}

Mini-projects on the menu have a corresponding directory in this repository. Each directory contains either a README.md file with general instructions for approaching the project *or* a Quarto notebook (e.g., `cleaning-level-1.qmd`) that will guide you through completing the more structured projects.

Before submitting, you'll need to add a copy of the [assessment.md](assessment.md) file (found here in this top-level directory) to the project directory and complete it. This file will be used to assess your work. Mini-project instructions will be published throughout the quarter, but you can complete projects before the materials are added.

## Project types

Mini-projects will mostly fall into one of the following categories: guided exercises, open-ended projects, or off-the-menu projects. The [menu overview](#menu-overview) below discusses these in more detail, but here's a quick summary of the mechanics for completing and submitting them:

### Guided exercises

These projects will have a Quarto notebook that will guide you through the project, like leveled data cleaning exercises. The notebook will include instructions, code snippets, and explanations to help you complete the project. Unless otherwise stated, all aspects of the project should complete these projects in the Quarto notebook.

### Open-ended projects

These projects will have a README.md file that will provide a general description of the project and some guidance on how to approach it. Provided it is sensible to do so, you will put your work in that same directory, in whatever form it takes.

In some cases it won't make sense (or even be possible) to complete the project in the directory of your assessment repo. For example, mini-projects that require some kind of "publication" (like a website or Quarto APA manuscript) will need to exist in their own dedicated repositories; they can't run as sub-directories of this one. Mini-projects intended to show off GitHub skills are nearly always going to need a separate repo.

In those cases, you should still add and complete an `assessment.md` file in the associated project directory (here in this centralized repo), which will include any necessary information about how to access your work.

### Off-the-menu projects {#otm-projects}

You can also design your own "off-the-menu" projects. To do so, create a new directory in the repository, name it appropriately, and add a copy of the [otm-proposal.md](otm-proposal.md) file from this directory. Instructions for completing and submitting the proposal via Canvas are included in that document.

Once approved, you can start working on the project. When you submit your finished project, you'll follow the same process as the on-the-menu projects, but you'll be resubmitting to the same assignment on Canvas that you submitted your proposal to.

## Submitting mini-projects {#submitting}

When you finish a mini-project, you should complete the [assessment.md](assessment.md) file in the project subdirectory, then submit a link to that subdirectory on Canvas.

**No matter what kind of project it is (structured, open-ended, or off-the-menu), the project needs a directory here in this repo, and that directory needs to include a completed [assessment.md](assessment.md) file.**

Submit to any one of your 10 mini-project assignments on Canvas (whichever the next open one is, doesn't matter as long as it's not one you've already used). The Canvas assignment is simply a text box where you should include:

1.  The name of the project and/or a brief description of the project
2.  A link to the subdirectory for your mini-project
    1.  If you completed the project in a separate repository, your [assessment.md](assessment.md) file in the subdirectory should explain how to access your work.
3.  Any additional info you'd like your grader to know

Your grader will access the directory you've linked to, review your work, and provide feedback in the [assessment.md](assessment.md) file. Once the assessment is complete, your grader will push the file back to your repository, and your Canvas assignment submission will be marked "complete." (Note: if a TA graded your work, Dr. Dowling will review and finalize grades before marking the assignment complete.)

### Pulling from the main (upstream) repository {#pullupstream}

You should periodically pull changes from the upstream repository (the one owned by Dr. Dowling) to your cloned repository to ensure you have the most up-to-date version of the assessment materials. Unfortunately RStudio doesn't have a nice easy button to click for this, so you'll need to do it from the terminal pane in RStudio.

Once you have set up your origin and upstream repos, you'll pull and push changes to *your* repo using the RStudio interface (the "pull" and "push" buttons in the Git pane). You'll pull from (and never push to) the upstream repo using the terminal command `git pull upstream main`. T he [guide for using your cloned repo repo](https://nrdowling.com/d2mr/resources/guides/origin-upstream-repos.html) walks through both the one-time setup process and this regular workflow process step by step.

# Mini-project menu overview {#menu-overview}

Earn up to 40 points across all mini-projects for meeting course objectives, and an additional 10 for engagement.

**These projects do not have strict submission or grading requirements.** You can make any mini-project as simple or as complex as you like. If you write a `hello_world()` function that's 5 lines long, that's fine, but you'll probably only be able to demonstrate a couple of the learning objectives. If you write a `hello_world()` function that's 50 lines long, or write multiple functions and build them into your own package, you'll probably be able to demonstrate a whole lot more than that. The complexity you aim for can matter for engagement points. That 5-line `hello_world()` function might not earn any engagement points, while the 50-line one could earn you all 10.

This goes for the structured guided exercises as well, like the leveled data cleaning exercises. The Level 1 exercises are designed to be relatively simple and straightforward, while the Level 2 exercises are more complex and open-ended, but you can complicate a Level 1 and simplify a Level 2 as much as you like. You're assessed on demonstrating the objectives and engagement, not on following the directions of the exercise to the letter.

Some projects are marked as "off the syllabus" (OtS). These are opportunities to learn and demonstrate skills that aren't explicitly covered in the course. These will let you showcase the skills we do explicitly cover in a different context, or let you explore something that interests you.

You are welcome to go "off the menu" (OtM) to design your own mini-project that lets you focus on your interests while still demonstrating the course objectives. If you choose to do this, you should submit a proposal for approval using the [otm-proposal.md](otm-proposal.md) file in this directory. Instructions for completing and submitting the proposal are in the same document.

### Mini-project menu {#project-menu}

This is the structure of this repository. Second-level directories group projects by the category of skills they assess (mosty; expect a LOT of overlap!). Third-level directories are individual projects.

Materials for mini-projects (either a simple readme with general instructions or guided content with a .qmd) will be added to their respective subdirectories as we progress through the quarter, but you may complete projects before the materials are added.

Projects may be added or revised throughout the quarter, so be sure to keep your clone up-to-date with the main repository. There's also a very good chance that not all projects will have published instructions, especially the off-the-syllabus projects and the `unassessed_misc` projects. Materials will be added in those kinds of "extra" projects as I get a better sense of students' interests and abilities.

**Tags:**

1.  **(GE):** This is a semi-structured guided exercise with a .qmd notebook to guide you through the project. These typically have both a "Level 1" and "Level 2" version, with the latter being more complex and offering less guidance.

2.  **(DEMO)**: This is not a mini-project, but a demonstration of the skills you'll need to complete the related mini-project. There is nothing to complete, and you should not submit these for grading.

3.  **(OtS)**: This project is "off-the-syllabus" because it involves skills beyond the learning objectives of the class.

4.  **(2+)**: This project is designed for pairs or groups of students to complete together.

5.  Data cleaning

    -   (GE, DEMO) Walkthrough

    1.  (GE) Level 1
    2.  (GE) Level 2
    3.  (2+) Un-clean some data & swap to clean

6.  Data wrangling

    -   ~~(GE, DEMO) Walkthrough~~ \*(will add if needed)

    1.  (GE) Level 1
    2.  (GE) Level 2
    3.  (2+) Wrangle some data & swap to recreate
    4.  (OtS) Access, read in, and wrangle nested or non-tabular data (e.g., JSON, XML)

7.  Data visualization & presentation

    -   (GE, DEMO) Walkthrough

    1.  (GE) Level 1
    2.  (GE) Level 2
    3.  Create a custom ggplot2 theme
    4.  (2+) Make plots (w/ some minimum requirements) & swap to recreate
    5.  Create beautiful tables with packages like `kableExtra`, `flextable`, `stargazer` , `gt`, or `xtable` ([there are lots!](https://towardsdatascience.com/top-7-packages-for-making-beautiful-tables-in-r-7683d054e541))
    6.  (OtS) Create a plot with non-ggplot2 packages

8.  Data analysis

    -   Note: Although projects 1 and 2 will include "guided exercises," these are much less guided that the cleaning, wrangling, and visualization GEs. I encourage you to approach either of these in your own way, with or without the structure.

    1.  (GE) Descriptive statistics
    2.  (GE) Hypothesis testing
    3.  (OtS) Higher-level statistics

9.  Data communication

    1.  Transpose a paper into Quarto markdown
    2.  Create a simple .qmd with basic elements
    3.  Create a demo .qmd or .Rmd for one or more class topics
    4.  (OtS) Recreate your Quarto report with papaja or RMarkdown
    5.  (OtS) Use Quarto to publish something other than a document (e.g., website, presentation, dashboard)
    6.  (OtS) Create a `shiny` app or dashboard

10. R Programming

    -   (DEMO) `hello_world()` examples

    1.  Create a `hello_world()` function
    2.  Create a plotting function
    3.  Create a wrangling function
    4.  Recreate an existing function
    5.  (OtS) Create a package

11. Git & GitHub (these can use real projects or dummy data)

    1.  Set up a skeleton repo to create APA quarto manuscripts
    2.  (2+) create and maintain a repo as collaborators and use cloning, forking, merging etc
    3.  (OtS) Create a GitHub pages website ~~4. (OtS) Host a package through a GitHub repo~~ (oops, this was a duplicate of 6.5 above)

12. Unassessed Learning Objectives & Miscellaneous

    1.  Create and document a style guide and apply it to a script or notebook
    2.  Create a debugging journal
    3.  Write documentation for a function that you’ve written (e.g., a `hello_world()` function or something you use in your independent project), following the standard documentation guidelines and expectations (i.e., it should match what you see when you use `?somefunction` )
    4.  In the format of your choosing (bullet points, paragraphs, mindmap, flowchart, slides, etc.), show me how you understand the points listed under “conceptual skills”.
    5.  Get obsessed with LaTeX!
    6.  Contribute answers to stackoverflow, reddit, etc.
    7.  Create a tutorial on a topic of your choosing in the format of your choosing
    8.  Fork the `schelling-games` repo and mess around with a range of skills

### FAQ

Visit the [FAQ page](http://nrdowling.com/d2mr/faq.html) of the course website for answers to common questions about the mini-projects and other course expectations.
