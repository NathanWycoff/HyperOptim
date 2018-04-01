This is a repository dedicated to a project to optimize machine learning hyperparameters using information visualization techniques.

It is an R web application which uses RShiny.

##Install Instructions

These instructions will be given for an Ubuntu Linux machine. Similar steps would need to be followed on other platforms.

**Step 1: Install R**

Simply do:

apt install r-base

**Step 2: Install RShiny**

R packages may be installed from within R:

launch R, and then run:

install.packages("shiny")

from within.

##Useage Instructions*

The main function to interact with the service is remote\_run from the web\_wrapper.R file. See the /examples/ folder, in particular lda\_example.R for useage cases. Running that function will launch a webbrowser with the application.
