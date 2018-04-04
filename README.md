# oetteR

This is my personal R package which contains a number of functions that help me to maintain an organized workflow. Most of the functions
are just curried versions of functions from other packages, nevertheless I added a decent amount of documentation, however there are
a few packages that I cosntantly use and that one should know about in order to understand the documentation.

# Prerequisites

- [Pipes and the Tidyverse](http://r4ds.had.co.nz/)
- Functional dataframe based modelling concepts as described in the R4DS book linked above. A package that I started out with is 
 [`pipelearner`](https://github.com/drsimonj/pipelearner) and  [`twidlr`](https://github.com/drsimonj/twidlr). A lot of my code 
 examples use these two packages and I even use them in some of my functions. They also have a nice tutorials on their github page and a great simple syntax that make it easy to get the advantages of dataframe based modelling concepts. Those two packages are great however there is a major disadvantage in my opinion.
 If you train a lot of models `pipelearner` will always store them in memory. R models tend to be very large so you quickly fill up 
 your memory on weaker machines. Further you cannot train a model whithout defining some sort of test set. Which sometimes had me define
 a test set with one observation. So at the moment I still use `pipelearner` for its great syntax but tend to write my own modelling
 dataframes and to use the tools devloped by Max Kuhn `rsample` `recipes` and `caret`.
 - I generate a lot of my visual results as `html` and use `rmarkdown` to its full capacity. In my project folder I usually have
 one folder for `Rmd` and one for `html` files. I have one `Rmd` file for each step in my workflow and all resulting `hmtl`s are
 rendered to the `html` folder. I have one `execute.R` file in the parent project folder which triggers rendering of all `Rmds`.
 The last `Rmd` file to be rendered will generate a index.html file which is basically a catalogue file with links to all `htmls`.
 This `index.html` file can be found in the project parent folder. I try to use widgets over static plots and tables whenever I can.
 It helps to know the following packages at least bit `webshot`, `bookddown`, `htmltools`, `knitr`, `plotly`, `DT`

# Naming

The names of all functions start with `f_` followed by another prefix that describes the role of the function inside my workflow

# Workflow (including links to vignettes)

## Variable Manipulations and Transformations `f_manip*`

## Data Preparation `f_clean_*`

When thinking about this step I did not know about the `recipes` package. I came up with something similar but less elabprate and 
thought-through. 

- [Data Perparation and PCA](http://rpubs.com/erblast/365505)

## Data Exploration and Visualisation `f_pca_*`, `f_stat_*`, `f_plot_*`

- [Alluvial Plots](http://rpubs.com/erblast/365703)
- [Group Analysis](http://rpubs.com/erblast/366964)


## Feature Testing `f_model_importance_*`

- [Leakage and variable importance for modelling](http://rpubs.com/erblast/366422)

## Model Training `f_train_*`

- [Gamma/Tweedie Regression with Lasso](http://rpubs.com/erblast/366619)

## Model Visualisation `f_model_plot_*`

- [Visualising Regression Models](http://rpubs.com/erblast/365705)

## Reporting `f_html_*`, `f_datatable`, `f_plot_obj_2_html`

These functions help me to generate html output.

## Apps `f_shiny_*`, `f_simulation_*`

These functions start a shinyserver and let you run a simulation or a shiny app.

- `f_shiny_multiview` runs a tool for analysing labeled groups in a data set  
- `f_shiny_som` runs a tool for training self organizing maps. The subsequent clustering algorithm allows you to cluster only adjacent areas of the map.  

You can pass your own data via the `data` argument to both functions, or select from a variety of sample datasets if `data == NULL`.

# Additional Content

Apart from the vignettes that I have linked to already above I have added some files that I produced as prrof of concept (POC) files.
When I try out a new package or get stuck somewhere with a problem I like to write those as a minimal example of how to solve or not to solve a problem. One can use the function `f_content()` to produce and open a index `html` file with links to all those files.

Some of the POCs are also posted on Rpubs

## Visualisations
- [clickable file links in DT::datatable](http://rpubs.com/erblast/369527)  
- [mix taglist and plots](http://rpubs.com/erblast/369525)  
- [ggalluvial](http://rpubs.com/erblast/369522)  
- [ggpubr](http://rpubs.com/erblast/369520)  

## Modelling

### Modelling Dataframe packages
- [Pipelearner for classification](http://rpubs.com/erblast/369542)  
- [rsample](http://rpubs.com/erblast/370014)
- [recipes](http://rpubs.com/erblast/recipes)
- [caret](http://rpubs.com/erblast/caret)

### Regression models with caret
- [01 robust linear regression, `rlm`](http://rpubs.com/erblast/rlm)
- [02 neuronal networks, `nnet`](http://rpubs.com/erblast/nnet)
- [03 multiviariate adaptive regession splines (MARS), `earth`](http://rpubs.com/erblast/mars)

### Various
- [WOE, IV Scorecard in Credit Risk Modelling](http://rpubs.com/erblast/creditrisk)

## Algorithms
- [SOM with connectivity restrains](http://rpubs.com/erblast/SOM)  



