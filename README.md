# Satellite derived Chlorophyll data visualization

UNDER DEVELOPMENT

This is a R Shiny application that has been developed to visualize
satellite derived chlorophyll data for the pre-defined boxes
for each dfo region involved in the atlantic zone monitoring program.
The focus of this app is to test out various bloom fitting
methods.

# How to use :

## Standalone (i.e. local shiny server)

1. Install [R](www.r-project.org) and [RStudio](www.rstudio.com)

2. Download the Shiny app code from this repository, either through direct download, using Github Desktop, or by typing:

```
$ git clone https://github.com/clayton33/bloomFitApp.git
```

in a terminal.

The latter is recommended, as then updating the app can be done
with a simple `git pull` command or by pulling changes in Github 
Desktop.

3. Open the file `app.R` in RStudio. You will need to install 
several packages (as listed at the top of `app.R`). To do this
run the following in the R console (copy/paste):
```r
install.packages(c('shiny','shinyWidgets','oce','ocedata', 'quantreg'))
```

4. After installing all the required packages, run the app by 
clicking "Run App" at the top of the code editor.

5. Once the app is running, select the satellite, region,
box, and year from the pull down menus, and click 'Run' to load
the data for those defined variables. The user can then play
with methods to clean up the data and various methods to infer
spring bloom parameters.


# Information on other included files

00_regionBoxes.R - definition of all satellite boxes used for each
	region, note that Labrador Sea is also included here and
	as its own region. Depreciated boxes have also been included
	in this file, but are not appended to the region lists

