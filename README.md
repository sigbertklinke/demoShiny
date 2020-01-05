# demoShiny #

Runs a Shiny app as demo or shows an overview of all Shiny demo apps related to a `topic`. 
If more than one app relates to `topic` a list of apps is returned otherwise the app is run.
For more details see `vignettes('demoShiny')`.

```r
# collect all apps of loaded packages
demoShiny() 
# collect all apps of the package demoShiny or with the name demoShiny
demoShiny('demoShiny') 

```

