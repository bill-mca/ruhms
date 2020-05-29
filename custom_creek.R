#! /usr/bin/R

source('./creek_model.R')
source('./xlsx_generator.R')

###    Creek Model is a program for basic hydraulic modelling of waterways
###    Copyright (C) 2020 William McAlister

### NOTE
### User controlled variables are styled in caps with underscores eg USER_VAR
### Program variables are styled with lower case and dots eg hidden.var

### Development to do:
###
### Improve the way that data flows through the different steps. Doing so will
####### ensure one lot of pre-intervention depth numbers is used throughout
####### which will solve the disagreement between excel and R number noted.
####### See also, comments on coord.list

### Known issues:
###
### Excel calculates depth different to R because R uses the thalweg from the
####### provided coordinates whereas excel uses the thalweg you feed it.
####### I've observed a 5% difference in calculated unit.stream.power as a
####### result. Error should be proportional to imprecision of measurement of
####### thalweg.elevation from the profile chart.
####### I made a hotfix by explicitly overwriting input thalweg.elevation.
### Structure xsec.area != xsec.area - pi.xsec.area in every case. I suspect
####### This is the result of rough interpolation of coordinates during
####### calculation of xsec areas.
### Some of the variables have unconventional/unnecessarily-long names. I don't
####### know what possessed me to use base.channel.width.distance for example.


### Here the user chooses between read.qgis.csv or read.global.mapper.csv
### read.qgis.csv is compatible with ArcGIS profiles
DEFAULT_CSV_READER <- read.qgis.csv

### Catchment name for example: Sandhills - Reedy Creek
CATCHMENT_NAME <- 'Landtasia - Mulloon'

### Survey data reference. For example:
### Derived from Spatial Services (NSW) 2016 Braidwood LiDAR survey 1m DEM
SURVEY_DATA_REF <- 'Derived from Spatial Services (NSW) 2016 Braidwood LiDAR survey 1m DEM'

### Define the value of surface roughness pre- and post-intervention here:
### Applies the same value at every site.
ROUGHNESS  <- 0.04
PI_ROUGHNESS <- 0.04

### Read in the main input data.frame
shr <- read.csv('input_site_table.csv', stringsAsFactors=FALSE)

### I don't know why, but excel csvs seem to add a weird special character in
### cell A1. Uncommenting the next line will fix the problem.
colnames(shr)[1] <- 'site.code'

### Check if there is an input csv of coordinates for each row of the data.frame
fnames <- paste('input_csvs/', shr$site.code, '.csv', sep='')
#names(fnames) <- shr$site.code
missing.files <- !sapply(fnames, file.exists)
if(any(missing.files)){
    stop(paste(
        "ERROR Can't find the following csvs in the input_csvs/ folder:",
        paste(names(missing.files)[missing.files], collapse=', ')
    ))
}

### A future version of creek model should opperate on this coord.list
### by asserting that nrow(input.data.frame) == length(coord.list)
### or naming coord.list
#coord.list <- lapply(fnames, read.qgis.csv)
#thalwegs <- sapply(coord.list, get.thalweg)

### This function calculates the geometrical parameters from the coordinates.
### It also reads in the coordinate CSVs.
transects <- creek.model(shr, read.func=read.qgis.csv)

### A future version should split the model.output function into several steps
### 1 - Calculate the geometrical parameters
### 2 - return coordinates with geometrical parameters
### 3 - draw cross sections
### 4 - draw thalweg diagrams
### thalweg diagrams should be completely seperable and should be able to work
####### by optionally generating thalwegs from the coord.list as per:
####### thalwegs <- sapply(coord.list, get.thalweg)


col.order <- c(
    "site.code",
    "thalweg.elevation", "topbank.elevation", "spill.elevation",
    "structure.height", "depth", "pi.depth",
    "distance.to.bottom", "distance.to.downstream",
    "base.channel.width.distance", "structure.width.distance",
    "wp.length", "pi.wp.length",
    "xsec.area","pi.xsec.area", "structure.xsec.area",
    "gradient", "pi.gradient",
    "roughness", "pi.roughness",
    "hydraulic.radius","pi.hydraulic.radius",
    "velocity", "pi.velocity",
    "discharge", "pi.discharge",
    "traction", "pi.traction",
    "stream.power", "pi.stream.power",
    "unit.stream.power", "pi.unit.stream.power",
    "froude.number", "pi.froude.number"
)

### This function generates a CSV spreadsheet of geometrical parameters
### calculated by creek.model. It also draws cross section graphs and thalweg
### diagrams.
model.output <- model.from.transects(
    transects, NULL, CATCHMENT_NAME, SURVEY_DATA_REF)

### Default routine to calculate gradients. Assumes that us.step.for.grad
### and ds.step.for.grad are defined columns of shr
shr$gradient <- calculate.gradient(shr, pi=FALSE)
shr$pi.gradient <- calculate.gradient(shr, pi=TRUE)

### User defined surface roughness for all sites. For a more nuanced approach
### Comment these out and define roughness and pi.roughness in the
### input_site_table.csv
shr$roughness <- ROUGHNESS
shr$pi.roughness <- PI_ROUGHNESS

### This function uses all of the above to create a hydraulic model.
hydrau <- model.calculation(cbind(model.output,
                                  shr[ , c('gradient', 'pi.gradient',
                                           'roughness', 'pi.roughness')]))

### This function draws graphs and writes a CSV
hydraulics.output(hydrau)

### Generate an Excel spreadheet version of the hydraulic calculations.
### This'll be saved into the working directory instead of the timestamped
### Hydraulic modeling output folder (for now).
excel.output <-
    cbind(shr, model.output[ , !(colnames(model.output) %in% colnames(shr))])
excel.output$thalweg.elevation <- model.output$thalweg.elevation
excel.output <- add.excel.formulas(excel.output, hydraulic.formulas, col.order)
write.csv(excel.output, paste(format(Sys.time(), "%Y-%m-%d-%H-%M"),
                              CATCHMENT_NAME, 'Excel.csv'),
          row.names=FALSE, na='')
