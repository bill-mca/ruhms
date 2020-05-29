### This code generates excel

### Here I make an attempt to define the post-intervention hydraulic formulas
### in terms of the pre-intervention formulas. I ended up deciding to just
### write each formula out explicitly which is code duplication and not ideal
### ultimately having seperate data.frames for the pre- and post-intervention
### scenarios will probably work out better. If this code were python instead
### of R I would've defined a class 'hydraulic_model' with two data.frames.

## hydraulic.formulas <- list(
##     hydraulic.radius = c(xsec.area="xsec.area", "/", wp.length="wp.length"),
##     velocity = c("(", hydraulic.radius="hydraulic.radius", "^(2/3)*",
##                  gradient="gradient", "^(1/2))/", roughness="roughness"),
##     discharge = c(velocity="velocity", "*", xsec.area="xsec.area"),
##     traction = c("9806*", depth="depth", "*", gradient="gradient"),
##     stream.power = c("9806*", depth="depth", "*", discharge="discharge", "*",
##                      gradient="gradient"),
##     unit.stream.power = c(stream.power="stream.power", "*",
##                           base.channel.width.distance=
##                               "base.channel.width.distance"),
##     froude.number = c(velocity="velocity", "/((9.8*", depth="depth", ")^0.5)")
## )

## jj <- unlist(sapply(hydraulic.formulas, names))
## jj <- jj[jj!='']
## jj <- jj[!duplicated(jj)]
## names(jj) <- NULL

## pi.jj <- paste('pi.', jj, sep='')
## hydraulic.formulas[[1]]['xsec.area'] <- pi.jj['xsec.area']
## hydraulic.formulas[[1]]['roughness'] <- pi.jj['roughness']

### Hydraulic formulas are defined in an ugly way as a means of fascilitating
### the replacement of variables with excel cell references
hydraulic.formulas <- list(
    hydraulic.radius = c(xsec.area="xsec.area", "/", wp.length="wp.length"),
    pi.hydraulic.radius = c(pi.xsec.area="pi.xsec.area", "/", pi.wp.length="pi.wp.length"),
    velocity = c("(", hydraulic.radius="hydraulic.radius", "^(2/3)*",
                 gradient="gradient", "^(1/2))/", roughness="roughness"),
    pi.velocity = c("(", pi.hydraulic.radius="pi.hydraulic.radius", "^(2/3)*",
                    pi.gradient="pi.gradient", "^(1/2))/",
                    pi.roughness="pi.roughness"),
    discharge = c(velocity="velocity", "*", xsec.area="xsec.area"),
    pi.discharge = c(pi.velocity="pi.velocity", "*",
                     pi.xsec.area="pi.xsec.area"),
    traction = c("9806*", depth="depth", "*", gradient="gradient"),
    pi.traction = c("9806*", pi.depth="pi.depth", "*",
                    pi.gradient="pi.gradient"),
    stream.power = c("9806*", depth="depth", "*", discharge="discharge", "*",
                     gradient="gradient"),
    pi.stream.power = c("9806*", pi.depth="pi.depth", "*",
                        pi.discharge="pi.discharge", "*",
                        pi.gradient="pi.gradient"),
    unit.stream.power = c(stream.power="stream.power", "/",
                          base.channel.width.distance=
                              "base.channel.width.distance"),
    ### I've decided to keep base.channel.width the same pre- and post interv.
    pi.unit.stream.power = c(pi.stream.power="pi.stream.power", "/",
                          base.channel.width.distance=
                              "base.channel.width.distance"),
    froude.number = c(velocity="velocity", "/((9.8*", depth="depth", ")^0.5)"),
    pi.froude.number = c(pi.velocity="pi.velocity", "/((9.8*",
                         pi.depth="pi.depth", ")^0.5)"),
    depth = c(topbank.elevation="topbank.elevation", "-",
              thalweg.elevation="thalweg.elevation"),
    pi.depth = c(topbank.elevation="topbank.elevation", "-",
              spill.elevation="spill.elevation")
)

## formulas.listas <-
##     lapply(hydraulic.formulas,
##            function(x){parse(text=(paste(x, collapse='')))}
##            )

## fl <- sapply(hydraulic.formulas, function(x){paste(x, collapse='')})

add.excel.formulas <- function(shr, formula.list, col.order=NULL){
    ### organise the columns into the order you want for the output here
    ### before the algoritm goes through and makes formulas
    if(!is.null(col.order)){
        shr.bac <- shr
        shr[ , col.order[!(col.order %in% colnames(shr))]]  <- NA
        shr <- shr[ , col.order]
        }
    excel.spreadsheet.colnames <- c(
        toupper(letters),
        unlist(lapply(
            toupper(letters), function(x){paste(x, toupper(letters), sep='')})
            )
    )
    fl <- sapply( formula.list, function(x){(paste(x, collapse=''))})
    #print (fl)
    already.present  <- names(fl)[names(fl) %in% colnames(shr)]
    excel.columns <- excel.spreadsheet.colnames[
        1:(ncol(shr)+length(fl)-length(already.present))]
    names(excel.columns) <-
        c(colnames(shr), names(fl)[!(names(fl) %in% colnames(shr))])
    #print(excel.columns)
    for(i in 1:length(excel.columns)){
        fl <- gsub(paste('(?<!\\.)', names(excel.columns)[i], sep='') ,
                   paste(excel.columns[i], 'xx', sep=''),
                   fl, perl=TRUE)}
                                        #fl <- paste('=', fl, sep='')
    #print(fl)
    for(i in 1:length(fl)){
        shr[[names(fl)[i]]] <-
            paste('=',
                  lapply(2:(nrow(shr)+1), function(x){gsub('xx', x, fl[i])}),
                  sep='')
    ### 2:nrow(shr)+1 because excel spreadsheets put the colnames in row 1
    }
    if(!is.null(col.order)){
        left.out <- shr.bac[ , !(colnames(shr.bac) %in% colnames(shr)),
                            drop=FALSE]
        shr <- cbind(shr, left.out)
        }
    return(shr)
}
