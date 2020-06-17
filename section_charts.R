
### From here down the functions defined relate to the construction of thalweg
### diagrams

### you should be able to set which side of the diagram the highest structure
### will be drawn on

### This function draws vertical lines on an existing R plot
draw.vertical.lines <- function(x, y.bot, y.top, colour='#5876b0', lwd=1){
    stopifnot(all(length(x) == length(y.bot), length(x) == length(y.top)))
    for (i in 1:length(x)) {
        l <- rbind(c(x[i], y.bot[i]),
                   c(x[i], y.top[i])
                   )
        lines(l, col=colour, lwd=lwd)
    }
}

### This function draws Horizontal lines or arrows on an existing R plot
draw.horizontal.lines <- function(x.left, x.right, y, colour='#5876b0', lwd=1,
                                  ARROWS=FALSE){
    stopifnot(all(length(x.left) == length(y),
                  length(x.left) == length(x.right)))
    for (i in 1:length(y)) {
        l <- rbind(c(x.left[i], y[i]),
                   c(x.right[i], y[i])
                   )
        if(ARROWS){
            arrows(x0=l[1 ,1], y0=l[1 ,2],
                   x1=l[2, 1], y1=l[2, 2],
                   col=colour, lwd=lwd, length=0.03, code=2)
        } else {
            lines(l, col=colour, lwd=lwd)
        }
    }
}

draw.levels.chart <- function(input.data.frame,
                              catchment.name='unnamed catchment',
                              source.ref='',
                              output.dir='.',
                              cex=1){
    ### input.data.frame must have the following fields:
    attach(input.data.frame)
    ### calculate the extents of the chart
    height <- max(c(thalweg.elevation, topbank.elevation))
    y.top <- max(c(thalweg.elevation, spill.elevation, topbank.elevation))
    y.bot <- min(c(thalweg.elevation, spill.elevation, topbank.elevation))
    width <- tail(distance.to.bottom, 1) - head(distance.to.bottom, 1)
    xlims <- c(0, width)
    ylims <- c(y.bot-1, y.top+1)
    ### Setup the diagram to be scaled correctly
    ### The diagram should be scaled 1:100 vertical and 1:10000 horizontal
    ### R takes input in inches
    height.in <- (((y.top - y.bot)/100)*39.37) + 2 # + 2 for margins
    width.in <- ((width/10000)*39.37) + 2         # + 2 for margins
    ### little debug line and then setup the SVG to be the right size
    print(paste(catchment.name, height.in, ' x ', width.in))
    svg(paste(output.dir, '/', catchment.name, " - Levels.svg", sep=''),
        width=width.in, height=height.in)
    ### draw the plot window and label the axes
    plot.new()
    par(mai=c(1,1,1,1), cex=cex)
    plot.window(xlims, ylims)
    #box()
    title(main = paste(catchment.name, 'Long Section'),
          sub  = source.ref,
          xlab = paste(round(tail(distance.to.bottom, 1)/1000, 2), 'km'),
          ylab = paste(round(y.top-y.bot[1], 2), 'm'),
          cex=cex)
    ### Draw the thalweg
    lines(cbind(distance.to.bottom, thalweg.elevation), lwd=2, col='#5876b0')
    ### Draw the topbank
    lines(cbind(distance.to.bottom, topbank.elevation), col='#4e4e4e', lwd=2)
    ### Draw vertical lines to represent the new structures
    text(thalweg.elevation~distance.to.bottom, cex=0.8, labels=site.code,
         adj=c(-0.2,1.2))
    ## This draws vertical lines proportional in height to the structures:
    draw.vertical.lines(distance.to.bottom, thalweg.elevation, spill.elevation)
    ## Check if sites are lower on the left or right of the page:
    if(thalweg.elevation[1] < thalweg.elevation[length(thalweg.elevation)]){
    ## This draws horizontal lines representing the backwater of each structure
    ## It'll look weird if the structures don't top-to-tail:
        draw.horizontal.lines(
            distance.to.bottom[1:(length(distance.to.bottom)-1)],
            distance.to.bottom[2:length(distance.to.bottom)],
            spill.elevation[1:(length(spill.elevation)-1)]
        )
    }
    else {
        draw.horizontal.lines(
            distance.to.bottom[2:(length(distance.to.bottom))],
            distance.to.bottom[1:length(distance.to.bottom)-1],
            spill.elevation[2:(length(spill.elevation))]
        )
    }
    ### Draw the X axis
    axis(1, at=c(0, tail(distance.to.bottom, 1)), col='#4e4e4e', lty=1,
         lwd=2, labels=F)
    ### Draw the Y axis
    axis(2, at=c(y.bot, y.top), col='#4e4e4e', lty=1, lwd=2, labels=F)
    ### Turn off the SVG device
    dev.off()
    detach(input.data.frame)
}

draw.sites.chart <- function(input.data.frame,
                              catchment.name='unnamed catchment', source.ref='',
                             output.dir='.',
                             cex=1){
    ### input.data.frame must have the following fields:
    attach(input.data.frame)
    ### calculate the extents of the chart
    height <- max(c(thalweg.elevation, topbank.elevation))
    y.top <- max(c(thalweg.elevation, spill.elevation, topbank.elevation))
    y.bot <- min(c(thalweg.elevation, spill.elevation, topbank.elevation))
    width <- tail(distance.to.bottom, 1)
    xlims <- c(0, width)
    ylims <- c(y.bot-1, y.top+1)
    ### Setup the diagram to be scaled correctly
    ### The diagram should be scaled 1:100 vertical and 1:10000 horizontal
    ### R takes input in inches
    height.in <- (((y.top - y.bot)/100)*39.37) + 2 # + 2 for margins
    width.in <- ((width/10000)*39.37) + 2         # + 2 for margins
    ### little debug line and then setup the SVG to be the right size
    print(paste(catchment.name, height.in, ' x ', width.in))
    svg(paste(output.dir, '/', catchment.name, " - Sites.svg", sep=''),
        width=width.in, height=height.in)
    ### draw the plot window and label the axes
    plot.new()
    par(mai=c(1,1,1,1), cex=cex)
    plot.window(xlims, ylims)
    #box()
    title(main = paste(catchment.name, 'Long Section'),
          sub  = source.ref,
          xlab = paste(round(tail(distance.to.bottom, 1)/1000, 2), 'km'),
          ylab = paste(round(y.top-y.bot[1], 2), 'm'))
    ### Draw the thalweg
    lines(cbind(distance.to.bottom, thalweg.elevation), lwd=2, col='#5876b0')
    ### Draw the topbank
    lines(cbind(distance.to.bottom, topbank.elevation), col='#4e4e4e', lwd=2)
    ### Draw vertical lines to represent the new structures
    text(thalweg.elevation~distance.to.bottom, cex=0.8, labels=site.code,
         adj=c(-0.2,1.2))
    ## This draws vertical lines at each structure site:
    draw.vertical.lines(distance.to.bottom, thalweg.elevation-0.3,
                        topbank.elevation+0.3, lwd=2, colour='black')
    ## This draws horizontal lines representing the backwater of each structure
    ## It'll look weird if the structures don't top-to-tail:
    draw.horizontal.lines(distance.to.bottom, distance.to.bottom + 20,
                          thalweg.elevation - 0.3, lwd=2, colour='black',
                          ARROWS=TRUE)
    draw.horizontal.lines(distance.to.bottom, distance.to.bottom + 20,
                          topbank.elevation + 0.3, lwd=2, colour='black',
                          ARROWS=TRUE)
    ### Draw the X axis
    axis(1, at=c(0, tail(distance.to.bottom, 1)), col='#4e4e4e', lty=1,
         lwd=2, labels=F)
    ### Draw the Y axis
    axis(2, at=c(y.bot, y.top), col='#4e4e4e', lty=1, lwd=2, labels=F)
    ### Turn off the SVG device
    dev.off()
    detach(input.data.frame)
}
