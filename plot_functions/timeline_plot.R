library(jsonlite)
library(RColorBrewer)

parsePeriod <- function(periodData){
    ## NOTE (Michael): Simply modify the 'time' element
    periodData$time = as.Date(unlist(periodData$time))
    periodData
}

parsePoint <- function(pointData){
    pointData$time = as.Date(unlist(pointData$time))
    pointData
}

parseInput <- function(data, type){
    if(type == "period"){
        output = parsePeriod(data)
    } else if(type == "point"){
        output = parsePoint(data)
    } else {
        stop("Unsupported type found")
    }
    output
}

timelineDataParser <- function(path){
    timelineData = read_json(path)
    plotType = names(timelineData)
    mapply(FUN = parseInput, data = timelineData, type = plotType, SIMPLIFY = FALSE)
}

timelineDataValidator <- function(data){
    
}


getDateLimit <- function(data){
    date_limit = as.Date(range(unlist(lapply(data, FUN = function(x) x$time))),
                         origin = "1970-01-01")
    ## NOTE (Michael): Add a bit of room towards the end
    date_limit[2] = date_limit[2] + 60
    date_limit
}

getNumType <- function(data){
    length(unique(unlist(lapply(data, FUN = function(x) x$type))))
}



plotPeriodData <- function(data, barwidth = 0.25, baroffset = 1.625, col){
    ## HACK (Michael): This is a hack
    baroffset = baroffset - data$position * 0.15 + as.numeric(data$position > 0) * barwidth
    ## baroffset = baroffset - data$position 
    y0 = baroffset
    y1 = baroffset - barwidth
    x0 = data$time[1]
    x1 = data$time[2]
    xmean = mean(c(x0, x1))
    ymean = mean(c(y0, y1))
    rect(x0, y1, x1, y0, col = col, border = "white")
    text(xmean, ymean, labels = data$title, col = "grey100")
}

plotPointData <- function(data, midpoint){
    points(data$time, midpoint, cex = 2)
    annotation_height = data$annotation_height * 0.5
    lines(rep(data$time, 2), c(midpoint, midpoint + annotation_height))
    text(data$time, midpoint + annotation_height,
         labels = data$title, adj = 0)
}


timelinePlot <- function(data, bar_width= 0.25){
    xlimit = getDateLimit(data)
    numType = getNumType(data)
    ylimit = c(0, numType + 2)
    ymidpoint = mean(ylimit)

    ## Calculate categories
    categories = sapply(data, FUN = function(x) x$type)
    unique_categories = unique(categories)
    num_categories = length(unique_categories)

    ## Detemine colors
    colors = adjustcolor(brewer.pal(n = 9, name="Blues"), alpha.f = 1)[c(5, 7, 9)]
    color_pallets = as.list(colors)
    names(color_pallets) = unique_categories

    ## Determine offsets
    type_offset = as.list(0:(num_categories - 1))
    names(type_offset) = unique_categories
    
    ## Split the list of data
    plotTypes = names(data)
    periodData = data[which(plotTypes == "period")]
    pointData = data[which(plotTypes == "point")]


    ## Plot the data
    plot.new()
    plot.window(xlim = xlimit, ylim = ylimit)    
    ## box()
    ## axis(1)
    ## axis(2)
    lines(xlimit, rep(ymidpoint, 2), col = "grey")
    points(xlimit[1], ymidpoint, pch = 19)
    year_seq = seq(xlimit[1], xlimit[2], "year")
    points(year_seq, rep(ymidpoint, times = length(year_seq)), pch = 19, cex = 0.5)
    text(xlimit[1] - 50, ymidpoint, format(xlimit[1], "%Y"), srt = 90)
    text(xlimit[2] + 50, ymidpoint, format(xlimit[2], "%Y"), srt = 90)
    
    legend("topleft", legend = unique_categories, bty = "n", col = colors, lwd = 10)


    for(block in periodData){
        current_col = color_pallets[[block$type]]
        plotPeriodData(block, barwidth = bar_width,
                       baroffset = ymidpoint, col = current_col)
    }    
    ## lapply(pointData, plotPointData, midpoint = ymidpoint)
    for(block in pointData){
        plotPointData(block, midpoint = ymidpoint)
    }
}

## file_path = "../data/timeline.json"
## loaded_data = timelineDataParser(file_path)
## timelinePlot(loaded_data, bar_width= 0.1)
