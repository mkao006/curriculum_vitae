library(jsonlite)
library(RColorBrewer)

parseJob <- function(x, name){
    avg_time = x$avg_time
    skills_name = names(x$skills)
    skills_pct = unlist(x$skills)
    data.frame(company = name,
               title = x$title,
               skills_name= skills_name,
               skills_pct = skills_pct,
               avg_time = avg_time)
}

associationDataParser <- function(path){
    associationData = read_json(path)
    parsed_data = mapply(parseJob, associationData, name = names(associationData), SIMPLIFY=FALSE)
    parsed_dataframe = do.call(rbind, parsed_data)
    parsed_dataframe$skills_hour = with(parsed_dataframe, (skills_pct/100) * avg_time)
    rownames(parsed_dataframe) = NULL
    parsed_dataframe$company = factor(parsed_dataframe$company)
    parsed_dataframe$title = factor(parsed_dataframe$title)
    parsed_dataframe$skills_name = factor(parsed_dataframe$skills_name)
    
    parsed_dataframe    
}

associationPlot <- function(data){
    ## Calculate statistics 
    num_jobs = length(unique(data$company))
    num_skills = length(unique(data$skills_name))
    normalised_skills_hour = data$skills_hour/max(data$skills_hour)

    # Determin colors
    discrete_colors = brewer.pal(n = 9, name="Blues")[c(1, 3, 5, 7, 9)]
    continuous_colors = scales::gradient_n_pal(discrete_colors)(normalised_skills_hour)

    par(mfrow = c(1, 2), oma = rep(0, 4))
    par(mar = c(5.1, 4.1, 4.1, 0))
    ## Job Description
    plot.new()
    plot.window(xlim = c(0.5, 5.5),
                ylim = c(0.5, 4.5),
                xaxs = "i", yaxs = "i")
    abline(h = c(1.5, 2.5, 3.5))
    box()
    axis(1)
    axis(2)

    company_margin = 0.1
    unique_companys = rev(levels(data$company))
    for(i in 1:length(unique_companys)){
        text(0.8, num_jobs + 1.5 - i - company_margin, labels = unique_companys[i], adj = c(0, 1))
    }

    title_margin = 0.2
    unique_titles = rev(levels(data$title))
    for(i in 1:length(unique_titles)){
        text(0.8, num_jobs + 1.5 - i - title_margin, labels = unique_titles[i], adj = c(0, 1))
    }


    ## Job Association matrix
    par(mar = c(5.1, 0, 4.1, 2.1))
    with(data, symbols(skills_name,
                       company,
                       circles = skills_hour,
                       bg = continuous_colors,
                       fg = continuous_colors,
                       bty = "n",
                       xlab = "",
                       ylab = "",
                       axes = FALSE
                       ))
    axis(3, labels = levels(data$skills_name), at = as.numeric(unique(data$skills_name)), col = NA)
    ## box(col='white')
    ## box()
    ## axis(1)
    ## axis(2)    
    
}

## file_path = "../data/association.json"
## loaded_data = associationDataParser(file_path)
## associationPlot(loaded_data)
