library(jsonlite)
library(RColorBrewer)
library(jpeg)
library(png)
library(extrafont)
font_import()

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
    parsed_dataframe$skills_name = factor(parsed_dataframe$skills_name,
                                          levels = c("ETL", "Analytics", "Research",
                                              "Mgmt"))
    
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

    ## Layout
    layout(matrix(c(1, 2, 3, rep(4, 9)), nc = 4))
    par(mar = rep(0, 4))

    ## Company icon
    plot.new()
    plot.window(xlim = c(0, 10),
                ylim = c(0, 10))
    rasterImage(readJPEG("../company_icon/deepblu.jpg"), 1.5, 2, 7.5, 8)

    plot.new()
    plot.window(xlim = c(0, 10),
                ylim = c(0, 10))
    rasterImage(readPNG("../company_icon/fao.png"), 2.5, 3, 7.5, 8)

    plot.new()
    plot.window(xlim = c(0, 10),
                ylim = c(0, 10))
    rasterImage(readJPEG("../company_icon/ogilvy.jpg"), 2.5, 2.5, 7.5, 7.5)

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
    axis(3,
         labels = levels(data$skills_name),
         at = as.numeric(unique(data$skills_name)), col = NA,
         cex.axis = 2.5)
}


jpeg(file = "../cv_latex/experience_association.jpeg", width = 660 , height = 600, quality = 100,
     family = "LM Roman 10")

file_path = "../data/association.json"
loaded_data = associationDataParser(file_path)
associationPlot(loaded_data)
graphics.off()
