library(jsonlite)
library(RColorBrewer)


file_path = "../data/radar.json"
loaded_data = read_json(file_path)

polar2cartesian <- function(r, theta){
    x = r * cos(theta)
    y = r * sin(theta)
    list(x = x, y = y)
}

draw_polygon <- function(slice, n_slice, levels, n_levels, skill){
    angle = 2 * pi/n_slice
    colors = brewer.pal(n = 9, name="Blues")[c(1, 3, 5, 7, 9)]
    for(i in 1:levels){
        r = c((i - 1)/n_levels, i/n_levels, i/n_levels, (i - 1)/n_levels)
        theta = c((slice - 1) * angle, (slice - 1) * angle, (slice) * angle, (slice) * angle)
        with(polar2cartesian(r = r, theta = theta), polygon(x = x, y = y, col = colors[i]))
    }
    with(polar2cartesian(r = 1.2, theta = (slice - 0.5) * angle),
         text(x, y, labels = skill))
        
}

radar_plot <- function(){
}


n_skills = length(loaded_data)
skills_name = names(loaded_data)



plot.new()
plot.window(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
for(i in 1:6){
    draw_polygon(i, 6, sample(5, 1), 5, 'Test')
}
