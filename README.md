# CV Generation Project

This directory contains a project for generating a curriculum vitae (CV) and a supplementary document using R Markdown.

## Directory Structure

- `cv.Rmd`: The main R Markdown file for generating the CV in HTML format. It includes personal info, a bio, a skills radar chart, a timeline of experience, and an association matrix of skills and jobs.
- `simplicity.Rmd`: Another R Markdown file that generates a PDF document explaining the author's work philosophy using a LASSO equation analogy.
- `data/`: A directory containing JSON files (`association.json`, `radar.json`, `timeline.json`) that provide the data for the visualizations in the CV.
- `plot_functions/`: A directory containing R scripts (`association_plot.R`, `radar_plot.R`, `timeline_plot.R`) that define functions to create the plots used in `cv.Rmd`.
- `company_icon/`: A directory containing company logos used in the association plot.
- `cv_latex/`: A directory that likely contains a LaTeX version of the CV or assets for it.

## How to Use

To generate the CV and the supplementary document, you will need to have R and the necessary packages installed. You can then run the following command in your terminal:

```bash
Rscript -e "rmarkdown::render('cv.Rmd')"
Rscript -e "rmarkdown::render('simplicity.Rmd')"
```

This will generate `cv.html` and `simplicity.pdf` in the root of the directory.
