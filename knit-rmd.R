library(rmarkdown)

## Set where to save Rmd output files
out_dir <- 'C:/github/DIRT20/Rmd_output'

## Output as html
render("DIRT20_Data_Analysis.Rmd", "html_document",output_dir=out_dir)

## Output as PDF
#render("DIRT20_Data_Analysis.Rmd", "pdf_document",output_dir=out_dir)
