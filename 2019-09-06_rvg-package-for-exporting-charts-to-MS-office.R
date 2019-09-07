

library(rvg)
library(ggplot2)
library(officer)

doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_vg(doc, code = barplot(sample(1:20,10),xlab="Day",ylab="Widgets"),
                  type = "body")
print(doc, target = "my_plot.pptx")
