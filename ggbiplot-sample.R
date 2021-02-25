# https://www.datacamp.com/community/tutorials/pca-analysis-r

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
data(mtcars)
mtcars.pca=prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)
summary(mtcars.pca)
str(mtcars.pca)
ggbiplot(mtcars.pca)
ggbiplot(mtcars.pca, labels=rownames(mtcars))
mtcars.country = c(rep("Japan", 3), rep("US",4), rep("Europe", 7),
                   rep("US",3), "Europe", rep("Japan", 3), rep("US",4),
                   rep("Europe", 3), "US", rep("Europe", 3))
ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)
ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4),labels=rownames(mtcars), groups=mtcars.country)
ggbiplot(mtcars.pca,ellipse=TRUE,circle=TRUE, labels=rownames(mtcars), groups=mtcars.country)
ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country)
