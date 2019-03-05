randomX <- as.data.frame(rnorm(300))
randomY <- as.data.frame(rnorm(300))
dataframe <- cbind(randomX, randomY)

names(dataframe) <- c('ejeX', 'ejeY')

ggplot(dataframe) + geom_point(aes(ejeX, ejeY))

cor(dataframe$ejeX, dataframe$ejeY)


points <- list(x=vector("numeric", 0), y=vector("numeric", 0))
points$x <- c(points$x, 5)
points$y <- c(points$y, 6)


points <- as.data.frame(points)
points <- points[1,]
points$x <- 0
points$y <- 0

pint

medX <- median(dataframe[,1])
medY <- median(dataframe[,2])
