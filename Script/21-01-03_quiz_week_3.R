#load iris dataset
library(datasets)

df <- iris

##q 1
#get the mean length of virignica

tapply(df$Sepal.Length, df$Species, mean)

##q2
# get a vector of means for all species

apply(iris[,1:4], 2, mean)

##q3
# try out the different stuff

with(mtcars, tapply(mpg, cyl, mean))

tapply(mtcars$mpg, mtcars$cyl, mean)

sapply(split(mtcars$mpg, mtcars$cyl), mean)

##q 4
# absolute difference of horsepower of 4 cyl cars with 8 cyl cars

abs(tapply(mtcars$hp, mtcars$cyl, mean)[[1]] - 
      tapply(mtcars$hp, mtcars$cyl, mean)[[3]])

## q5
# debug LS

debug(ls)

ls(p)
n
