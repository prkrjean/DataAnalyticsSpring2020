data("USArrests")

states = row.names(USArrests)
states

# apply allows you to apply function to every row
# apply(dataste, 1(row) or 2(column), function)

apply(USArrests, 2, mean)

apply(USArrests, 2, var)

## important to scale (standardize) so that Assult does not take control due 
## to large mean and var

pr.out <- prcomp(USArrests, scale = TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale = 0)

pr.var = pr.out$sdev^2
pr.var

pve <- pr.var/sum(pr.var)
pve

## PCA on IRIS
data('iris')
head(iris)
iristdata1 <- iris[,1:4]
iristdata1
head(iristdata1)

help("princomp")
principal_componenets <- princomp(iristdata1, cor = TRUE, score = TRUE)
summary(principal_componenets)
plot(principal_componenets)
plot(principal_componenets, type = "l")

help("biplot")
biplot(principal_componenets)

## PCA on Boston

install.packages('MASS')
data(Boston, package = 'MASS')
help(Boston)
help(prcompo)
pca_out <- prcomp(Boston, scale. =T)
pca_out
plot(pca_out)

help(biplot)
biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
head(boston_pc)
summary(boston_pc)
