# https://cran.r-project.org/web/packages/BART/BART.pdf

library("MASS")
x <- Boston[, c(6, 13)]
y <- Boston$medv
head(cbind(x, y))
par(mfrow = c(2, 2))
plot(x[, 1], y, xlab = "x1=rm", ylab = "y=mdev")
plot(x[, 2], y, xlab = "x2=lstat", ylab = "y=mdev")
plot(x[, 1], x[, 2], xlab = "x1=rm", ylab = "x2=lstat")
par(mfrow = c(1, 1))

library("BART")
set.seed(99)
nd <- 200
burn <- 50
post <- wbart(x, y, nskip = burn, ndpost = nd)

names(post)
length(post$sigma)
length(post$yhat.train.mean)
dim(post$yhat.train)

plot(post$sigma, type = "l")
abline(v = burn, lwd = 2, col = "red")


# linear reg for comparison
lmf <- lm(y~., data.frame(x, y))
fitmat <- cbind(y, post$yhat.train.mean, lmf$fitted.values)
colnames(fitmat) <- c("y", "BART", "Linear")
cor(fitmat)
pairs(fitmat)


# prediction and uncertainty
i <- order(post$yhat.train.mean)
boxplot(post$yhat.train[, i])


# predict on test set
n <- length(y)
set.seed(14)
i <- sample(1:n, floor(0.75 * n))
x.train <- x[i, ]; y.train = y[i]
x.test <- x[-i, ]; y.test = y[-i]
cat("training sample size = ", length(y.train), "\n")
cat("testing sample size = ", length(y.test), "\n")
set.seed(99)
post1 <- wbart(x.train, y.train, x.test)
dim(post1$yhat.test)
length(post1$yhat.test.mean)

# or other way
set.seed(99)
post2 <- wbart(x.train, y.train)
yhat <- predict(post2, x.test)

summary(as.double(yhat - post1$yhat.test))

post3 <- wbart(x.train, y.train, nskip = 1000, ndpost = 10000,
               nkeeptrain = 0, nkeeptest = 0, nkeeptestmean = 0,
               nkeeptreedraws = 200)
yhatthin <- predict(post3, x.test)

fmat <- cbind(y.test, post1$yhat.test.mean, apply(yhatthin, 2, mean))
colnames(fmat) <- c("y", "yhat", "yhatThin")
pairs(fmat)
