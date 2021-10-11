x = seq(1,7,1)
a = 1
b = 0.2
c = 0.5
y1 = a + b*x
y2 = a + b*x +c*x^2 
par(new=TRUE)

plot(x,y1)
plot(x,y2)



B = 1000 # number of simulations
mse = function(y, y.hat) mean((y - y.hat)^2)
k = 12 # maximum degree of the polynomial we fit (model complexity)
ratio = .5 # train / test set split ratio
train.indices = c(rep(TRUE, ratio * n), rep(FALSE, (1 - ratio) * n))
train.errors = matrix(nrow = k, ncol = B)
test.errors = matrix(nrow = k, ncol = B)
for (b in (1:B)) {
  x = runif(n, 0, 6)
  y = target.fun(x) + rnorm(n, 0, 1)
  train.df = data.frame(x = x[train.indices], y = y[train.indices])
  test.df = data.frame(x = x[!train.indices], y = y[!train.indices])
  for (i in (1:k)) {
    fit = lm(y ~ poly(x, i, raw = TRUE), data = train.df)
    train.errors[i, b] = mse(train.df$y, predict(fit))
    test.errors[i, b] = mse(test.df$y, predict(fit, newdata = test.df))
  }
}
p.data = data.frame(k = rep(1:k, 2), error = c(rowMeans(train.errors), rowMeans(test.errors)), 
                    grp = c(rep("train", k), rep("test", k)))
ggplot(data = p.data, mapping = aes(x = factor(k), y = error, group = grp, color = grp)) +
  geom_point(size = 2) + geom_line(alpha = .2) + coord_cartesian(ylim = c(0, 30)) +
  xlab("Model Complexity") + ylab("Prediction Error") + 
  theme(legend.title = element_blank(), legend.position = c(.5, .7))