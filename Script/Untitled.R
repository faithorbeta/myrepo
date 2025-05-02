library(lme4)
install.packages('lmerTest')
library(lmerTest)
install.packages('MuMIn')
library(MuMIn)

g <- rbinom(100,1,0.5)
x <- rnorm(100)
y <- x + 2 * g + rnorm(100)
plot(x,y,col=hsv(h=g / 1.5))
# 1 | g means apply the offset everywhere
m <- lmer(y ~ x + (1 | g))
summary(m)
ranef(m)
rand(m)
# R2m (marginal r2) is for fixed effects
# R2c (conditional r2) is everything combined
r.squaredGLMM(m)
