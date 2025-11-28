df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
#df
nll_lm <- function(dataDf, parVec)  {
  y <- dataDf[,1]
  pred <- as.matrix(dataDf[,-1]) # get p's
  matr <- cbind(1,pred) # add intercept b0
  sigma <- parVec[1] 
  beta  <- parVec[2:5] 
  u <- matr %*% beta 
  ei <- y-u
  -sum(dnorm(ei, mean = 0, sd=sigma, log = TRUE))
}   

inits <- c(sd(df$y), mean(df$y), 0, 0, 0 ) 
mod <- optim(par = inits, fn = nll_lm, dataDf = df,method = "L-BFGS-B",
             lower = -Inf, upper= Inf)
# We implemented the negative log likelihood as optim performs minimization, 
# but it will maximize if control$fnscale is negative ~ from help file

y <- df[,1]
pred <- as.matrix(df[,-1])
matr <- cbind(1, pred)
betaHat <- solve(crossprod(matr), crossprod(matr,y))
betaHat
mod$par[-c(1)]
all.equal(betaHat[,1], mod$par[-c(1)])

sqrt(crossprod(y - matr %*% betaHat)/32)
mod$par[1]

modlm <- summary(lm(y ~ matr))
modlm$sigma
all.equal(mod$par[1], modlm$sigma)
# difference as for the optim the maximum likelihood is calculated with degrees
# of freedom n while lm uses n-p 

fitAR1 <- optim(par = inits, fn = nll_lm, dataDf = df,method = "L-BFGS-B",
                lower = -Inf, upper= Inf, hessian = TRUE)

ep1 <- sqrt(diag(solve(fitAR1$hessian)))
ep1

# Added the coefficients and residuals 
modlm$coefficients
modlm$residuals
     