### Simulate Data

# We KNOW the TRUTH!
mu=5

# Number of observations
n.obs=100

# observed x
x=rnorm(n.obs,mu,sd=7)


#### Estimation
jags.data <- list(x=x, N=n.obs) # data available for estimation

# Pass Model to JAGS
m1 <- jags.model(file="simple_mean.bug", data=jags.data, n.chains=2, n.adapt=2000) 

# Burn in
update(m1, n.iter=500) 

# Run Sampler
m1.out <- coda.samples(model=m1, variable.names=c("mu"), n.iter=5000) 

# visualisation
summary(m1.out)
plot(m1.out)



##################### with sigma....
# Pass Model to JAGS
m2 <- jags.model(file="simple_mean2.bug", data=jags.data, n.chains=2, n.adapt=2000) 

# Burn in
update(m2, n.iter=500) 

# Run Sampler
m2.out <- coda.samples(model=m2, variable.names=c("mu","sigma"), n.iter=5000) 

# visualisation
summary(m2.out)
plot(m2.out)



#### Comparison

m1.compare=glm(x~1)

summary(m1.compare)

