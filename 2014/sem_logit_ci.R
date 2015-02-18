library("erer")
df <- data.frame(x=c(2,1,3,0,3),
                 y=c(1,0,1,0,0))
df
model <- glm(data=df, y~x, 
             family=binomial(link="logit"))
summary(model)

model$coefficients
vcov(model)

new <- data.frame(x=c(2,4))
prognoz <- predict(model, newdata = new)
plogis(prognoz)
prognoz2 <- predict(model, newdata = new, 
                   type = "response")
prognoz2

V <- vcov(model)

# x=2 
sqrt(V[1,1]+2^2*V[2,2]+2*2*V[1,2])
prognoz <- predict(model, newdata = new, 
                   se.fit = TRUE)
prognoz

prog_df <- as.data.frame(prognoz)
prog_df$x <- c(2,4)
prog_df

library("dplyr")
prog_df <- mutate(prog_df, 
                  left_ci = fit - 1.96*se.fit,
                  right_ci = fit + 1.96*se.fit)

prog_df

qnorm(0.975)
prog_df <- mutate(prog_df, 
                  left_ci_p=plogis(left_ci),
                  right_ci_pi=plogis(right_ci),
                  p_hat = plogis(fit))
prog_df <- mutate(prog_df,
                  se_p=se.fit*dlogis(fit))
prog_df <- mutate(prog_df, 
            left_ci_p_red=p_hat-1.96*se_p,
            rigth_ci_p_red=p_hat+1.96*se_p)
prog_df

prognoz3 <- predict(model, newdata = new, 
                   se.fit = TRUE, type="response")
prognoz3
confint(model)

model <- glm(data=df, y~x, 
             family=binomial(link="logit"),
              x=TRUE)
maBina(model)
maBina(model, x.mean = FALSE)
