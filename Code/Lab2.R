#Lab2###
library(tidyverse)
load("Data/Pb_all.rda")
summary(Pb_all)
##2.A(a)
ggplot(data=Pb_all,aes(year,Pb))+geom_point()
#2.A(b-c)
Pb_lm <- lm(log(Pb)~I(year-1975),data=Pb_all)
Pb_lm_sum <- summary(Pb_lm)
#2.A(d)
Pb_x0 <- data.frame(year=2003)
cbind(Pb_x0,predict(Pb_lm,newdata=Pb_x0,se.fit=TRUE),
      conf = predict(Pb_lm, newdata = Pb_x0,interval = "confidence"),
      pred = predict(Pb_lm, newdata = Pb_x0, interval = "prediction")) |>
  mutate(df=NULL,
         residual.scale=NULL,
         conf.fit=NULL,
         pred.fit=NULL,
         se.pred=sqrt(Pb_lm_sum$sigma^2+se.fit^2)) ->
  Pb_x0_pred
#2.A(e)
Pb_all <- mutate(
  Pb_all,
  yhat=predict(Pb_lm),
  e = Pb_lm$residuals
)
Pb_lim_e <- max(abs(Pb_all$e))*c(-1,1)
ggplot(Pb_all, aes(x=yhat,y=e)) + geom_point() +
         geom_hline(yintercept = 0) +
         expand_limits(y=Pb_lim_e)
ggplot(Pb_all, aes(sample = e)) +
  geom_qq() + geom_qq_line()
ggplot(Pb_all, aes(x = e)) +
  geom_bar(width=1) + scale_x_binned()
#2.A(f)
ggplot(Pb_all, aes(x=yhat,y=e)) + geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y=Pb_lim_e) + facet_wrap(~region)
ggplot(Pb_all, aes(sample = e)) +
  geom_qq() + geom_qq_line()+facet_wrap(~region)
#2.B(a)
ggplot(data=Pb_all,aes(year,Pb))+geom_point()+facet_wrap(~region)
#2.B(b-c)
ggplot(data=Pb_all,aes(year,log(Pb)))+geom_point()+facet_wrap(~region)
#2.B(d)
Pb_all <- mutate(Pb_all,region = relevel(region,"VastraGotaland"))
Pb_lm_mult <- lm(log(Pb)~I(year-1975)+region,data=Pb_all)
Pb_lm_mult_sum <- summary(Pb_lm_mult)
exp(Pb_lm_mult$coefficients)
exp(confint(Pb_lm_mult))
#2.B(e)
Pb_x0 <- data.frame(year=2003,region="VastraGotaland")
cbind(Pb_x0,predict(Pb_lm_mult,newdata=Pb_x0,se.fit=TRUE),
      conf = predict(Pb_lm_mult, newdata = Pb_x0,interval = "confidence"),
      pred = predict(Pb_lm_mult, newdata = Pb_x0, interval = "prediction")) |>
  mutate(df=NULL,
         residual.scale=NULL,
         conf.fit=NULL,
         pred.fit=NULL,
         se.pred=sqrt(Pb_lm_mult_sum$sigma^2+se.fit^2)) ->
  Pb_x0_pred
Pb_x0_pred
exp(Pb_x0_pred[, 3:ncol(Pb_x0_pred)])
#2.B(f)
exp(Pb_lm_mult$coefficients)
exp(confint(Pb_lm_mult))
#2.B(g)
Pb_x0 <- data.frame(year=c(1975,2003), region=rep("Orebro",2))
cbind(Pb_x0,predict(Pb_lm_mult,newdata=Pb_x0,se.fit=TRUE),
      conf = predict(Pb_lm_mult, newdata = Pb_x0,interval = "confidence"),
      pred = predict(Pb_lm_mult, newdata = Pb_x0, interval = "prediction")) |>
  mutate(df=NULL,
         residual.scale=NULL,
         conf.fit=NULL,
         pred.fit=NULL,
         se.pred=sqrt(Pb_lm_mult_sum$sigma^2+se.fit^2)) ->
  Pb_x0_pred
Pb_x0_pred
exp(Pb_x0_pred[, 3:ncol(Pb_x0_pred)])

#2.C(a)
Pb_mult <- lm(log(Pb)~I(year-1975)+region,data=Pb_all)
summary(Pb_mult)

#2.C(b)
anova(Pb_lm,Pb_lm_mult)
#2.C(c)
Pb_seq <- data.frame(year=seq(min(Pb_all$year),max(Pb_all$year)))
Pb_seq |> mutate(
  fit = predict(Pb_lm, newdata=Pb_seq),
  conf = predict(Pb_lm, newdata=Pb_seq, interval = "confidence"),
  pred = predict(Pb_lm, newdata = Pb_seq, interval = "prediction")) ->
  Pb_log_ints
glimpse(Pb_log_ints)
ggplot(Pb_log_ints, aes(x=(year-1975))) +
  geom_point(data = Pb_all, aes(y=log(Pb)))+
  geom_line(aes(y=fit),color="blue",linewidth=1)+
  geom_ribbon(aes(ymin=conf[,"lwr"],ymax=conf[,"upr"]),alpha=0.2)+
  geom_line(aes(y=pred[,"lwr"]),
            color="red",linetype="dashed",linewidth=1) +
  geom_line(aes(y=pred[,"upr"]),
            color="red", linetype="dashed", linewidth=1) + facet_wrap(~region)

#New model
Pb_seq <- expand.grid(
  year = seq(min(Pb_all$year), max(Pb_all$year)),
  region = unique(Pb_all$region)
)
Pb_seq |> mutate(
  fit = predict(Pb_lm_mult, newdata=Pb_seq),
  conf = predict(Pb_lm_mult, newdata=Pb_seq, interval = "confidence"),
  pred = predict(Pb_lm_mult, newdata = Pb_seq, interval = "prediction")) ->
  Pb_log_ints

glimpse(Pb_log_ints)

ggplot(Pb_log_ints, aes(x=(year-1975))) +
  geom_point(data = Pb_all, aes(y=log(Pb)))+
  geom_line(aes(y=fit),color="blue",linewidth=1)+
  geom_ribbon(aes(ymin=conf[,"lwr"],ymax=conf[,"upr"]),alpha=0.2)+
  geom_line(aes(y=pred[,"lwr"]),
            color="red",linetype="dashed",linewidth=1) +
  geom_line(aes(y=pred[,"upr"]),
            color="red", linetype="dashed", linewidth=1) + 
  facet_wrap(~region)
#3.C(d)
Pb_all <- mutate(
  Pb_all,
  yhat=predict(Pb_lm_mult),
  e = Pb_lm_mult$residuals
)
Pb_lim_e <- max(abs(Pb_all$e))*c(-1,1)
ggplot(Pb_all, aes(x=yhat,y=e)) + geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y=Pb_lim_e)
ggplot(Pb_all, aes(sample = e)) +
  geom_qq() + geom_qq_line()
ggplot(Pb_all, aes(x = e)) +
  geom_bar(width=1) + scale_x_binned()
#2.A(f)
ggplot(Pb_all, aes(x=yhat,y=e)) + geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y=Pb_lim_e) + facet_wrap(~region)
ggplot(Pb_all, aes(sample = e)) +
  geom_qq() + geom_qq_line()+facet_wrap(~region)

