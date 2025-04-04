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
