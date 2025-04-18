#Lab1####
library(tidyverse)
load("Data/Pb_all.rda")
summary("Pb_all.rda")
Pb_myregion <- filter(Pb_all, region=="VastraGotaland")
summary(Pb_myregion)
##1.A  Linear model####
###1.A(a)####
ggplot(data=Pb_myregion,aes((year),Pb))+geom_point()
###1.A(b)#####
ggplot(data=Pb_myregion,aes((year-1975),Pb))+geom_point()
##1.A(c-e)####
Pb_lm <- lm(Pb~I(year-1975), data=Pb_myregion)
summary(Pb_lm)
confint(Pb_lm)
##1.A(f-g)####
Pb_x0 <- data.frame(year=2003)
Pb_lm_sum <- summary(Pb_lm)
cbind(Pb_x0,
      predict(Pb_lm, newdata=Pb_x0, se.fit=TRUE),
      conf = predict(Pb_lm, newdata=Pb_x0,
                     interval = "confidence"),
      pred = predict(Pb_lm, newdata=Pb_x0,
                     interval="prediction")) |>
  mutate(df=NULL,
         residual.scale=NULL,
         conf.fit=NULL,
         pred.fit=NULL,
         se.pred=sqrt(Pb_lm_sum$sigma^2+se.fit^2)) ->
  Pb_x0_pred
round(Pb_x0_pred,digits=3)
##1.A(h)
Pb_seq <- data.frame(year=seq(min(Pb_myregion$year),max(Pb_myregion$year)))

Pb_seq |> mutate(
  fit = predict(Pb_lm, newdata=Pb_seq),
  conf = predict(Pb_lm, newdata=Pb_seq, interval = "confidence"),
  pred = predict(Pb_lm, newdata = Pb_seq, interval = "prediction")) ->
  Pb_lm_ints
glimpse(Pb_lm_ints)

ggplot(Pb_lm_ints, aes(x=(year-1975))) +
  geom_point(data = Pb_myregion, aes(y=Pb))+
  geom_line(aes(y=fit),color="blue",linewidth=1)+
  geom_ribbon(aes(ymin=conf[,"lwr"],ymax=conf[,"upr"]),alpha=0.2)+
  geom_line(aes(y=pred[,"lwr"]),
            color="red",linetype="dashed",linewidth=1) +
  geom_line(aes(y=pred[,"upr"]),
            color="red", linetype="dashed", linewidth=1)

##1.A(i)
Pb_myregion <- mutate(Pb_myregion,
                      yhat = predict(Pb_lm),
                      e = Pb_lm$residuals)
Pb_lim_e <- max(abs(Pb_myregion$e))*c(-1,1)
ggplot(Pb_myregion, aes(x = yhat, y = e)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y=Pb_lim_e) + geom_smooth()
##1.A(j)
ggplot(Pb_myregion, aes(sample = e)) +
  geom_qq() + geom_qq_line()

ggplot(Pb_myregion, aes(x = e)) +
  geom_bar(width=1) + scale_x_binned()
##1.B(a-b)
Pb_myregion <- filter(Pb_all, region=="VastraGotaland") #Reset region
ggplot(data=Pb_myregion,aes((year-1975),log(Pb)))+geom_point()
##1.B(c-e)
loglin_model <- lm(log(Pb)~I(year-1975),Pb_myregion)
summary(loglin_model)
confint(loglin_model)
##1.B(f-g)
new_pb <- data.frame(year=2003)
pb_log_sum <- summary(loglin_model)
cbind(new_pb,
      predict(loglin_model, newdata=new_pb, se.fit=TRUE),
      conf = predict(loglin_model, newdata=new_pb,
                     interval = "confidence"),
      pred = predict(loglin_model, newdata=new_pb,
                     interval="prediction")) |>
  mutate(df=NULL,
         residual.scale=NULL,
         conf.fit=NULL,
         pred.fit=NULL,
         se.pred=sqrt(pb_log_sum$sigma^2+se.fit^2)) ->
  new_pb_pred
round(new_pb_pred,digits=3)
##1.B(h)
Pb_seq <- data.frame(year=seq(min(Pb_myregion$year),max(Pb_myregion$year)))
Pb_seq |> mutate(
  fit = predict(loglin_model, newdata=Pb_seq),
  conf = predict(loglin_model, newdata=Pb_seq, interval = "confidence"),
  pred = predict(loglin_model, newdata = Pb_seq, interval = "prediction")) ->
  Pb_log_ints
glimpse(Pb_log_ints)
ggplot(Pb_log_ints, aes(x=(year-1975))) +
  geom_point(data = Pb_myregion, aes(y=log(Pb)))+
  geom_line(aes(y=fit),color="blue",linewidth=1)+
  geom_ribbon(aes(ymin=conf[,"lwr"],ymax=conf[,"upr"]),alpha=0.2)+
  geom_line(aes(y=pred[,"lwr"]),
            color="red",linetype="dashed",linewidth=1) +
  geom_line(aes(y=pred[,"upr"]),
            color="red", linetype="dashed", linewidth=1)

ggplot(Pb_log_ints, aes(x = (year-1975))) +
  geom_point(data=Pb_myregion, aes(y=Pb)) +
  geom_line(aes(y=exp(fit)), color="blue", linewidth=1) +
  geom_ribbon(aes(ymin=exp(conf[,"lwr"]),ymax=exp(conf[,"upr"])), alpha=0.2) +
  geom_line(aes(y=exp(pred[,"lwr"])),
            color="red", linetype="dashed", linewidth=1) +
  geom_line(aes(y=exp(pred[,"upr"])),
            color="red", linetype="dashed", linewidth=1)
Pb_myregion <- mutate(Pb_myregion,
                      yhat_log = predict(loglin_model),
                      e_log = loglin_model$residuals)
Pb_lim_elog <- max(abs(Pb_myregion$e_log))*c(-1,1)
##1.B(i)
ggplot(Pb_myregion, aes(x = (year-1975), y = e_log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y=Pb_lim_elog)
ggplot(Pb_myregion, aes(x = yhat_log, y = e_log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  expand_limits(y=Pb_lim_elog)
ggplot(Pb_myregion, aes(sample = e_log)) +
  geom_qq() + geom_qq_line()
ggplot(Pb_myregion, aes(x = e_log)) +
  geom_bar(width=1) + scale_x_binned()
##C does not need new code

