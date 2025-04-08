#Lab3###
library(tidyverse)
load("Data/Pb_all.rda")
Pb_all <- mutate(Pb_all, region=relevel(region,"VastraGotaland"))
##3.A(a)
Pb_log <- lm(log(Pb)~I(year-1975),Pb_all)
Pb_bad <- lm(Pb~I(year-1975),Pb_all)
n <- nobs(Pb_log)
log_pred <- mutate(Pb_all,
                   yhat = predict(Pb_log),
                   r = rstudent(Pb_log),
                   v = hatvalues(Pb_log),
                   D = cooks.distance(Pb_log))
bad_pred <- mutate(Pb_all,
                   yhat = predict(Pb_bad),
                   r = rstudent(Pb_bad),
                   v = hatvalues(Pb_bad),
                   D = cooks.distance(Pb_bad))
#good model
pplus1 <- length(Pb_log$coefficients)
ggplot(cbind(log_pred), aes(x = year, y=v)) +
  geom_jitter(width=1) + geom_hline(yintercept=1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red")
#bad model
pplus1b <- length(Pb_bad$coefficients)
ggplot(cbind(bad_pred), aes(x = year, y=v)) +
  geom_jitter(width=1) + geom_hline(yintercept=1/n) +
  geom_hline(yintercept = 2*pplus1b/n, color = "red")

##3.A(b)
Pb_lm <- lm(log(Pb)~I(year-1975)+region, Pb_all)
n <- nobs(Pb_lm)
Pb_pred <- mutate(Pb_all,
                   yhat = predict(Pb_lm),
                   r = rstudent(Pb_lm),
                   v = hatvalues(Pb_lm),
                   D = cooks.distance(Pb_lm))
pplus1 <- length(Pb_lm$coefficients)
ggplot(cbind(Pb_pred), aes(x = year, y=v, color=region)) +
  geom_jitter(width=1) + geom_hline(yintercept=1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red")
##3.A(c)
highlightcolors <- c("|r*|>3"="red")
student_r_plt <- ggplot(Pb_pred, aes(x = yhat, y = r)) +
  geom_point(data = filter(Pb_pred, abs(r) <= 3)) +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  
  geom_jitter(data = filter(Pb_pred, abs(r) > 3),
              aes(color = "|r*|>3"), width = 0, height = 0.05, size = 3) +
  
  labs(title = "Studentized residuals vs predictor",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  theme(legend.position = "bottom")
##3.A(d)
student_r_plt + facet_wrap(~region)
##3.A(e)
ggplot(Pb_pred, aes(x = yhat, y = sqrt(abs(r)))) +
  geom_point() +
  geom_hline(yintercept = c(0, sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = 2) +
  geom_point(data = filter(Pb_pred, abs(r)>3),
             aes(color = "|r*|>3"), size = 3) +
  labs(title = "Sqrt absolute studentized residuals vs predictor",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  theme(legend.position = "bottom") + facet_wrap(~region)
#3.A(f)
f1.Pb <- pplus1
f2.Pb <- Pb_lm$df.residual
cook.limit.Pb <- qf(0.5,f1.Pb,f2.Pb)

ggplot(Pb_pred, aes(yhat, D)) +
  geom_point(size =3) +
  geom_hline(yintercept = 4/n,linetype =2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Pb: Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  scale_color_manual(highlightcolors) + facet_wrap(~region)

#check the influential observation
Pb_pred <- Pb_pred %>%
  mutate(highlight_obs = (D == max(D)))

ggplot(Pb_pred, aes(yhat, D)) +
  geom_point(size = 3) +
  geom_point(data = filter(Pb_pred, highlight_obs),
             aes(x = yhat, y = D), color = "magenta", shape = 21, fill = "magenta", size = 4, stroke = 1.2) +
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Pb: Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)") +
  facet_wrap(~region)

ggplot(Pb_pred, aes(x = year, y = v, color = region)) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  geom_point(data = filter(Pb_pred, highlight_obs),
             aes(x = year, y = v), shape = 21, fill = "magenta", color = "black", size = 4, stroke = 1.2)

ggplot(Pb_pred, aes(x = yhat, y = r)) +
  geom_point(data = filter(Pb_pred, abs(r) <= 3)) +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_jitter(data = filter(Pb_pred, abs(r) > 3),
              aes(color = "|r*|>3"), width = 0, height = 0.05, size = 3) +
  geom_point(data = filter(Pb_pred, highlight_obs),
             aes(x = yhat, y = r), shape = 21, fill = "magenta", color = "black", size = 4, stroke = 1.2) +
  labs(title = "Studentized residuals vs predictor",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  theme(legend.position = "bottom")
#3.A(g)
highlightshapes <- c("Cook's D>0.1" = 24)
dfbetas_values <- dfbetas(Pb_lm)
Pb_pred$dfbetas_year <- dfbetas_values[, "I(year - 1975)"]
ggplot(Pb_pred, aes(x = year, y = dfbetas_year)) +
  geom_point(size = 2) +
  geom_point(data = filter(Pb_pred, abs(residuals(Pb_lm)) > 3),
             aes(color = "|r*|>3"), size = 3) +
  geom_point(data = filter(Pb_pred, cooks.distance(Pb_lm) > cook.limit.Pb),
             aes(shape = "Cook's D>0.1"),
             size = 3) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.Pb) * c(-1, 1),
             color = "red") +
  geom_hline(yintercept = 2 / sqrt(nrow(Pb_pred)) * c(-1, 1),
             color = "red", linetype = "dashed") +
  ylab("DFBETAS for Centered Year") +
  xlab("Fitted Values (yhat)") +
  labs(title = "PB: DFBETAS for Centered Year: Impact on the Model",
       subtitle = "Analysis for Year Variable",
       caption = "Thresholds for Cook's D and Residuals") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes) +
  facet_wrap(~ region)
#3.A(h)
cooks_d <- cooks.distance(Pb_lm)
max_cooks_d_index <- which.max(cooks_d)
highlight_point <- Pb_all[max_cooks_d_index, ]

ggplot(data = Pb_all, aes(x = year, y = Pb)) +
  geom_point() +
  geom_point(data = highlight_point, aes(x = year, y = Pb), color = "red", size = 3) +
  facet_wrap(~ region)
##Lab3.C
Pb_inter <- lm(log(Pb)~I(year-1975)*region, data=Pb_all)
summary(Pb_inter)
anova(Pb_lm,Pb_inter)
Pb_pred2 <- mutate(Pb_all,
                  yhat = predict(Pb_inter),
                  r = rstudent(Pb_inter),
                  v = hatvalues(Pb_inter),
                  D = cooks.distance(Pb_inter))
pplus1 <- length(Pb_inter$coefficients)
ggplot(cbind(Pb_pred2), aes(x = year, y=v, color=region)) +
  geom_jitter(width=1) + geom_hline(yintercept=1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red")
highlightcolors <- c("|r*|>3"="red")
ggplot(Pb_pred2, aes(x = yhat, y = r)) +
  geom_point(data = filter(Pb_pred2, abs(r) <= 3)) +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  
  geom_jitter(data = filter(Pb_pred2, abs(r) > 3),
              aes(color = "|r*|>3"), width = 0, height = 0.05, size = 3) +
  
  labs(title = "Studentized residuals vs predictor",
       color = "Highlight") +
  scale_color_manual(values = highlightcolors) +
  theme(legend.position = "bottom") + facet_wrap(~region)
##3.B(b)
mod1  <- lm(log(Pb)~I(year-1975),Pb_all)
summary(mod1)
summary(Pb_lm)
summary(Pb_inter)
AIC(mod1)
AIC(Pb_lm)
AIC(Pb_inter)
BIC(mod1)
BIC(Pb_lm)
BIC(Pb_inter)
