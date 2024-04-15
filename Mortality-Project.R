#Importing csv file----

chilean_mortality <- read.csv("ChileanMortality.csv", header = TRUE)


#Installing and loading packages

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("lubridate")
library(lubridate)

install.packages('knitr')
library(knitr)

install.packages('tidyverse')
library(tidyverse)

install.packages('tidyr')
library(tidyr)

#Data cleaning----

mortality <- chilean_mortality
colSums(is.na(mortality))


#Converting character objects to date objects

mortality$DATE_START <-as.Date(mortality$DATE_START)
mortality$DATE_END <- as.Date(mortality$DATE_END)
mortality$BIRTHDATE <- as.Date(mortality$BIRTHDATE)

#Converting gender into a binary variable

mortality <- mortality %>% 
  mutate(male = ifelse(SEX == "M",1, 0),
         female = ifelse(SEX == "F",1,0))

#Creating binary variable for death

mortality <- mortality %>%
  mutate(delta = ifelse(DEATH == "TRUE", 1, 0))

#Converting health and person_type to factor variables

mortality$HEALTH <- as.factor(mortality$HEALTH)
mortality$PERSON_TYPE <- as.factor(mortality$PERSON_TYPE)

#Calculating exact age, and duration of policy under the observation period

mortality$AGE_START <- floor(time_length(difftime(mortality$DATE_START, mortality$BIRTHDATE), "years"))

mortality$AGE_END <- floor(time_length(difftime(mortality$DATE_END, mortality$BIRTHDATE), "years"))

mortality$AGE_START_EXACT <- time_length(difftime(mortality$DATE_START, mortality$BIRTHDATE), "years")

mortality$AGE_END_EXACT <- time_length(difftime(mortality$DATE_END, mortality$BIRTHDATE), "years")


summary(mortality)

#Removing the observation of the 130 year old person, assuming that it is wrongly recorded

mortality_2 <- mortality 

mortality_2 <- mortality_2[mortality_2$AGE_START!=max(mortality_2$AGE_START),]

max(mortality_2$AGE_END)

max(mortality_2$AGE_START)


#Data analysis----

#Graphs

#Age histogram

age_stat <- mortality_2 %>%
  summarize(mean_age = mean(AGE_START))
age_stat

Age <- mortality_2 %>% filter(DEATH == "TRUE") %>% 
  group_by(AGE_START) %>% summarise(count = n()) 

Age_mode_start <- mortality_2 %>% 
  group_by(AGE_START) %>% summarise(count = n()) %>% arrange(desc(count))

Age_dist <- ggplot(mortality_2, aes(x = AGE_START)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Age", x = "Age at Start", y = "Density of Annuitants' Age") +
  geom_vline(aes(xintercept = mean_age), age_stat, color = "red", lwd = 2) + 
  geom_line(stat = "density")

print(Age_dist)

Age_mode <- mortality_2 %>% filter(DEATH == "TRUE") %>% 
  group_by(AGE_END) %>% summarise(count = n()) %>% arrange(desc(count))

age_table <- head(Age_mode, 10000)
age_table %>% summarise(mean = mean(AGE_END))

age_end <- mortality_2 %>% filter(DEATH == "TRUE") %>%
  summarize(mean_age_end = mean(AGE_END))
age_end

kable(age_table)

''
#Age box chart

age_start_alive <- mortality_2 %>% filter(DEATH == "FALSE") %>%
  summarize(mean_age_start = mean(AGE_START))
age_start_alive

age_start_dead <- mortality_2 %>% filter(DEATH == "TRUE") %>%
  summarize(mean_age_start = mean(AGE_START))
age_start_dead


health_boxplot <- ggplot(data = mortality_2, mapping = aes(x = DEATH , y = AGE_START)) + 
  geom_boxplot(fill = c("lightgreen", "coral2"), color = c("darkblue", "darkblue")) +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "black") + coord_flip() +
  labs(title = "Deaths at varying age")

print(health_boxplot)

health_gender <- mortality_2 %>%
  group_by(HEALTH, SEX) %>% 
  summarise(n(), n_death = sum(DEATH), death_mean = mean(DEATH))

health_gender

#Health status bar chart

HealthStatus <- mortality_2 %>%
  filter(DEATH == "TRUE") %>%
  ggplot() + geom_bar(mapping = aes(x = HEALTH, colour = SEX)) +
  labs(x = "Health Status",
       y = "Frequency") + ggtitle("Deaths by Health Status and Gender")

print(HealthStatus)

health_mortality <- mortality_2 %>% select(SEX, HEALTH) %>%
  filter(HEALTH == "Disabled") %>% group_by(SEX) %>%
  summarise(count = n())

#Pie charts

Data_by_sex <- mortality_2 %>% group_by(SEX) %>% 
  summarise(Countbysex = n()) %>%
  mutate(percentage = Countbysex/nrow(mortality_2))


Death_by_sex <- mortality_2 %>% group_by(SEX) %>% filter(DEATH == "TRUE") %>% 
  summarise(Countbysex = n()) %>% 
  mutate(percentage = Countbysex/nrow(mortality_2))

death_gender_pie <- ggplot(Death_by_sex, aes(x = "", y = Countbysex, fill = SEX)) +
  geom_bar( stat = "identity") + coord_polar("y", start = 0) + theme_minimal() +
  scale_fill_manual(values = c("darkorange","lightblue", "purple")) + 
  ggtitle("Number of deaths by gender")

print(death_gender_pie)

gender_pie <- ggplot(Data_by_sex, aes(x = "", y = Countbysex, fill = SEX)) +
  geom_bar( stat = "identity") + coord_polar("y", start = 0) + theme_minimal() +
  scale_fill_manual(values = c("darkorange","lightblue", "purple")) + 
  ggtitle("Annuitants by Sex")

print(gender_pie)

#Additional Summary Statistics

data_by_person <- mortality_2 %>%
  group_by(PERSON_TYPE) %>%
  summarise(n(), no_of_death = sum(DEATH), death_mean = mean(DEATH))

data_by_health <- mortality_2 %>%
  group_by(HEALTH) %>%
  summarise(n(), no_of_death = sum(DEATH), death_mean = mean(DEATH))

data_by_health <- mortality_2 %>%
  group_by(HEALTH) %>%
  group_by(SEX) %>%
  summarise(n(), no_of_death = sum(DEATH), death_mean = mean(DEATH))



summary(mortality_2)

#Survival analysis----

#Installing packages

install.packages('KMsurv')
library(KMsurv)

install.packages('survival')
library(survival)

#Kaplan-Meier----

mortality_2 <- mortality_2 %>% mutate(investigation = as.numeric(DATE_END - DATE_START))

mortality_2$Investigation_years <- mortality_2$investigation/365.25

reference_year <- 60

mortality_2$Left_truncation_time <- as.numeric(difftime(mortality_2$AGE_START_EXACT, reference_year
                                                        ))

mortality_2$Right_censoring_time <- as.numeric((difftime(mortality_2$AGE_END_EXACT, reference_year
                                                         )))

mortality_3 <- mortality_2 %>% filter(mortality_2$Left_truncation_time != mortality_2$Right_censoring_time)

mortality_3 <- mortality_3 %>%
  mutate(health = ifelse(HEALTH == "Healthy",1,0))

mortality_3 <- mortality_3 %>%
  mutate(gender = ifelse(SEX == "F",1,0))

mortality_3 <- mortality_3 %>%
  mutate(person = ifelse(PERSON_TYPE == "Main Annuitant",1,0))



surv_obj <- Surv(mortality_3$Left_truncation_time, mortality_3$Right_censoring_time, 
                 event = mortality_3$DEATH)

summary(surv_obj)

#Kaplan-Meier Curves----

#Sex

KM_fit_sex <- survfit(surv_obj ~ SEX, data = mortality_3, type="kaplan-meier")
summary(KM_fit_sex)

plot(KM_fit_sex, xlab = "Time since age 60 (years)", ylab = "S(x)", main = "KM estimate of males and females",
     col = c("red", "blue"))
legend("topright", legend = c("Females", "Males"), fill = c("red", "blue"))

#KM of males that are healthy/disabled

male_mortality <- mortality_3 %>% filter(SEX == "M")

male_mortality$Left_truncation_time <- as.numeric(difftime(male_mortality$AGE_START_EXACT, reference_year
))

male_mortality$Right_censoring_time <- as.numeric((difftime(male_mortality$AGE_END_EXACT, reference_year
)))

male_mortality <- male_mortality %>% filter(male_mortality$Left_truncation_time != male_mortality$Right_censoring_time)

male_surv_obj <- Surv(male_mortality$Left_truncation_time, male_mortality$Right_censoring_time, 
                      event = male_mortality$DEATH)

KM_fit_male_health <- survfit(male_surv_obj ~ HEALTH, data = male_mortality, type = "kaplan-meier")
plot(KM_fit_male_health, xlab = "Time since age 60 (years)", ylab = "S(x)", 
     main = "KM estimate of males that are healthy or disabled",
     col = c("red", "blue"))
legend("topright", legend = c("Healthy", "Disabled"), fill = c("blue", "red"))

summary(male_surv_obj)

#KM of females that are annuitants/beneficiaries

female_mortality <- mortality_3 %>% filter(SEX == "F" & HEALTH == "Healthy")

female_mortality$Left_truncation_time <- as.numeric(difftime(female_mortality$AGE_START_EXACT, reference_year
))

female_mortality$Right_censoring_time <- as.numeric((difftime(female_mortality$AGE_END_EXACT, reference_year
)))

female_mortality <- female_mortality %>% filter(female_mortality$Left_truncation_time != female_mortality$Right_censoring_time)

female_surv_obj <- Surv(female_mortality$Left_truncation_time, female_mortality$Right_censoring_time,
                        event = female_mortality$DEATH)

KM_fit_female_annuitant <- survfit(female_surv_obj ~ PERSON_TYPE, data = female_mortality, type = "kaplan-meier")
plot(KM_fit_female_annuitant, xlab = "Time since age 60 (years)", ylab = "S(x)", 
     main = "KM estimate of females that are main annuitant or beneficiary",
     col = c("red", "blue"))
legend("topright", legend = c("Main Annuitant", "Beneficiary"), fill = c("blue", "red"))

summary(female_surv_obj)

#Death

KM_fit <- survfit(surv_obj ~ 1, conf.int = 0.95, conf.type = "log")
                 
summary(KM_fit)

plot(KM_fit,  main = "KM estimate with 95% confidence intervals",
     ylab = "S(x)",
     xlab = "Time since age 60 (years)")


#Health status

KM_fit_health <- survfit(surv_obj ~ HEALTH, data = mortality_3, type="kaplan-meier")

summary(KM_fit_health)

plot(KM_fit_health,  xlab = "Time since age 60 (years)", ylab = "S(x)", 
     main = "KM estimate of people with 'healthy' or 'disabled' status",
     col = c("blue", "red"))
legend("topright", legend = c("Healthy", "Disabled"), fill = c("red", "blue"))


#Person type

KM_fit_person <- survfit(surv_obj ~ PERSON_TYPE, data = mortality_3, type="kaplan-meier")

summary(KM_fit_person)

plot(KM_fit_person,  xlab = "Time (years)", ylab = "S(x)", 
     main = "KM estimate of annuitant type",
     col = c("red", "blue"))
legend("topright", legend = c("Main Annuitant", "Beneficiary"), fill = c("red", "blue"))


#Cox regression

cox.reg <- coxph(Surv(mortality_3$Left_truncation_time, mortality_3$Right_censoring_time, 
                      event = mortality_3$DEATH) ~ as.factor(mortality_3$gender) + 
                   as.factor(mortality_3$health) +
                   as.factor(mortality_3$person) +
                   mortality_3$AGE_START,
                 method = "breslow")

summary(cox.reg)


cox.reg.male <- coxph(male_surv_obj ~ 
                        as.factor(male_mortality$health) +
                        as.factor(male_mortality$person) +
                        male_mortality$AGE_START,
                      method = "breslow")

summary(cox.reg.male)

cox.reg.female <- coxph(female_surv_obj ~ 
                          as.factor(female_mortality$person),
                        method = "breslow")

summary(cox.reg.female)

#Graduation----

#Installing Packages

install.packages("demography", dependencies = TRUE)
library(demography)

#Computing dx

mortality_4 <- mortality_3 %>% filter(HEALTH == "Healthy")

mortality_4 <- mortality_4 %>%
  group_by(DATE_END) %>%
  mutate(year = first(year(DATE_END))) %>%
  ungroup

mortality_4 <- mortality_4 %>% mutate(YEAR_END = format(as.Date(mortality_4$DATE_END, format="%d/%m/%Y"),"%Y"))

mortality_4 <- mortality_4 %>% mutate(YEAR_START = format(as.Date(mortality_4$DATE_START, format="%d/%m/%Y"),"%Y"))

Death_by_year <- mortality_4 %>% group_by(YEAR_END) %>% filter(DEATH == "TRUE") %>% 
  summarise(Countbyyear = n())
 
dx <- mortality_4 %>% group_by(AGE_END) %>% filter(DEATH == "TRUE") %>%
  summarise(Deathbyyear = n())

dx <- dx %>% filter(AGE_END <= 100)

sum(dx$Deathbyyear)

#Computing Ex

# Function to calculate exposed to risk for vectors of birthdate, date_start, and date_end
calculate_exposed_to_risk <- function(birthdate, date_start, date_end) {
  # Convert dates to Date objects
  birthdate <- as.Date(birthdate)
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)
  
  # Calculate age at start and end dates
  age_start <- as.numeric(difftime(date_start, birthdate, units = "weeks") / 52.1775)
  age_end <- as.numeric(difftime(date_end, birthdate, units = "weeks") / 52.1775)
  
  # Initialization
  ages <- list()
  exposures <- list()
  
  # Determine contributions for each age
  for (i in seq_along(birthdate)) {
    ages[[i]] <- numeric()
    exposures[[i]] <- numeric()
    
    for (age in seq(ceiling(age_start[i]), ceiling(age_end[i]))) {
      if (age == ceiling(age_start[i]) & age == ceiling(age_end[i])) {
        ages[[i]] <- c(ages[[i]], age)
        exposures[[i]] <- c(exposures[[i]], as.numeric(difftime(date_end[i], date_start[i], units = "weeks") / 52.1775))
      } else if (age == ceiling(age_start[i])) {
        ages[[i]] <- c(ages[[i]], age)
        exposures[[i]] <- c(exposures[[i]], 1 - (age_start[i] - floor(age_start[i])))
      } else if (age == ceiling(age_end[i])) {
        ages[[i]] <- c(ages[[i]], age)
        exposures[[i]] <- c(exposures[[i]], age_end[i] - floor(age_end[i]))
      } else {
        ages[[i]] <- c(ages[[i]], age)
        exposures[[i]] <- c(exposures[[i]], 1)
      }
    }
  }
  
  
  result_df <- lapply(seq_along(ages), function(i) {
    data.frame(age = ages[[i]] - 1, exposure = exposures[[i]])
  })
  

  return(result_df)
}

mortality_5 <- mortality_5 %>%
  rowwise() %>%
  mutate(exposed_to_risk = (calculate_exposed_to_risk(BIRTHDATE, DATE_START, DATE_END))) %>%
  ungroup()

exposure_table <- mortality_5 %>%
  unnest(exposed_to_risk) %>%
  select(age, exposure)

exposure_table <- exposure_table %>% group_by(age) %>% 
  mutate(central_exposure = sum(exposure)) %>% 
  unique() %>% 
  arrange(age)

total_exposure <- exposure_table %>% select(age, central_exposure) %>%
  unique() %>%
  arrange(age) %>% filter(age <= 100)

graduation_table <- cbind(total_exposure, dx) %>%
  select(-AGE_END) %>%
  mutate(Mx = Deathbyyear/central_exposure)

graduation_table <- graduation_table %>%
  mutate(initial_exposure = central_exposure + (1/2)*Deathbyyear) %>%
  mutate(px = exp(-Mx)) %>%
  mutate(qx = 1-px)

plot(graduation_table$age, graduation_table$Mx, type = "l")

#Parametric models----

x <- 60:100
Dx <- graduation_table$Deathbyyear
mx <- graduation_table$Mx
Ex <- graduation_table$central_exposure

#Gompertz

gompertz <- nls(mx ~ exp(b0 + b1*x), start = list(b0 = 1, b1 = 0), weights = Ex/mx)
gompertz

mx_gompertz <- fitted(gompertz)
plot(x, log(mx), main = "Gompertz", pch = 20)
lines(x, log(mx_gompertz), col = "darkblue")

#Makeham

makeham <- nls(mx ~ A + exp(b0 + b1*x), start = list(A = 0, b0 = coef(gompertz)[1], b1 = coef(gompertz)[2]),
               weights = Ex/mx)
makeham

mx_makeham <- fitted(makeham)
plot(x, log(mx), main = "Makeham", pch = 20)
lines(x, log(mx_makeham), col = "red")

#Smooth spline

spline <- smooth.spline(x, mx, cv=T)
smooth_para <- spline$spar

smSpline <- smooth.spline(x, mx, spar = smooth_para)

mx_smSpline <- fitted(smSpline)

plot(x, log(mx), main = "Smoothing Spline", pch = 20)
lines(x, log(mx_smSpline), col = "green")

#Cubic spline

install.packages('splines')
library(splines)

knots <- c(61, 67, 78, 95, 97)
cubic_basis <- ns(x, knots = knots)
matplot(x, cubic_basis, type = "l", xlab = "age (x)", ylab = "phi_i(x)",
        main = "Cubic B-spline basis")

cubSpline  <- lm(mx ~ cubic_basis, weights = Ex / mx )
cubSpline

mx_cubSpline <- fitted(cubSpline)
plot(x, log(mx), xlab = "Age", ylab = "Central mortality rate (log scale)", 
     main = "Chilean Pensioners: Cubic Spline", pch = 20)
lines(x, log(mx_cubSpline), col = 'blue')




#Statistical tests----

#Computing test statistics

zx_makeham <- (Dx - Ex * mx_makeham) / sqrt(Ex * mx_makeham)
zx_gompertz <- (Dx - Ex * mx_gompertz) / sqrt(Ex * mx_gompertz)
zx_smSpline <- (Dx - Ex * mx_smSpline) / sqrt(Ex * mx_smSpline)
zx_cubSpline <- (Dx - Ex * mx_cubSpline) / sqrt(Ex * mx_cubSpline)



#Chi-square test

chi2Test <- function(O, E, npar, alpha = 0.05){
  chi2 <- sum((O - E)^2 / E) #Test statistic
  df <- length(O) - npar
  chi2_alpha <- qchisq(1 - alpha, df) #Critical value
  p.value <- 1 - pchisq(chi2, df) #p.value
  list(statistic = chi2, c.value = chi2_alpha, df = df, p.value = p.value)
}

#Gompertz

length(coef(gompertz))

chi2Test(Dx, Ex * mx_gompertz, 2)

#Makeham

length(coef(makeham))

chi2Test(Dx, Ex * mx_makeham, 3)

#Smooth spline

chi2Test(Dx, Ex * mx_smSpline, smSpline$df)

#Cubic spline

chi2Test(Dx, Ex * mx_cubSpline, cubSpline$rank)



#Standardized deviations test

stdTest <- function(zx, breaks = c(-Inf, -1, 0, 1, Inf)){
  observed <- table(cut(zx, breaks)) #count observation in each interval
  expected.p <- diff(pnorm(breaks)) #expected probabilities for standard normal
  chisq.test(observed, p = expected.p) #apply chisquare test
}

stdTest_gompertz <- stdTest(zx_gompertz)
stdTest_makeham <- stdTest(zx_makeham)
stdTest_smSpline <- stdTest(zx_smSpline)
stdTest_cubSpline <- stdTest(zx_cubSpline)

stdTest_gompertz
stdTest_makeham 
stdTest_smSpline 
stdTest_cubSpline 

#histogram and qq-plot

hist(zx_smSpline)
qqnorm(zx_smSpline, main = "Normal Q-Q Plot for Smoothing Spline")
qqline(zx_smSpline, col = "red", lwd = 2)

hist(zx_cubSpline)
qqnorm(zx_cubSpline, main = "Normal Q-Q Plot for Cubic Spline")
qqline(zx_cubSpline, col = "blue", lwd = 2)

#Signs test

number_ages <- length(x)
signTest_gompertz <- binom.test(sum(zx_gompertz > 0), number_ages)
signTest_makeham <- binom.test(sum(zx_makeham > 0), number_ages)
signTest_smSpline <- binom.test(sum(zx_smSpline > 0), number_ages)
signTest_cubSpline <- binom.test(sum(zx_cubSpline > 0), number_ages)


signTest_makeham
signTest_gompertz
signTest_smSpline$p.value
signTest_cubSpline$p.value



#Cumulative deviations test

cumDevTest <- function(A, E, alpha = 0.05){
  cumDev <- sum(A - E) / sqrt(sum(E)) #Test statistic
  z_alpha <- qnorm(1 - alpha/2) #Critical value
  p.value <- 2 *(1 - pnorm(cumDev, lower.tail=(cumDev > 0))) #p.value (Note it is two-tailed)
  list(statistic = cumDev, c.value = z_alpha, p.value = p.value)
}


cumDevTest_gompertz <- cumDevTest(Dx, Ex * mx_gompertz)
cumDevTest_gompertz$statistic
cumDevTest_gompertz$p.value

cumDevTest_makeham <- cumDevTest(Dx, Ex * mx_makeham)
cumDevTest_makeham$statistic
cumDevTest_makeham$p.value

cumDevTest_smSpline <- cumDevTest(Dx, Ex * mx_smSpline) 
cumDevTest_smSpline

cumDevTest_cubSpline <- cumDevTest(Dx, Ex * mx_cubSpline)
cumDevTest_cubSpline



#Grouping of signs test

groupSignTest <- function(zx, alpha = 0.05){
  #Count +'s and -'s
  signs <- sign(zx)
  n1 <- sum(signs == 1)
  n2 <- sum(signs == -1)
  #Count runs
  y <- c(-1, sign(zx))
  G <- sum((y[-1] != y[-(n1 + n2 + 1)]) & y[-1] != -1) # No Runs
  #Normal approximation
  mu <- n1 * (n2 + 1) / (n1 + n2)
  s2 <- (n1 * n2)^2 / (n1 + n2)^3
  G_alpha <- qnorm(alpha, mean = mu, sd = sqrt(s2)) #Critical value
  p.value <- (pnorm(G + 0.5, mean = mu, sd = sqrt(s2))) #p.value (one sided) 
  list(statistic = G, c.value = G_alpha, p.value = p.value)
}


groupSignTest_gompertz <- groupSignTest(zx_gompertz)
groupSignTest_makeham <- groupSignTest(zx_makeham)
groupSignTest_smSpline <- groupSignTest(zx_smSpline)
groupSignTest_cubSpline <- groupSignTest(zx_cubSpline)


groupSignTest_gompertz
groupSignTest_makeham
groupSignTest_smSpline
groupSignTest_cubSpline

#Serial correlations test

acf(zx_gompertz)
acf(zx_makeham)
acf(zx_smSpline)
acf(zx_cubSpline)

#Comparison the Chilean Life Tables

#mx

plot(graduation_table$age, graduation_table$Mx)
lines(graduation_table$age, graduation_table$Mx)

#qx

plot(cubSpline$fitted.values)

mx_grad <- cubSpline$fitted.values
px_grad <- exp(-mx_grad)
qx_grad <- 1- px_grad

plot(x, qx_grad, xlab = "age", ylab = "qx", main = "Graduated values of qx")
lines(x, qx_grad)



#CB-H-2020
plot(ChileanLifeTables$Age, ChileanLifeTables$`qx 2020 (CB-H-2020)`, xlab = "age", ylab = "qx",
     xlim = c(60,100), ylim = c(0, 0.4))
lines(ChileanLifeTables$Age, ChileanLifeTables$`qx 2020 (CB-H-2020)`)

#RV-M-2020
plot(ChileanLifeTables$Age, ChileanLifeTables$`qx 2020 (RV-M-2020)`, xlab = "age", ylab = "qx",
     xlim = c(60,100), ylim = c(0, 0.35))
lines(ChileanLifeTables$Age, ChileanLifeTables$`qx 2020 (RV-M-2020)`)


#B-M-2020
plot(ChileanLifeTables$Age, ChileanLifeTables$`qx 2020 (B-M-2020)`, xlab = "age", ylab = "qx",
     xlim = c(60,100), ylim = c(0, 0.35))
lines(ChileanLifeTables$Age, ChileanLifeTables$`qx 2020 (B-M-2020)`)


plot(x, qx_grad, xlab = "age", ylab = "qx", main = "Graduated values of qx", type = "n")
lines(x, qx_grad, col = 'dodgerblue', lwd = 2)
lines(ChileanLifeTables$Age, ChileanLifeTables$`qx 2020 (CB-H-2020)`, col = 'red', lwd = 2)
lines(ChileanLifeTables$Age, ChileanLifeTables$`qx 2020 (RV-M-2020)`, col = 'green', lwd = 2)
lines(ChileanLifeTables$Age, ChileanLifeTables$`qx 2020 (B-M-2020)`, col = 'orange', lwd = 2)
legend("topleft", legend = c("Graduated Rates", "CB-H-2020", "RV-M-2020", "B-M-2020"), 
       fill = c("dodgerblue", "red", "green", "orange"))

















