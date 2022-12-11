title: "Simulation"
author: "Ishu Jaswani, Cristian Leo"
date: "2022-11-15"
output:
  html_document:
    toc: true
    toc_float: true
header-includes:
  - \usepackage{sectsty}
  - \allsectionsfont{\color{cyan}}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE,warning = FALSE, message = FALSE)
```

# Power of the test
The population considered is the number of students in New York in the academic year 2017-2018 (NYSED Data Site, 2018). Based on this population we want a sample with a statistical power of 90% for an effect size of 0.1. Based on research question 3 will be running a RCT, therefore we can get the minimum sample size to achieve the stated statistical power by running an anova power test with 4 groups, which are the 4 independent variables in our analysis: treatment or control, level of income, gender, and level of education.
Based on the test 356 students per group are required to achieve the statistical power desired.

“2018 NY STATE Higher Education Enrollment Data | NYSED Data Site.” Accessed December 5, 2022. https://data.nysed.gov/highered-enrollment.php?year=2018&state=yes.
```{r}
Uni_studnets_NY <- 1236841

library(pwr)
sample_h1 = pwr.t.test(d = 0.2/2,sig.level = 0.05,power = 0.9,type = "two.sample",alternative = "greater")$n; sample_h1  # based in power of the test, effect size, in each group
sample_ratio <- sample_h1/Uni_studnets_NY; sample_ratio
sample_anova <- pwr.anova.test(k = 4,f = 0.1,sig.level = 0.05,power = 0.9)$n;sample_anova 
#356 in each group
```

# Simulation with effect (1 experiment)
# Survey Questions Simulation
## Survey Question 1

What is your house hold income level?
1) 0-50k 2) 50-150K 3) 150K-300 4) 300+


The household income of students are aound 150k-300k since college tuition is expensive. We are assuming that most students dont take a loan to come to college therefore, dont have extremely now household incomes.

```{r}
set.seed(224) # randomly generate numbers to remain constant
income_house <- rnorm(n = 360,mean = 150,sd = 100) # normal distribution 
max(income_house);min(income_house)
library(Hmisc);library(dplyr);library(forcats)
income_house_sust <- cut(income_house,breaks = c(-Inf,50,150,300,Inf),preset_bins_labels = cut(income_house,breaks = c(-Inf,50,150,300,Inf)),labels =c("0-50k", "50-150k", "150-300k", "300k+"))
fct_count(income_house_sust)
```



## Survery Question 2

What is your gender ?
1) male
2) female

Considering only male and female We have simulated a binomial distribution where there are equal number of males and equal number of females.

```{r}
set.seed(3)
gender <- factor(rbinom(n=360, size=1, p=0.5), levels = c(0, 1), labels = c("Male", "Female")) # binomial distribution
fct_count(gender)
```


## Survery Question 3
What is your education level?
1) undergraduate 2) Graduate 3) MBA 4) PHD

Since there are more undergraduates we have skewed the distribution towards undergraduates.
we have used a normal distribution which was centered around 1 and had a standard deviation of 2 and assigned values accordingly so that undergraduates are more and rest are assigned less.

```{r}
set.seed(2)
edu <- rnorm(n = 360,mean = 1,sd = 2) # normal distribution
max(edu);min(edu)
library(Hmisc);library(dplyr)
edu_sust <- cut(edu,breaks = c(-Inf,1,2,3,Inf),preset_bins_labels = cut(edu,breaks = c(-Inf,1,2,3,Inf)),labels =c("Undergraduate", "Graduate", "MBA", "PHD"))
fct_count(edu_sust)
```


## Survey question 4

How often do you shop for new clothing?
Answers: Once a week or more; About once a month; A few times a year; Once a year; Never

We have simulated a normal distribution as we understand taht people shopping never are less and people shopping once a week or more also less too. the general audiance lies within the range of once a month and few times a year.

```{r}
# normal dist
set.seed(1)
often_tr <- rnorm(n = 360,mean = 2.5,sd = 1) # normal distribution
max(often_tr);min(often_tr)
library(Hmisc);library(dplyr)
often_clothing <- cut(often_tr,breaks = c(-1,1,2,3,4,6),preset_bins_labels = cut(often_tr,breaks = c(-1,1,2,3,4,6)),labels =c("Once a week or more","About once a month","A few times a year","Once a year","Never"))
fct_count(often_clothing)
```

## Survey question 5
Please indicate your level of agreement with the following statement:
When given the option, I choose to purchase sustainable products.
Answers: Strongly disagree; Somewhat disagree; Neither agree nor disagree; Somewhat agree; Strongly agree

We have used a normal distribution and then assigned values according so that teh distribution is skewed towards somewhat agree and strongly agree as we assume at if people are given the option holding other parameters constant. They would wish to purchase sustainable products

```{r}
#

library(dplyr);library(Hmisc);library(forcats)
set.seed(1)
agree <- rnorm(n = 360,mean = 4,sd = 1.5)
max(agree);min(agree)
library(Hmisc);library(dplyr)
sust_agree <- cut(agree,breaks = c(-1,1,2,3,4,Inf),preset_bins_labels = cut(agree,breaks = c(-1,1,2,3,4,6)),labels =c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
fct_count(sust_agree)
```

## Survey question 6
Please indicate your level of agreement with the following statement:
I am willing to pay more for a product that is sustainably produced.
Answers: Strongly disagree; Somewhat disagree; Neither agree nor disagree; Somewhat agree; Strongly agree

We assume that the distribution of the likert scale will follow a similar trend - as question 5. As people think that sustainable products are more expensive but it is a step further to saving the enviornment. Therefore they would be willing to pay more in general.


```{r}
set.seed(2)
pay <- rnorm(n = 360,mean = 4,sd = 1.5)
max(pay);min(pay)
library(Hmisc);library(dplyr)
sust_pay <- cut(pay,breaks = c(-1,1,2,3,4,Inf),preset_bins_labels = cut(pay,breaks = c(-1,1,2,3,4,6)),labels =c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
fct_count(sust_pay)
```

## Survey Question 7
Please indicate your level of agreement with the following statement:
When retail shopping, I look at the tags and other available information to determine if the article of clothing to determine if it was produced sustainably.
Answers: Strongly disagree; Somewhat disagree; Neither agree nor disagree; Somewhat agree; Strongly agree


```{r}
set.seed(2)
info_tags <- rnorm(n = 360,mean = 1,sd = 2)
max(info_tags);min(info_tags)
library(Hmisc);library(dplyr)
info_tags_sust <- cut(info_tags,breaks = c(-Inf,1,2,3,4,Inf),preset_bins_labels = cut(info_tags,breaks = c(-Inf,1,2,3,4,Inf)),labels =c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
fct_count(info_tags_sust)
```

## Survey Question 8
Please indicate your level of agreement with the following statement: #rephrase
When a company adopts sustainable business practices, I will go out of my way to buy their products.
Answers: Strongly disagree; Somewhat disagree; Neither agree nor disagree; Somewhat agree; Strongly agree

Question 5 aims to identify in what extent sustainability is a purchasing criteria. On a scale from 1 to 5, where 1 is "Strongly disagree", and 5 is "Strongly agree", we assume that most of the responses will focus on the slight right of middle of the scale. Indeed, our assumed mean is 3.3 and standard deviation 2. We used a random normal distribution to assure a distribution focused on the mean and with tails on the extremes.
```{r}
set.seed(3)
extra_effort <- rnorm(n = 360,mean = 3.3,sd = 2)
max(extra_effort);min(extra_effort)
library(Hmisc);library(dplyr);library(forcats)
sust_extra_effort <- cut(extra_effort,breaks = c(-Inf,1,2,3,4,Inf),preset_bins_labels = cut(extra_effort,breaks = c(-Inf,1,2,3,4,Inf)),labels =c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
fct_count(sust_extra_effort)
```

## Survey Question 9
Treatment group: For the shirt pictured here (insert picture of non-sustainable shirt), how much is this item worth?
Answers: Continuous scale ($0 to $50)

Control group: For the shirt pictured here (insert picture of sustainable shirt with description), how much is this item worth?
Answers: Continuous scale ($0 to $50)

Question 6 is a customized question, which will show a t-shirt with a description. In the treatment group the description will mention that the t-shirt is made by sustainable fabrics and the company operates following environmental sustainable practices. In the control group the same t-shirt will be displayed, but this time the description will mention that the t-shirt and the company that produced are not sustainable.
In the survey will be given the possibility to the survey takers to drag a point in the bar from 0 to 50 to indicate how much is the given t-shirt worth it.
We assume that the respondents in the treatment group will answer with a higher price on average. Therefore, the distribution in the control group will be right skewed with a lower mean and concentration of price given, while the treatment group will report the opposite. To do so, we used a Chi-squared distribution with 20 degrees of freedom for the control group and a skewed random normal distribution with a skewness parameter of 0.5 with the rsnorm function from the "fGarch" library.
```{r}
# control
set.seed(3)
cont_price <-as.integer(rchisq(n = 180,df = 20)); min(cont_price);max(cont_price)
hist(cont_price)
# treatment
library(fGarch)
set.seed(2)
treat_price <- as.integer(rsnorm(180, 40, 5, 0.5)); min(treat_price);max(treat_price)
hist(as.integer(treat_price))
```

## Survey Question 10
Treatment group: Would you be willing to buy the same shirt for $40?
Answers: Yes; No

Control group: Would you be willing to buy the same shirt for $40?
Answers: Yes; No

Following question 6, question 7 is a customized question as well. However, if question 6 measures the price perception of the t-shirt, question 7 measures the willingness to buy given the same price both for the treatment group and the control group.
We assume that the proportion of "Yes" in the treatment group is higher than the proportion of "Yes" in the control group. Moreover, we state a meaningful difference of roughly 40%, in order to be compelling for the companies to switch their production from non sustainable to sustainable.
To simulate the results we used a random binomial distribution with probability of 0.3 in the control group and 0.6999 in the treatment group.
```{r}
# control
set.seed(3)
cont_buy <- as.factor(rbinom(n = 180,size = 1,prob = 0.3))
cont_buy <- fct_recode(cont_buy,"Yes"="1","No"="0")
fct_count(cont_buy)

# treatment
set.seed(12029)
treat_buy <- as.factor(rbinom(n = 180,size = 1,prob = 0.6999))
treat_buy <- fct_recode(treat_buy,"Yes"="1","No"="0")
fct_count(treat_buy)
```

## Survey Question 11
Do you believe the quality of sustainably produced clothing is higher than non-sustainably produced clothing?
Answers: Yes; No; I don’t know

Question 8 allows us to understand a deeper insights on why a sustainable product is worthed more than a non sustainable one. We assume that one of the main reason is a higher quality of the product. Therefore, we may expect a higher proportion of yes both in the treatment and control group. To achieve that, we simulated the data with a random normal distribution with mean 1 and standard deviation of 0.8. Then we binned the numbers generated by the random normal distribution with the possible answers, where numbers up to 1 mean "Yes", 1 to 2 mean "No", and greater than 2 mean "I don't know".
```{r}
set.seed(2)
quality <- rnorm(n = 360,mean = 1,sd = 0.8)
max(quality);min(quality)
library(Hmisc);library(dplyr)
quality_sust <- cut(quality,breaks = c(-Inf,1,2,Inf),preset_bins_labels = cut(quality,breaks = c(-Inf,1,2,Inf)),labels =c("Yes","No","I don't Know"))
fct_count(quality_sust)
```


# Simulation Dataset
```{r,echo=TRUE,include=TRUE}

main_data <- data.frame(index = 1:360,often_clothing,sust_agree,sust_pay,info_tags_sust,sust_extra_effort,quality_sust,edu_sust,gender,income_house_sust)
colnames(main_data)
set.seed(20)
main_data <- main_data %>%
  mutate(z = row_number() %in% sample(n(), n() / 2) )

skimr::skim(main_data)
main_data <- main_data |>
  arrange(z)

main_data <- main_data |>
  mutate(price = c(cont_price,treat_price),buy=c(cont_buy,treat_buy))

main_data
```

# Statistical Analysis Simulation with effect (1 experiment)
## Research Question 1
Do sustainable labels influence people’s perception about products’ perceived price?

In the first research question we examine whether the sustainability of a product affects the customer perception about the products' perceived price. To do so we analyze the answers of the question 10 of the survey with a prop test.
```{r}
main_data |>
  select(z,buy)|>
  table()

rq1 <- prop.test(x = c(128,54),n=c(180,180),alternative = "greater")

effect_size_rq1 <- prop.test(x = c(128,54),n=c(180,180),alternative = "greater")$estimate; (effect_size_rq1[1]-effect_size_rq1[2])*100 # in percentage%
```

## Research Question 2
Do people willing to pay a higher price for a product if it comes with a sustainable label? If so, to what degree?

The second research questions aims to provide an actionable insight for the companies. Indeed, the goal of this study is to compel the companies to switch their operations into a sustainable way. 
```{r}
rq2 <- t.test(x=main_data[main_data$z == 1, ]$price,y=main_data[main_data$z == 0, ]$price,alternative = "greater")

effect_size_rq2 <- t.test(x=main_data[main_data$z == 1, ]$price,y=main_data[main_data$z == 0, ]$price,alternative = "greater")$estimate; (effect_size_rq2[1]-effect_size_rq2[2])
```
## Research Question 3 - binary outcome
Do gender, education and household income level af- fect the influences of sustainability tags on people’s willingness to purchase?
```{r}
library(tidyverse);library(broom)
main_data_hilo <- main_data %>% 
  mutate(price_hilo = factor(ifelse(price >= mean(price), 1, 0), levels = c(0, 1)))
model <- glm(price_hilo~z+edu_sust+income_house_sust+gender,data=main_data_hilo, family = "binomial")
pred <- predict(model, type='response')
pred_hilo <- ifelse(pred>0.5, 1, 0)

prop.table(table(pred = pred_hilo, true = main_data_hilo$price_hilo))
summary(model)
```


## Research Question 3 - continuous outcome
```{r}
main_data$z <- as.factor(main_data$z)

model <- lm(price~z+edu_sust +income_house_sust+gender,data=main_data)
summary(model)

model_anov <- aov(price~z+edu_sust +income_house_sust+gender,data=main_data)

t1 <- TukeyHSD(model_anov,which = "income_house_sust" ,conf.level=.95)$income_house_sust

t2 <- TukeyHSD(model_anov,which = "z" ,conf.level=.95)$z

t3 <- TukeyHSD(model_anov,which = "edu_sust" ,conf.level=.95)$edu_sust

t4 <- TukeyHSD(model_anov,which = "gender" ,conf.level=.95)$gender

t5 <- rbind(t2,t1,t3,t4)


plot(TukeyHSD(model_anov,which = "income_house_sust" ,conf.level=.95))

tidy(anova(object = model))



class(tidy(model))

tidy(model) |>
  select(estimate,p.value) |>
  mutate(i = seq(1,9,1)) |>
  filter(i==1) |>
  select(estimate,p.value)
```

# Simulation with effect (1000 experiments)
## Level of Income
```{r}
set.seed(4172)
B <- 1000 # number of experiments to replicate
# put above experiment into function for replication
experiment <- function(n = 360) {
  y_0 <- rnorm(n = n, mean = 150, sd = 100) %>% round(digits = 1)
  tibble(y_0)
}
# per notes, which is less intuitive (reads like one experiment, size 60 * 1000)
income_house <- experiment(n = 360 * B) %>% mutate(i = rep(seq(B), each = 360) )
max(income_house);min(income_house)
library(Hmisc);library(dplyr)
income_house_sust <- cut(income_house$y_0,breaks = c(-Inf,50,150,300,Inf),preset_bins_labels = cut(income_house,breaks = c(-Inf,50,150,300,Inf)),labels =c("0-50k", "50-150k", "150-300k", "300k+"))
 fct_count(income_house_sust)
```

## Gender
```{r}
fct_count(as.vector(replicate(1000,factor(rbinom(n = 360, size = 1, p=0.5), levels = c(0, 1), labels = c("Male", "Female")))))

rep(seq(1000),each = 360)

library(tidyverse)
library(broom)
B <- 1000 # number of experiments to replicate
# put above experiment into function for replication
analyze_experiment <- function(y_1 , y_0) {
  prop.test(x= c(y_1,y_0), n=c(360/2,360/2), alternative = 'greater') %>%
  tidy() |>
  mutate(effect = estimate1-estimate2) |>
  select(effect_size = effect, upper_ci = conf.high, p = p.value)
}
experiment <- function(n = 360) {
  y_0 <- factor(rbinom(n = n/2, size = 1, p=0.5), levels = c(0, 1), labels = c("Male", "Female"))
  y_1 <- factor(rbinom(n = n/2, size = 1, p=0.5), levels = c(0, 1), labels = c("Male", "Female"))
  tibble(y_0, y_1 )
}
# per notes, which is less intuitive (reads like one experiment, size 60 * 1000)
Gender_simulation   <- experiment(n = 360 * B) %>% mutate(i = rep(seq(B), each = 180) )
# run test on each experiment

x <- Gender_simulation %>% group_by(i,y_1) |> summarise(no_rows = length(y_1))
y <- Gender_simulation %>% group_by(i,y_0) |> summarise(no_rows = length(y_0))

Gender_simulation <- cbind(x,y)
results <- Gender_simulation |>
  filter(y_1 =="Male") |>
  select(no_rows...3, no_rows...6) |>    # row 3 is treatment and row 6 is control
  mutate(i = rep(seq(B), each = 1) ) |>
  group_by(i) |>
  summarise(analyze_experiment(no_rows...3,no_rows...6))


mean(results$p < 0.05)
summary(results$effect_size)
summary(results$upper_ci)
```

# Statistical Analysis Simulation with effect (experiment = 1000)
## Research Question 1
```{r}
library(tidyverse)
library(broom);library(dplyr)
set.seed(2)
B <- 1000 # number of experiments to replicate
# put above experiment into function for replication
analyze_experiment <- function(y_1 , y_0) {
  prop.test(x= c(y_0,y_1), n=c(360/2,360/2), alternative = 'greater') %>%
  tidy() |>
  mutate(effect = estimate1-estimate2) |>
  select(effect_size = effect, upper_ci = conf.high, p = p.value)
}
experiment <- function(n = 360) {
  y_0 <- factor(rbinom(n = n/2,size = 1,prob = 0.3), levels = c(0, 1),  labels = c("Yes", "No"))
  y_1 <- factor(rbinom(n = n/2,size = 1,prob = 0.6999), levels = c(0, 1),  labels = c("Yes", "No"))
  tibble(y_0, y_1 )
}

buy_simulation   <- experiment(n = 360 * B) %>% mutate(i = rep(seq(B), each = 360/2) )
# run test on each experiment

x <- buy_simulation %>% group_by(i,y_1) |> summarise(no_rows = length(y_1))
y <- buy_simulation %>% group_by(i,y_0) |> summarise(no_rows = length(y_0))

buy_simulation <- cbind(x,y)
results <- buy_simulation |>
  filter(y_1 =="Yes") |>
  select(no_rows...3, no_rows...6) |>    # row 3 is control and row 6 is test
  mutate(i = rep(seq(B), each = 1) ) |>
  group_by(i) |>
  summarise(analyze_experiment(no_rows...3,no_rows...6))


mean(results$p > 0.05)
summary(results$effect_size)
summary(results$upper_ci)
```

## Research Question 2
```{r}
set.seed(4172)
B_2 <- 1000 # number of experiments to replicate
# put above experiment into function for replication
experiment <- function(n = 360) {
  y_0 <- as.integer(rchisq(n = n/2,df = 20))
  y_1 <- as.integer(rsnorm(n/2, 40, 5, 0.5))
  tibble(y_0, y_1 )
}

analyze_experiment <- function(y_1, y_0) {
  t.test(x = y_1, y = y_0, alternative = 'less') %>%
  tidy() %>%
  select(effect = estimate, upper_ci = conf.high, p = p.value)
}


# per notes, which is less intuitive (reads like one experiment, size 60 * 1000)
d_rep   <- experiment(n = 360 * B_2) %>% mutate(i = rep(seq(B), each = 360/2) )
# run test on each experiment
results <- d_rep %>% group_by(i) %>% summarise( analyze_experiment(y_1, y_0) )
# same summaries as in notes; but can do whatever you want
mean(results$p < 0.05)
summary(results$effect)
summary(results$upper_ci)

```


# Simulation with no effect
## Survey Question 1
```{r}
set.seed(224) # randomly generate numbers to remain constant
income_house <- rnorm(n = 360,mean = 180,sd = 100) # normal distribution 
max(income_house);min(income_house)
library(Hmisc);library(dplyr)
income_house_sust <- cut(income_house,breaks = c(-Inf,50,180,360,Inf),preset_bins_labels = cut(income_house,breaks = c(-Inf,50,180,360,Inf)),labels =c("0-50k", "50-180k", "180-360k", "360k+"))
fct_count(income_house_sust)
```

## Survery Question 2
```{r}
set.seed(3)
gender <- factor(rbinom(n=360, size=1, p=0.5), levels = c(0, 1), labels = c("Male", "Female")) # binomial distribution
fct_count(gender)
```

## Survery Question 3
```{r}
set.seed(2)
edu <- rnorm(n = 360,mean = 2.5,sd = 2) # normal distribution
max(edu);min(edu)
library(Hmisc);library(dplyr)
edu_sust <- cut(edu,breaks = c(-Inf,1,2,3,Inf),preset_bins_labels = cut(edu,breaks = c(-Inf,1,2,3,Inf)),labels =c("Undergraduate", "Graduate", "MBA", "PHD"))
fct_count(edu_sust)
```

## Survey question 4
```{r}
# normal dist
set.seed(1)
often_tr <- rnorm(n = 360,mean = 2.5,sd = 2) # normal distribution
max(often_tr);min(often_tr)
library(Hmisc);library(dplyr)
often_clothing <- cut(often_tr,breaks = c(-1,1,2,3,4,6),preset_bins_labels = cut(often_tr,breaks = c(-1,1,2,3,4,6)),labels =c("Once a week or more","About once a month","A few times a year","Once a year","Never"))
fct_count(often_clothing)
```

## Survey question 5
```{r}
#

library(dplyr);library(Hmisc);library(forcats)
set.seed(1)
agree <- rnorm(n = 360,mean = 2.5,sd = 2)
max(agree);min(agree)
library(Hmisc);library(dplyr)
sust_agree <- cut(agree,breaks = c(-1,1,2,3,4,Inf),preset_bins_labels = cut(agree,breaks = c(-1,1,2,3,4,6)),labels =c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
fct_count(sust_agree)
```

## Survey Question 6-7
```{r}
# control
set.seed(3)
cont_price <-rnorm(180, 30, 15); min(cont_price);max(cont_price)

# treatment
library(fGarch)
set.seed(2)
treat_price <- as.integer(rnorm(180, 30, 15)); min(treat_price);max(treat_price)
hist(as.integer(treat_price))
```

## Survey Question 8-9
```{r}
# control
set.seed(3)
cont_buy <- as.factor(rbinom(n = 180,size = 1,prob = 0.5))
cont_buy <- fct_recode(cont_buy,"Yes"="1","No"="0")
fct_count(cont_buy)

# treatment
set.seed(12029)
treat_buy <- as.factor(rbinom(n = 180,size = 1,prob = 0.5))
treat_buy <- fct_recode(treat_buy,"Yes"="1","No"="0")
fct_count(treat_buy)
```

## Survey Question 10
```{r}
set.seed(2)
quality <- rnorm(n = 360,mean = 1.5,sd = 0.8)
max(quality);min(quality)
library(Hmisc);library(dplyr)
quality_sust <- cut(quality,breaks = c(-Inf,1,2,Inf),preset_bins_labels = cut(quality,breaks = c(-Inf,1,2,Inf)),labels =c("Yes","No","I don't Know"))
fct_count(quality_sust)
```
# Simulation Dataset
```{r,echo=TRUE,include=TRUE}

main_data <- data.frame(index = 1:360,often_clothing,sust_agree,sust_pay,info_tags_sust,sust_extra_effort,quality_sust,edu_sust,gender,income_house_sust)
colnames(main_data)
set.seed(20)
main_data <- main_data %>%
  mutate(z = row_number() %in% sample(n(), n() / 2) )

skimr::skim(main_data)
main_data <- main_data |>
  arrange(z)

main_data <- main_data |>
  mutate(price = c(cont_price,treat_price),buy=c(cont_buy,treat_buy))

main_data
```

# Statistical Analysis Simulation with no effect
## Research Question 1
```{r}
main_data |>
  select(z,buy)|>
  table()

rq1 <- prop.test(x = c(82,74),n=c(180,180),alternative = "greater")

effect_size_rq1 <- prop.test(x = c(82,74),n=c(180,180),alternative = "greater")$estimate; (effect_size_rq1[1]-effect_size_rq1[2])*100 # in percentage%
```

## Research Question 2
```{r}
main_data[main_data$z == 1, ]$price

rq2 <- t.test(x=main_data[main_data$z == 1, ]$price,y=main_data[main_data$z == 0, ]$price,alternative = "greater")

effect_size_rq2 <- t.test(x=main_data[main_data$z == 1, ]$price,y=main_data[main_data$z == 0, ]$price,alternative = "greater")$estimate; (effect_size_rq2[1]-effect_size_rq2[2])
```

## Research Question 3
```{r}
main_data <- main_data %>% 
  mutate(price_hilo = factor(ifelse(price >= mean(price), 1, 0), levels = c(0, 1)))
model <- glm(price_hilo~z+edu_sust+income_house_sust+gender,data=main_data, family = "binomial")
pred <- predict(model, type='response')
pred_hilo <- ifelse(pred>0.5, 1, 0)

prop.table(table(pred = pred_hilo, true = main_data$price_hilo))
summary(model)
```
