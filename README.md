---
title: "The Effect of COVID 19 on Mortgage lending in Oklahoma"
author: "Opal Fraser"
date: "May 2, 2023"
output: beamer_presentation
latex_engine: pdflatex
header-includes:
  - \usepackage{booktabs}
  - \usepackage{siunitx}
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(readxl)

# Borrower Race or National Origin
# 1=American Indian or Alaskan Native; 
# 2=Asian; 
# 3=Black or African American; 
# 4=Native Hawaiian or Other Pacific Islander; 
# 5=White; 
# 7=Information not provided by applicant in mail or telephone application
# Gender
# 1=Male; 
# 2=Female; 
# 3=Information not provided by applicant in mail or telephone application; 
# 4=No Co-Borrower
# Credit Scores are separated into ranges:  1 = <620, 2 = 620 to < 660, 3 = 660 < 700, 4 = 700 < 760, 5 = 760 or greater

df1 <- read_excel("/Users/home/Desktop/ECON 5253/DScourseS23/FinalProject/2019FHL.xlsx")
df1 <- data.frame(df1)
names(df1) <- c("totmonthlyincome","LTV",
                  "bo1race","bo1gender","bo1age",
                  "noteratepercent","noteamt",
                  "hsexpenseratio","debtexpenseratio")

df1 <- df1 %>% mutate(year = 2019)


df2 <- read_excel("/Users/home/Desktop/ECON 5253/DScourseS23/FinalProject/2021FHL.xlsx")
df2 <- data.frame(df2)
names(df2) <- c("totmonthlyincome","LTV",
               "bo1race","bo1gender","bo1age",
               "noteratepercent","noteamt",
               "hsexpenseratio","debtexpenseratio")

df2 <- df2 %>% mutate(year = 2021)

# Make sure continuous variables are formatted as numeric
df1 %<>% mutate(across(c("totmonthlyincome","LTV","bo1age","noteratepercent","noteamt",
                        "debtexpenseratio"), as.numeric))
# Make sure discrete variables are formatted as factors
df1 %<>% mutate(across(c("bo1gender","bo1race"), as.factor))

df1 %<>% mutate(lognoteamt = log(noteamt))
df1 %<>% mutate(logtotmonthincome = log(totmonthlyincome))
df1 %<>% select(noteamt, lognoteamt, totmonthlyincome, logtotmonthincome, noteratepercent, LTV, bo1race, bo1age, bo1gender, hsexpenseratio, debtexpenseratio, year)

```

```{r, echo=FALSE, message=FALSE, warning=FALSE}

# format as numeric
df2 %<>% mutate(across(c("totmonthlyincome","LTV","bo1age","noteratepercent","noteamt",
                         "debtexpenseratio"), as.numeric))
# Make sure discrete variables are formatted as factors
df2 %<>% mutate(across(c("bo1gender","bo1race"), as.factor))

df2 %<>% mutate(lognoteamt = log(noteamt))
df2 %<>% mutate(logtotmonthincome = log(totmonthlyincome))
df2 %<>% select(noteamt, lognoteamt, totmonthlyincome, logtotmonthincome, noteratepercent, LTV, bo1race, bo1age, bo1gender, hsexpenseratio, debtexpenseratio,year)

# mutate both years together after rbind
df <- rbind(df1, df2)
df <- df %>% mutate(year= as.factor(year))

est1 <- lm(LTV ~ lognoteamt + noteratepercent + bo1race*year + bo1gender*year + bo1age + debtexpenseratio, data=df)

est2 <- lm(lognoteamt ~ noteratepercent + LTV + bo1race*year + bo1gender*year + bo1age + debtexpenseratio + hsexpenseratio, data=df)

est3 <- lm(noteratepercent ~ debtexpenseratio + lognoteamt*year + LTV + bo1race*year + bo1gender*year + bo1age, data=df)
```
# Introduction

The project answers a question about lending in Oklahoma; does mortgage lending differ among race or gender? The difference in difference analysis looks at data before and after the COVID 19 pandemic. Using linear regression the data-set is analyzed to reveal differences. Goal is to see if same effects in terms of LTV and note rate percent echo in Oklahoma mortgages as in previous findings. The second part is a difference in difference analysis to see if COVID-19 affected mortgage lending. 

# Study 1
Study 1: According to Bartlett, Morse, and Stanton (2021), the median loan-to-value (LTV) ratio for Black borrowers with mortgage debt in the SCF+ in 2016 was roughly 66, compared to 52 for White borrowers.

[1] Bartlett, R., Morse, A., & Stanton, G. (2021). Does mortgage lending discriminate by race or sex? Evidence from HMDA data. Journal of Financial Economics, 142(2), 557-578. doi:10.1016/j.jfineco.2021.03.005

# Study 2
Study 2: According to Cheng, Lin, and Liu's (2014) study on racial discrepancies in mortgage interest rates, the authors found that there is a widespread disparity in mortgage rates between black and white borrowers. Furthermore, the study concludes that black women who are financially vulnerable suffer the most from this disparity.

[2] Cheng, P., Lin, Z., & Liu, Y. (2014). Racial Discrepancy in Mortgage Interest Rates. The Journal of Real Estate Finance and Economics.

# Data

Data collected by the Federal Housing Finance Agency from a random sample of loan-level mortgage acquisitions acquired in Oklahoma in 2019 and 2021. Dataset was mutated to change to factors where necessary. Finally, two sets were bound together for final regression. Created year variable for 2019 and 2021. 

# Data - 2019 note amount
bo1race                   mean
native american   1       167257.
asian             2       246547.
black             3       177500.
pacific islander  4       190495 
white             5       196257.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(modelsummary)
datasummary_skim(df1,histogram=FALSE)

```
# Data - 2021 note amount


bo1race                   mean
native american   1       186050.
asian             2       232612.
black             3       232612.
pacific islander  4       191333.
white             5       215323.

```{r echo=FALSE, message=FALSE, warning=FALSE}
library(modelsummary)
datasummary_skim(df2,histogram=FALSE)
```

# Models
The analysis employs multiple linear regression models to estimate the impact of several independent variables, including race, gender, age, income, credit score, and loan-to-value ratio, on the dependent variable, the amount of the mortgage note/percentage rate. 

# Model 1

$$
\text{LTV} = \beta_0 + \beta_1 \log(\text{noteamt}) + \beta_2 \text{noteratepercent} + \beta_3 \text{bo1race} \times \text{year} + \beta_4 \text{bo1gender} \times \text{year} + \beta_5 \text{bo1age} + \beta_6 \text{debtexpenseratio} + \epsilon
$$

# Model 1: Key Findings

- Significant predictors: lognoteamt, noteratepercent, bo1race2, bo1race5, bo1gender2, bo1age, and debtexpenseratio
- Multiple R-squared: 0.226
- Adjusted R-squared: 0.221

# Model 2
$$
\text{lognoteamt} = \text{noteratepercent} + \text{LTV} + \text{bo1race} \times \text{year} + \text{bo1gender} \times \text{year} + \text{bo1age} + \text{debtexpenseratio} + \text{hsexpenseratio} + \epsilon
$$

# Model 2: Key Findings

- Significant predictors: noteratepercent, LTV, bo1race2, bo1race5, bo1gender2, and hsexpenseratio
- Multiple R-squared: 0.1249
- Adjusted R-squared: 0.1189

# Model 3
$$
\text{noteratepercent} = \text{debtexpenseratio} + \text{lognoteamt} + \text{LTV} + \text{bo1race} \times \text{year} + \text{bo1gender} \times \text{year} + \text{bo1age} + \epsilon
$$
# Model 3: Key Findings

- Significant predictors: debtexpenseratio, lognoteamt, year2021, LTV, bo1race4, and year2021:bo1gender2
- Multiple R-squared: 0.6846
- Adjusted R-squared: 0.6825

# Difference in Difference results 
Includes interaction terms between race * year & gender * year

```{r echo=FALSE, fig.height=20, fig.width=6}
modelsummary(list(est1, est2, est3), stars = T)


```

# Conclusion
The results suggest that there are significant differences in mortgage lending in Oklahoma based on race and gender. In 2019, the average mortgage note amount was the highest for the Asian borrower group, followed by White, Black, Pacific Islander, and Native American borrowers. In 2021, the average mortgage note amount increased for all racial groups, with the most significant increase observed in the Black borrower group. However, more studies would need to be done considering other variables such as area like the Census tract, and it would have to control for sorting by salary or culture. 



