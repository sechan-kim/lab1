My first GitHub project
---
title: "MELS_Model_rmd_file"
output: html_document
date: "2026-10-12"
---

```{r setup, include=FALSE}
library(lme4)
library(lmerTest)
library(readr)
library(data.table)
library("haven")
library("dplyr")
library("lavaan")
library("performance")
library("tidyr")
library("tidyverse")
library(psych)
library(knitr)
library(dplyr)
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
data_raw <- read.csv("~/Downloads/blozislab/TALIS2024_teachers_NoESE_CSV/ttgintt4.csv",
                 sep = ";")
data2_raw <- as.data.frame(fread("~/Downloads/blozislab/TALIS2024_principals_NoESE_CSV/tcgintt4.csv"))

target_countries <- c("BRA", "JPN", "AUS", "USA") #
dt <- as.data.table(data)

result <- dt[CNTRY %in% target_countries,
             .(Number_of_Teachers = uniqueN(IDTEACH),
               Number_of_Schools  = uniqueN(IDSCHOOL)),
             by = .(IDCNTRY, CNTRY)][order(IDCNTRY)]
result

iv <- c("TT4G47E")
dv <- c("TT4G54A","TT4G54B","TT4G54C","TT4G54D")
```



# USA

```{r Japan database set up}
data_long <- data_raw %>%
  select(IDCNTRY, CNTRY, IDSCHOOL, IDTEACH,
         all_of(iv), all_of(dv)) %>%
  filter(CNTRY == "USA") %>%
  mutate(across(all_of(dv), ~ ifelse(. > 4, NA, .))) %>%
  mutate(across(all_of(iv), ~ ifelse(. %in% c(98, 99), NA, .))) %>%
  mutate(ivmean = rowMeans(across(all_of(iv)), na.rm = TRUE)) %>%
  pivot_longer(
    cols      = all_of(dv),
    names_to  = "item",
    values_to = "response"
  )

data_listwise <- data_long %>%
  group_by(IDSCHOOL, IDTEACH) %>%
  filter(!any(is.na(response)), !is.na(ivmean)) %>%   # also drop missing iv
  ungroup()


data_avg <- data_listwise %>%
  group_by(IDSCHOOL, IDTEACH) %>%
  summarise(
    dvmean = round(mean(response), 4),
    ivmean = first(ivmean),
    .groups = "drop") %>%
  mutate(
    iv_school = mean(ivmean),                   
    iv_within = ivmean - iv_school              
  ) %>%
  ungroup()

```


Model1 null
```{r model 1}
m0 <- lmer(dvmean ~ 1 + (1|IDSCHOOL), data = data_avg, REML = TRUE)  # null
summary(m0)
```
Response variable name: dvmean (Null)
Level 1 variance:  0.25709
Level 2 variance: 0.02727
ICC: 0.02727 / (0.25709 + 0.02727) = 0.09590

Model 2
```{r model 2 no covariates}
model2_null <- data_avg %>%
  select(IDSCHOOL, IDTEACH, disruption_mean = dvmean) %>%
  group_by(IDSCHOOL) %>%
  arrange(IDSCHOOL) %>%
  as.data.frame()

write.csv(model2_null, "~/Downloads/blozislab/talis_usa_test_mod2_null.csv",
          row.names = FALSE, quote = FALSE)
```

```{r}
res <- read.csv("~/Downloads/blozislab/MELS_blozis_usa_test_null.csv")
p <- setNames(res$estimate, res$parameter)

var_b <- exp(p["alp0"])^2                                # between schools
var_w <- exp(p["delta_U_(Intercept)"] + exp(p["logSa"]) / 2)   # within schools
ran_var <- exp(p["logSa"])

var_b
var_w
ran_var
var_b / (var_b + var_w)
```

Null
Level 1 variance: 0.2731
Level 2 variance: 0.02006  
Random Variance: 1.005678
ICC: 0.02006 / (0.2731 + 0.02006) = 0.06843


model 3
```{r}
ord_long <- data_listwise %>%
  select(IDSCHOOL, IDTEACH, item, y = response) %>%
  arrange(IDSCHOOL, IDTEACH, item)

write_csv(ord_long, "~/Downloads/blozislab/talis_usa_ordinal.csv")
```

Level 1 variance: 5.3813
Level 2 variance: 0.8089 
Random Variance: 0.0591
ICC: 0.4907 / (0.4907 + 3.369) = 0.1307

# Brazil

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
data_raw <- read.csv("~/Downloads/blozislab/TALIS2024_teachers_NoESE_CSV/ttgintt4.csv",
                 sep = ";")
data2_raw <- as.data.frame(fread("~/Downloads/blozislab/TALIS2024_principals_NoESE_CSV/tcgintt4.csv"))
knitr::opts_chunk$set(echo = TRUE)

iv <- c("TT4G47E")
dv <- c("TT4G54A","TT4G54B","TT4G54C","TT4G54D")

```

```{r Brazil database set up}
data_long <- data_raw %>%
  select(IDCNTRY, CNTRY, IDSCHOOL, IDTEACH,
         all_of(iv), all_of(dv)) %>%
  filter(CNTRY == "BRA") %>%
  mutate(across(all_of(dv), ~ ifelse(. > 4, NA, .))) %>%
  mutate(across(all_of(iv), ~ ifelse(. %in% c(98, 99), NA, .))) %>%
  mutate(ivmean = rowMeans(across(all_of(iv)), na.rm = TRUE)) %>%
  pivot_longer(
    cols      = all_of(dv),
    names_to  = "item",
    values_to = "response"
  )

data_listwise <- data_long %>%
  group_by(IDSCHOOL, IDTEACH) %>%
  filter(!any(is.na(response)), !is.na(ivmean)) %>%   # also drop missing iv
  ungroup()


data_avg <- data_listwise %>%
  group_by(IDSCHOOL, IDTEACH) %>%
  summarise(
    dvmean = round(mean(response), 4),
    ivmean = first(ivmean),
    .groups = "drop") %>%
  mutate(
    iv_school = mean(ivmean),                   
    iv_within = ivmean - iv_school              
  ) %>%
  ungroup()

```


Model1 null
```{r model 1}
m0 <- lmer(dvmean ~ 1 + (1|IDSCHOOL), data = data_avg, REML = TRUE)  # null
summary(m0)

m1 <- lmer(dvmean ~ ivmean + (1|IDSCHOOL), data = data_avg, REML = TRUE) #l1 covariate
summary(m1)
```
Response variable name: dvmean (Null)
Level 1 variance: 0.47618
Level 2 variance: 0.07775
ICC: 0.07775 / (0.47618 + 0.07775) = 0.1404

L1 Covariate
Level 1 variance: 0.47618
Level 2 variance: 0.07449 
ICC: 0.07449 / (0.47618 + 0.07449) = 0.1353

Model 2
```{r model 2 no covariates}
model2_null <- data_avg %>%
  select(IDSCHOOL, IDTEACH, disruption_mean = dvmean) %>%
  group_by(IDSCHOOL) %>%
  arrange(IDSCHOOL) %>%
  as.data.frame()

write.csv(model2_null, "~/Downloads/blozislab/talis_bra_test_mod2_null.csv",
          row.names = FALSE, quote = FALSE)
```
```{r}
res <- read.csv("~/Downloads/blozislab/MELS_blozis_bra_test_null.csv")
p <- setNames(res$estimate, res$parameter)

var_b <- exp(p["alp0"])^2                                # between schools
var_w <- exp(p["delta_U_(Intercept)"] + exp(p["logSa"]) / 2)   # within schools

var_b
var_w
var_b / (var_b + var_w)
```
Null
Level 1 variance: 0.4666
Level 2 variance: 0.08149  
ICC: 0.08149 / (0.4666 + 0.08149) = 0.1487

```{r model 2 covariates}
mixwild_data <- data_avg %>%
  select(IDSCHOOL, IDTEACH,
         disruption_mean = dvmean,
         sen_school   = iv_school,
         sen_within = iv_within) %>%
  filter(!is.na(sen_comp)) %>%
  group_by(IDSCHOOL) %>%
  arrange(IDSCHOOL) %>%
  as.data.frame()

write.csv(mixwild_data, "~/Downloads/blozislab/talis_bra_test_mod2.csv", row.names = FALSE, quote = FALSE)
```
```{r}
res <- read.csv("~/Downloads/blozislab/MELS_blozis_bra_test_sen.csv")
p <- setNames(res$estimate, res$parameter)

var_b <- exp(p["alp0"])^2                                # between schools
var_w <- exp(p["delta_U_(Intercept)"] + exp(p["logSa"]) / 2)   # within schools

var_b
var_w
var_b / (var_b + var_w)

```
L1 Covariate
Level 1 variance: 0.4627
Level 2 variance: 0.07691934  
ICC: 0.07692 / (0.4627 + 0.07692) = 0.1425


model 3
```{r}
ord_long <- data_listwise %>%
  select(IDSCHOOL, IDTEACH, item, y = response) %>%
  arrange(IDSCHOOL, IDTEACH, item)

write_csv(ord_long, "~/Downloads/blozislab/talis_bra_ordinal.csv")
```

Level 1 variance: 0.4907
Level 2 variance: 3.369  
ICC: 0.4907 / (0.4907 + 3.369) = 0.1271



# Japan

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
data_raw <- read.csv("~/Downloads/blozislab/TALIS2024_teachers_NoESE_CSV/ttgintt4.csv",
                 sep = ";")
data2_raw <- as.data.frame(fread("~/Downloads/blozislab/TALIS2024_principals_NoESE_CSV/tcgintt4.csv"))
knitr::opts_chunk$set(echo = TRUE)

iv <- c("TT4G47E")
dv <- c("TT4G54A","TT4G54B","TT4G54C","TT4G54D")

```

```{r Japan database set up}
data_long <- data_raw %>%
  select(IDCNTRY, CNTRY, IDSCHOOL, IDTEACH,
         all_of(iv), all_of(dv)) %>%
  filter(CNTRY == "JPN") %>%
  mutate(across(all_of(dv), ~ ifelse(. > 4, NA, .))) %>%
  mutate(across(all_of(iv), ~ ifelse(. %in% c(98, 99), NA, .))) %>%
  mutate(ivmean = rowMeans(across(all_of(iv)), na.rm = TRUE)) %>%
  pivot_longer(
    cols      = all_of(dv),
    names_to  = "item",
    values_to = "response"
  )

data_listwise <- data_long %>%
  group_by(IDSCHOOL, IDTEACH) %>%
  filter(!any(is.na(response)), !is.na(ivmean)) %>%   # also drop missing iv
  ungroup()


data_avg <- data_listwise %>%
  group_by(IDSCHOOL, IDTEACH) %>%
  summarise(
    dvmean = round(mean(response), 4),
    ivmean = first(ivmean),
    .groups = "drop") %>%
  mutate(
    iv_school = mean(ivmean),                   
    iv_within = ivmean - iv_school              
  ) %>%
  ungroup()

```


Model1 null
```{r model 1}
m0 <- lmer(dvmean ~ 1 + (1|IDSCHOOL), data = data_avg, REML = TRUE)  # null
summary(m0)
```
Response variable name: dvmean (Null)
Level 1 variance:  0.25709
Level 2 variance: 0.02727
ICC: 0.02727 / (0.25709 + 0.02727) = 0.09590

Model 2
```{r model 2 no covariates}
model2_null <- data_avg %>%
  select(IDSCHOOL, IDTEACH, disruption_mean = dvmean) %>%
  group_by(IDSCHOOL) %>%
  arrange(IDSCHOOL) %>%
  as.data.frame()

write.csv(model2_null, "~/Downloads/blozislab/talis_jpn_test_mod2_null.csv",
          row.names = FALSE, quote = FALSE)
```

```{r}
res <- read.csv("~/Downloads/blozislab/MELS_blozis_jpn_test_null.csv")
p <- setNames(res$estimate, res$parameter)

var_b <- exp(p["alp0"])^2                                # between schools
var_w <- exp(p["delta_U_(Intercept)"] + exp(p["logSa"]) / 2)   # within schools
ran_var <- exp(p["logSa"])

var_b
var_w
ran_var
var_b / (var_b + var_w)
```

Null
Level 1 variance: 0.2731
Level 2 variance: 0.02006  
Random Variance: 1.005678
ICC: 0.02006 / (0.2731 + 0.02006) = 0.06843


model 3
```{r}
ord_long <- data_listwise %>%
  select(IDSCHOOL, IDTEACH, item, y = response) %>%
  arrange(IDSCHOOL, IDTEACH, item)

write_csv(ord_long, "~/Downloads/blozislab/talis_jpn_ordinal.csv")
```

Level 1 variance: 5.3813
Level 2 variance: 0.8089 
Random Variance: 0.0591
ICC: 0.4907 / (0.4907 + 3.369) = 0.1307

# AUS

```{r Brazil database set up}
data_long <- data_raw %>%
  select(IDCNTRY, CNTRY, IDSCHOOL, IDTEACH,
         all_of(iv), all_of(dv)) %>%
  filter(CNTRY == "AUS") %>%
  mutate(across(all_of(dv), ~ ifelse(. > 4, NA, .))) %>%
  mutate(across(all_of(iv), ~ ifelse(. %in% c(98, 99), NA, .))) %>%
  mutate(ivmean = rowMeans(across(all_of(iv)), na.rm = TRUE)) %>%
  pivot_longer(
    cols      = all_of(dv),
    names_to  = "item",
    values_to = "response"
  )

data_listwise <- data_long %>%
  group_by(IDSCHOOL, IDTEACH) %>%
  filter(!any(is.na(response)), !is.na(ivmean)) %>%   # also drop missing iv
  ungroup()


data_avg <- data_listwise %>%
  group_by(IDSCHOOL, IDTEACH) %>%
  summarise(
    dvmean = round(mean(response), 4),
    ivmean = first(ivmean),
    .groups = "drop") %>%
  mutate(
    iv_school = mean(ivmean),                   
    iv_within = ivmean - iv_school              
  ) %>%
  ungroup()

```


Model1 null
```{r model 1}
m0 <- lmer(dvmean ~ 1 + (1|IDSCHOOL), data = data_avg, REML = TRUE)  # null
summary(m0)
```
Response variable name: dvmean (Null)
Level 1 variance: 0.5466
Level 2 variance: 0.08514
ICC: 0.0370 / (0.0370 + 0.6034) = 0.1348

Model 2
```{r model 2 no covariates}
model2_null <- data_avg %>%
  select(IDSCHOOL, IDTEACH, disruption_mean = dvmean) %>%
  group_by(IDSCHOOL) %>%
  arrange(IDSCHOOL) %>%
  as.data.frame()

write.csv(model2_null, "~/Downloads/blozislab/talis_aus_test_mod2_null.csv",
          row.names = FALSE, quote = FALSE)
```

```{r}
res <- read.csv("~/Downloads/blozislab/MELS_blozis_aus_test_null.csv")
p <- setNames(res$estimate, res$parameter)

var_b <- exp(p["alp0"])^2                                # between schools
var_w <- exp(p["delta_U_(Intercept)"] + exp(p["logSa"]) / 2)   # within schools

var_b
var_w
var_b / (var_b + var_w)
```
Null
Level 1 variance: 0.5496
Level 2 variance: 0.08464  
ICC: 0.08464 / (0.5496 + 0.08464) = 0.1334


model 3
```{r}
ord_long <- data_listwise %>%
  select(IDSCHOOL, IDTEACH, item, y = response) %>%
  arrange(IDSCHOOL, IDTEACH, item)

write_csv(ord_long, "~/Downloads/blozislab/talis_aus_ordinal.csv")
```

Level 1 variance: 0.4907
Level 2 variance: 3.369  
ICC: 0.4907 / (0.4907 + 3.369) = 0.1271


# Summary Tables
### Multilevel Model Summary Table: Brazil

1. Data set: TALIS OECD 2024
2. Name of construct/s that you chose to analyze: Teachers' report of classroom disruption levels
3. Variable names of individual items that measure the construct:
      a. TT4G54A TT4G54B TT4G54C TT4G54D
      b. dvmean = average(TT4G54A TT4G54B TT4G54C TT4G54D)
4. Sample size at level 2: 486
5. Sample size at level 1: 6003

```{r model-table, echo=FALSE}
model_data <- data.frame(
  Model = c("1", "2", "3"),
  `Response variable name` = c("dvmean", "dvmean",
                               "TT4G54A, TT4G54B, TT4G54C, TT4G54D"),
  `Level 2 variance`      = c("0.07775", "0.08149", "0.49071"),
  `Level 1 variance`      = c("0.47618", "0.46658", "3.36867"),
  `Random scale variance` = c("-", "0.03033", "0.15524"),
  ICC                     = c("0.14036", "0.14869", "0.12715"),
  check.names = FALSE
)

knitr::kable(model_data, align = rep('l', 6))
```

### Multilevel Model Summary Table: Japan

1. Data set: TALIS OECD 2024
2. Name of construct/s that you chose to analyze: Teachers' report of classroom disruption levels
3. Variable names of individual items that measure the construct:
      a. TT4G54A TT4G54B TT4G54C TT4G54D
      b. dvmean = average(TT4G54A TT4G54B TT4G54C TT4G54D)
4. Sample size at level 2: 403
5. Sample size at level 1: 6914	

```{r model-table, echo=FALSE}
model_data <- data.frame(
  Model = c("1", "2", "3"),
  `Response variable name` = c("dvmean", "dvmean",
                               "TT4G54A, TT4G54B, TT4G54C, TT4G54D"),
  `Level 2 variance`      = c("0.02727", "0.02006", "0.80888"),
  `Level 1 variance`      = c("0.25709", "0.27306", "5.38130"),
  `Random scale variance` = c("-",       "1.00568", "0.05906"),
  ICC                     = c("0.09590", "0.06843", "0.13067"),
  check.names = FALSE
)

knitr::kable(model_data, align = rep('l', 6))
```


### Multilevel Model Summary Table: Australia

1. Data set: TALIS OECD 2024
2. Name of construct/s that you chose to analyze: Teachers' report of classroom disruption levels
3. Variable names of individual items that measure the construct:
      a. TT4G54A TT4G54B TT4G54C TT4G54D
      b. dvmean = average(TT4G54A TT4G54B TT4G54C TT4G54D)
4. Sample size at level 2: 357
5. Sample size at level 1: 6038	

```{r model-table, echo=FALSE}
model_data <- data.frame(
  Model = c("1", "2", "3"),
  `Response variable name` = c("dvmean", "dvmean",
                               "TT4G54A, TT4G54B, TT4G54C, TT4G54D"),
  `Level 2 variance`      = c("0.08514", "0.08463", "1.21070"),
  `Level 1 variance`      = c("0.54656", "0.54967", "5.73853"),
  `Random scale variance` = c("-",       "0.20993", "0.03241"),
  ICC                     = c("0.13478", "0.13343", "0.17422"),
  check.names = FALSE
)

knitr::kable(model_data, align = rep('l', 6))
```



### Multilevel Model Summary Table: USA

1. Data set: TALIS OECD 2024
2. Name of construct/s that you chose to analyze: Teachers' report of classroom disruption levels
3. Variable names of individual items that measure the construct:
      a. TT4G54A TT4G54B TT4G54C TT4G54D
      b. dvmean = average(TT4G54A TT4G54B TT4G54C TT4G54D)
4. Sample size at level 2: 14
5. Sample size at level 1: 1988	

```{r model-table, echo=FALSE}
model_data <- data.frame(
  Model = c("1", "2", "3"),
  `Response variable name` = c("dvmean", "dvmean",
                               "TT4G54A, TT4G54B, TT4G54C, TT4G54D"),
  `Level 2 variance`      = c("0.08514", "0.08463", "1.21070"),
  `Level 1 variance`      = c("0.54656", "0.54967", "5.73853"),
  `Random scale variance` = c("-",       "0.20993", "0.03241"),
  ICC                     = c("0.13478", "0.13343", "0.17422"),
  check.names = FALSE
)

knitr::kable(model_data, align = rep('l', 6))
```

