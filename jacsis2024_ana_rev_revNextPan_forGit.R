library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)

# install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize) 

# install.packages("umap")
# install.packages("uwot")
library(uwot)

# install.packages("gower")
library(gower)

# install.packages("Rtsne")
library(Rtsne)

library(cluster)
library(umap)

# install.packages("VGAM")
library(VGAM)

library(MASS)

# Load CSV data
### Data should be saved in UTF-8 CSV in EXCEL
# load data ### This is just an example. Not real data!!!
load("jacsis_git_example2024.RData")

# Filter data for valid IDs
data <- data %>% filter(Monitor_ID %in% valid_ids$V1)

data <- left_join(data, pastinf, by = c("Monitor_ID" = "Monitor_ID_2024_2_jac"))

######### ORs #########
### Ikou by Q42.1
data <- data %>%
  mutate(ikou = case_when(
    data$Q39 == 3 ~ NA,
    data$Q42.1 == 1 ~ 1,
    data$Q42.1 == 2 ~ 1,
    data$Q42.1 == 3 ~ 0,
    data$Q42.1 == 4 ~ 0,
    TRUE ~ NA))

###### Demographic ######

### Age
# make group
data <- data %>%
  mutate(nenrei = case_when(
    AGE < 20 ~ "10",
    AGE >= 20 & AGE < 30 ~ "20",
    AGE >= 30 & AGE < 40 ~ "30",
    AGE >= 40 & AGE < 50 ~ "40",
    AGE >= 50 & AGE < 60 ~ "50",
    AGE >= 60 & AGE < 70 ~ "60",
    AGE >= 70 & AGE < 80 ~ "70",
    AGE >= 80 ~ "80",
    TRUE ~ NA))

table(data$nenrei)
table(data$nenrei, data$ikou)

# Copy to clipboard
temp <- table(data$nenrei, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$nenrei <- as.factor(data$nenrei)
data$nenrei <- relevel(data$nenrei, ref = "50")

# Binomial logistic regression for ikou
model <- glm(ikou ~ nenrei, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Sex
# make age group
table(data$SEX)
table(data$SEX, data$ikou)

# Copy to clipboard
temp <- table(data$SEX, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$SEX <- as.factor(data$SEX)
data$SEX <- relevel(data$SEX, ref = "1")

# Binomial logistic regression for ikou
model <- glm(ikou ~ SEX, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Income
# make group
data <- data %>%
  mutate(nenshu = case_when(
    Q80.1 <= 4 ~ "0",
    Q80.1 >= 5 & Q80.1 <= 6 ~ "200",
    Q80.1 >= 7 & Q80.1 <= 8 ~ "400",
    Q80.1 >= 9 & Q80.1 <= 10 ~ "600",
    Q80.1 >= 11 & Q80.1 <= 12 ~ "800",
    Q80.1 >= 13 & Q80.1 <= 18 ~ "1000",
    Q80.1 >= 19 & Q80.1 <= 20 ~ "other",
    TRUE ~ NA))

table(data$nenshu)
table(data$nenshu, data$ikou)

# Copy to clipboard
data$nenshu <- factor(data$nenshu, levels=c("0", "200", "400", "600", "800", "1000", "other"))

table(data$nenshu, data$ikou)
temp <- table(data$nenshu, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$nenshu <- relevel(data$nenshu, ref = "200")

# Binomial logistic regression for ikou
model <- glm(ikou ~ nenshu, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### School
# make group
data <- data %>%
  mutate(gakureki = case_when(
    Q5.1 == 12 | Q5.1 == 13 ~ "Now",
    Q21.1 == 1 ~ "Junior",
    Q21.1 == 2 | Q21.1 == 3 ~ "High",
    Q21.1 == 4 | Q21.1 == 5 ~ "Tech",
    Q21.1 >= 6 & Q21.1 <= 8 ~ "Univ",
    Q21.1 == 9 ~ "Grad",
    TRUE ~ "other"))

table(data$gakureki)
table(data$gakureki, data$ikou)

# Copy to clipboard
data$gakureki <- factor(data$gakureki, levels=c("Now", "Junior", "High", "Tech", "Univ", "Grad", "other"))

table(data$gakureki, data$ikou)
temp <- table(data$gakureki, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$gakureki <- relevel(data$gakureki, ref = "High")

# Binomial logistic regression for ikou
model <- glm(ikou ~ gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

###### Adjustment ######
model <- glm(ikou ~ nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Job
# make group
data <- data %>%
  mutate(shigoto = case_when(
    Q5.1 == 12 | Q5.1 == 13 ~ "student",
    Q5.1 == 15 ~ "wife",
    Q5.1 == 14 | Q5.1 == 16 ~ "other",
    Q6 == 15 | Q6 == 16 ~ "med",
    Q11 == 2 ~ "talk",
    TRUE ~ "nontalk"))

table(data$shigoto)
table(data$shigoto, data$ikou)

# Copy to clipboard
data$shigoto <- factor(data$shigoto, levels=c("med", "talk", "nontalk", "wife", "student", "other"))

table(data$shigoto, data$ikou)
temp <- table(data$shigoto, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$shigoto <- relevel(data$shigoto, ref = "nontalk")

# Binomial logistic regression for ikou
model <- glm(ikou ~ shigoto, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ shigoto + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Living alone
# make group
data <- data %>%
  mutate(hitori = case_when(
    Q1.1 == 1 ~ "0",
    TRUE ~ "1"))

table(data$hitori)
table(data$hitori, data$ikou)

# Copy to clipboard
temp <- table(data$hitori, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level

# Binomial logistic regression for ikou
model <- glm(ikou ~ hitori, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ hitori + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Married
# make group
data <- data %>%
  mutate(kekkon = case_when(
    Q2 <= 3 ~ "1",
    TRUE ~ "0"))

table(data$kekkon)
table(data$kekkon, data$ikou)

# Copy to clipboard
temp <- table(data$kekkon, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level

# Binomial logistic regression for ikou
model <- glm(ikou ~ kekkon, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ kekkon + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Contact
# make group
data <- data %>%
  mutate(kouryu = case_when(
    Q19.1 <= 3 | Q19.2 <= 3 |Q19.3 <= 3 |Q19.4 <= 3 |Q19.5 <= 3 | Q19.6 <= 3 ~ "1",
    TRUE ~ "0"))

table(data$kouryu)
table(data$kouryu, data$ikou)

# Copy to clipboard
temp <- table(data$kouryu, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level

# Binomial logistic regression for ikou
model <- glm(ikou ~ kouryu, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ kouryu + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Child contact
# make group
data <- data %>%
  mutate(kodomo = case_when(
    Q19.1 ==  6 ~ "0",
    TRUE ~ "1"))

table(data$kodomo)
table(data$kodomo, data$ikou)

# Copy to clipboard
temp <- table(data$kodomo, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level

# Binomial logistic regression for ikou
model <- glm(ikou ~ kodomo, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ kodomo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Elder contact
# make group
data <- data %>%
  mutate(roujin = case_when(
    Q19.6 ==  6 ~ "0",
    TRUE ~ "1"))

table(data$roujin)
table(data$roujin, data$ikou)

# Copy to clipboard
temp <- table(data$roujin, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level

# Binomial logistic regression for ikou
model <- glm(ikou ~ roujin, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ roujin + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Anxiety
# make group
data <- data %>%
  mutate(shinpai = case_when(
    Q79.4 == 1 ~ "1",
    Q79.4 == 2 | Q79.4 == 3 ~ "2",
    Q79.4 == 4 ~ "3",
    Q79.4 == 5 | Q79.4 == 6 ~ "4",
    Q79.4 == 7 ~ "5"))

table(data$shinpai)
table(data$shinpai, data$ikou)

# Copy to clipboard
temp <- table(data$shinpai, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$shinpai <- as.factor(data$shinpai)
data$shinpai <- relevel(data$shinpai, ref = "3")

# Binomial logistic regression for ikou
model <- glm(ikou ~ shinpai, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ shinpai + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Kyodo/Sado
kyodo <- data$Q29.1 + data$Q29.5 + data$Q29.9 + data$Q29.13 + data$Q29.17 + data$Q29.21
data$kyodo <- ifelse(kyodo >= 19, 1, 0)

sado <- data$Q29.2 + data$Q29.6 + data$Q29.10 + data$Q29.14 + data$Q29.18 + data$Q29.22
data$sado <- ifelse(sado >= 16, 1, 0)

ext_kyodo <- data$Q29.3 + data$Q29.7 + data$Q29.11 + data$Q29.15 + data$Q29.19 + data$Q29.23
data$ext_kyodo <- ifelse(ext_kyodo >= 17, 1, 0)

ext_sado <- data$Q29.4 + data$Q29.8 + data$Q29.12 + data$Q29.16 + data$Q29.20 + data$Q29.24
data$ext_sado <- ifelse(ext_sado >= 13, 1, 0)

temp <- table(data$kyodo, data$ikou)
temp <- rbind(temp, table(data$ext_kyodo, data$ikou))
temp <- rbind(temp, table(data$sado, data$ikou))
temp <- rbind(temp, table(data$ext_sado, data$ikou))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression for ikou
model01 <- glm(ikou ~ kyodo, data = data, family = binomial)
model02 <- glm(ikou ~ ext_kyodo, data = data, family = binomial)
model03 <- glm(ikou ~ sado, data = data, family = binomial)
model04 <- glm(ikou ~ ext_sado, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))

temp <- rbind(temp01, temp02, temp03, temp04)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# 
# temp <- rbind(temp01, temp02, temp03, temp04)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
# Binomial logistic regression for ikou
model01 <- glm(ikou ~ kyodo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model02 <- glm(ikou ~ ext_kyodo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model03 <- glm(ikou ~ sado + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model04 <- glm(ikou ~ ext_sado + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))

temp <- rbind(temp01, temp02, temp03, temp04)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# 
# temp <- rbind(temp01, temp02, temp03, temp04)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Happy
# make group
data <- data %>%
  mutate(shiawase = case_when(
    Q76.2 == 1 | Q76.2 == 2 ~ "1",
    Q76.2 == 3 | Q76.2 == 4 ~ "2",
    Q76.2 == 5 | Q76.2 == 6 | Q76.2 == 7 ~ "3",
    Q76.2 == 8 | Q76.2 == 9 ~ "4",
    Q76.2 == 10 | Q76.2 == 11 ~ "5"))

table(data$shiawase)
table(data$shiawase, data$ikou)

# Copy to clipboard
temp <- table(data$shiawase, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$shiawase <- as.factor(data$shiawase)
data$shiawase <- relevel(data$shiawase, ref = "3")

# Binomial logistic regression for ikou
model <- glm(ikou ~ shiawase, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ shiawase + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Information
# make group
data <- data %>%
  mutate(info01 = case_when(
    Q62.1 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info02 = case_when(
    Q62.2 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info03 = case_when(
    Q62.3 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info04 = case_when(
    Q62.4 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info05 = case_when(
    Q62.5 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info06 = case_when(
    Q62.6 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info07 = case_when(
    Q62.7 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info08 = case_when(
    Q62.8 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info09 = case_when(
    Q62.9 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info10 = case_when(
    Q62.10 == 2 & Q62.11 == 2 & Q62.12 == 2 & Q62.13 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info14 = case_when(
    Q62.14 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info15 = case_when(
    Q62.15 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info16 = case_when(
    Q62.16 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info17 = case_when(
    Q62.17 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info18 = case_when(
    Q62.18 == 2 & Q62.19 == 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(info20 = case_when(
    Q62.20 == 2 ~ "0",
    TRUE ~ "1"))

temp <- table(data$info01, data$ikou)
temp <- rbind(temp, table(data$info02, data$ikou))
temp <- rbind(temp, table(data$info03, data$ikou))
temp <- rbind(temp, table(data$info04, data$ikou))
temp <- rbind(temp, table(data$info05, data$ikou))
temp <- rbind(temp, table(data$info06, data$ikou))
temp <- rbind(temp, table(data$info07, data$ikou))
temp <- rbind(temp, table(data$info08, data$ikou))
temp <- rbind(temp, table(data$info09, data$ikou))
temp <- rbind(temp, table(data$info10, data$ikou))
temp <- rbind(temp, table(data$info14, data$ikou))
temp <- rbind(temp, table(data$info15, data$ikou))
temp <- rbind(temp, table(data$info16, data$ikou))
temp <- rbind(temp, table(data$info17, data$ikou))
temp <- rbind(temp, table(data$info18, data$ikou))
temp <- rbind(temp, table(data$info20, data$ikou))

# Copy to clipboard
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression for ikou
model01 <- glm(ikou ~ info01, data = data, family = binomial)
model02 <- glm(ikou ~ info02, data = data, family = binomial)
model03 <- glm(ikou ~ info03, data = data, family = binomial)
model04 <- glm(ikou ~ info04, data = data, family = binomial)
model05 <- glm(ikou ~ info05, data = data, family = binomial)
model06 <- glm(ikou ~ info06, data = data, family = binomial)
model07 <- glm(ikou ~ info07, data = data, family = binomial)
model08 <- glm(ikou ~ info08, data = data, family = binomial)
model09 <- glm(ikou ~ info09, data = data, family = binomial)
model10 <- glm(ikou ~ info10, data = data, family = binomial)
model14 <- glm(ikou ~ info14, data = data, family = binomial)
model15 <- glm(ikou ~ info15, data = data, family = binomial)
model16 <- glm(ikou ~ info16, data = data, family = binomial)
model17 <- glm(ikou ~ info17, data = data, family = binomial)
model18 <- glm(ikou ~ info18, data = data, family = binomial)
model20 <- glm(ikou ~ info20, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))
temp10 <- exp(coef(model10))
temp14 <- exp(coef(model14))
temp15 <- exp(coef(model15))
temp16 <- exp(coef(model16))
temp17 <- exp(coef(model17))
temp18 <- exp(coef(model18))
temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# temp07 <- exp(confint(model07))
# temp08 <- exp(confint(model08))
# temp09 <- exp(confint(model09))
# temp10 <- exp(confint(model10))
# temp14 <- exp(confint(model14))
# temp15 <- exp(confint(model15))
# temp16 <- exp(confint(model16))
# temp17 <- exp(confint(model17))
# temp18 <- exp(confint(model18))
# temp20 <- exp(confint(model20))
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]
temp07 <- summary(model07)$coefficients[, 4]
temp08 <- summary(model08)$coefficients[, 4]
temp09 <- summary(model09)$coefficients[, 4]
temp10 <- summary(model10)$coefficients[, 4]
temp14 <- summary(model14)$coefficients[, 4]
temp15 <- summary(model15)$coefficients[, 4]
temp16 <- summary(model16)$coefficients[, 4]
temp17 <- summary(model17)$coefficients[, 4]
temp18 <- summary(model18)$coefficients[, 4]
temp20 <- summary(model20)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
# Binomial logistic regression for ikou
model01 <- glm(ikou ~ info01 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model02 <- glm(ikou ~ info02 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model03 <- glm(ikou ~ info03 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model04 <- glm(ikou ~ info04 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model05 <- glm(ikou ~ info05 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model06 <- glm(ikou ~ info06 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model07 <- glm(ikou ~ info07 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model08 <- glm(ikou ~ info08 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model09 <- glm(ikou ~ info09 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model10 <- glm(ikou ~ info10 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model14 <- glm(ikou ~ info14 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model15 <- glm(ikou ~ info15 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model16 <- glm(ikou ~ info16 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model17 <- glm(ikou ~ info17 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model18 <- glm(ikou ~ info18 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model20 <- glm(ikou ~ info20 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))
temp10 <- exp(coef(model10))
temp14 <- exp(coef(model14))
temp15 <- exp(coef(model15))
temp16 <- exp(coef(model16))
temp17 <- exp(coef(model17))
temp18 <- exp(coef(model18))
temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# temp07 <- exp(confint(model07))
# temp08 <- exp(confint(model08))
# temp09 <- exp(confint(model09))
# temp10 <- exp(confint(model10))
# temp14 <- exp(confint(model14))
# temp15 <- exp(confint(model15))
# temp16 <- exp(confint(model16))
# temp17 <- exp(confint(model17))
# temp18 <- exp(confint(model18))
# temp20 <- exp(confint(model20))
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]
temp07 <- summary(model07)$coefficients[, 4]
temp08 <- summary(model08)$coefficients[, 4]
temp09 <- summary(model09)$coefficients[, 4]
temp10 <- summary(model10)$coefficients[, 4]
temp14 <- summary(model14)$coefficients[, 4]
temp15 <- summary(model15)$coefficients[, 4]
temp16 <- summary(model16)$coefficients[, 4]
temp17 <- summary(model17)$coefficients[, 4]
temp18 <- summary(model18)$coefficients[, 4]
temp20 <- summary(model20)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Past infection
# make group

temp <- table(data$inf_survey_2020_2_jac, data$ikou)
temp <- rbind(temp, table(data$inf_survey_2021_1_jas, data$ikou))
temp <- rbind(temp, table(data$inf_survey_2021_2_jac, data$ikou))
temp <- rbind(temp, table(data$inf_survey_2022_1_jas, data$ikou))
temp <- rbind(temp, table(data$inf_survey_2022_2_jac, data$ikou))
temp <- rbind(temp, table(data$inf_survey_2023_1_jas, data$ikou))
# temp <- rbind(temp, table(data$inf_survey_2023_2_jac, data$ikou))
# temp <- rbind(temp, table(data$inf_survey_2024_1_jas, data$ikou))
# temp <- rbind(temp, table(data$inf_survey_2024_2_jac, data$ikou))
temp <- rbind(temp, table(data$inf_2020_all, data$ikou))
temp <- rbind(temp, table(data$inf_2021_all, data$ikou))
temp <- rbind(temp, table(data$inf_2022_all, data$ikou))
# temp <- rbind(temp, table(data$inf_2023_all, data$ikou))
# temp <- rbind(temp, table(data$inf_2024_all, data$ikou))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression for ikou
model01 <- glm(ikou ~ inf_survey_2020_2_jac, data = data, family = binomial)
model02 <- glm(ikou ~ inf_survey_2021_1_jas, data = data, family = binomial)
model03 <- glm(ikou ~ inf_survey_2021_2_jac, data = data, family = binomial)
model04 <- glm(ikou ~ inf_survey_2022_1_jas, data = data, family = binomial)
model05 <- glm(ikou ~ inf_survey_2022_2_jac, data = data, family = binomial)
model06 <- glm(ikou ~ inf_survey_2023_1_jas, data = data, family = binomial)
model07 <- glm(ikou ~ inf_2020_all, data = data, family = binomial)
model08 <- glm(ikou ~ inf_2021_all, data = data, family = binomial)
model09 <- glm(ikou ~ inf_2022_all, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# temp07 <- exp(confint(model07))
# temp08 <- exp(confint(model08))
# temp09 <- exp(confint(model09))
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]
temp07 <- summary(model07)$coefficients[, 4]
temp08 <- summary(model08)$coefficients[, 4]
temp09 <- summary(model09)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
# Binomial logistic regression for ikou
model01 <- glm(ikou ~ inf_survey_2020_2_jac + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model02 <- glm(ikou ~ inf_survey_2021_1_jas + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model03 <- glm(ikou ~ inf_survey_2021_2_jac + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model04 <- glm(ikou ~ inf_survey_2022_1_jas + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model05 <- glm(ikou ~ inf_survey_2022_2_jac + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model06 <- glm(ikou ~ inf_survey_2023_1_jas + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model07 <- glm(ikou ~ inf_2020_all + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model08 <- glm(ikou ~ inf_2021_all + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model09 <- glm(ikou ~ inf_2022_all + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# temp07 <- exp(confint(model07))
# temp08 <- exp(confint(model08))
# temp09 <- exp(confint(model09))
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]
temp07 <- summary(model07)$coefficients[, 4]
temp08 <- summary(model08)$coefficients[, 4]
temp09 <- summary(model09)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Adv reaction
# make group

temp <- table(data$adv1, data$ikou)
temp <- rbind(temp, table(data$adv2, data$ikou))

# Level
data$adv1 <- as.factor(data$adv1)
data$adv1 <- relevel(data$adv1, ref = "0")

data$adv2 <- as.factor(data$adv2)
data$adv2 <- relevel(data$adv2, ref = "0")

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression for ikou
model01 <- glm(ikou ~ adv1, data = data, family = binomial)
model02 <- glm(ikou ~ adv2, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))

temp <- rbind(temp01, temp02)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# 
# temp <- rbind(temp01, temp02)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]

temp <- rbind(temp01, temp02)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
# Binomial logistic regression for ikou
model01 <- glm(ikou ~ adv1 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model02 <- glm(ikou ~ adv2 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))

temp <- rbind(temp01, temp02)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# 
# temp <- rbind(temp01, temp02)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]

temp <- rbind(temp01, temp02)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Medical condition
# make group
data <- data %>%
  mutate(med01 = case_when(
    Q46.1 <= 2 &  Q46.2 <= 2 & Q46.3 <= 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(med02 = case_when(
    Q46.4 <= 2 &  Q46.5 <= 2 & Q46.13 <= 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(med03 = case_when(
    Q46.11 <= 2 &  Q46.12 <= 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(med04 = case_when(
    Q46.14 <= 2 &  Q46.15 <= 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(med05 = case_when(
    Q46.16 <= 2 ~ "0",
    TRUE ~ "1"))

data <- data %>%
  mutate(med06 = case_when(
    Q46.19 <= 2 &  Q46.20 <= 2 ~ "0",
    TRUE ~ "1"))

temp <- table(data$med01, data$ikou)
temp <- rbind(temp, table(data$med02, data$ikou))
temp <- rbind(temp, table(data$med03, data$ikou))
temp <- rbind(temp, table(data$med04, data$ikou))
temp <- rbind(temp, table(data$med05, data$ikou))
temp <- rbind(temp, table(data$med06, data$ikou))

# Copy to clipboard
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression for ikou
model01 <- glm(ikou ~ med01, data = data, family = binomial)
model02 <- glm(ikou ~ med02, data = data, family = binomial)
model03 <- glm(ikou ~ med03, data = data, family = binomial)
model04 <- glm(ikou ~ med04, data = data, family = binomial)
model05 <- glm(ikou ~ med05, data = data, family = binomial)
model06 <- glm(ikou ~ med06, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
# Binomial logistic regression for ikou
model01 <- glm(ikou ~ med01 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model02 <- glm(ikou ~ med02 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model03 <- glm(ikou ~ med03 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model04 <- glm(ikou ~ med04 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model05 <- glm(ikou ~ med05 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model06 <- glm(ikou ~ med06 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Regular doctor
# make group
data <- data %>%
  mutate(kakari = case_when(
    Q68.1 == 2 ~ "0",
    Q68.1 == 1 ~ "1",
    TRUE ~ NA))

table(data$kakari)
table(data$kakari, data$ikou)

# Copy to clipboard
temp <- table(data$kakari, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level

# Binomial logistic regression for ikou
model <- glm(ikou ~ kakari, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ kakari + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### COVID fear
# make group
data$koronafuan <- data$Q61.1

table(data$koronafuan)
table(data$koronafuan, data$ikou)

# Copy to clipboard
temp <- table(data$koronafuan, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$koronafuan <- as.factor(data$koronafuan)
data$koronafuan <- relevel(data$koronafuan, ref = "3")

# Binomial logistic regression for ikou
model <- glm(ikou ~ koronafuan, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ koronafuan + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Vaccine misinformation
# make group
# count
data$Q35.1_count <- ifelse(data$Q35.1 >= 4, 1, 0) 
data$Q35.2_count <- ifelse(data$Q35.2 >= 4, 1, 0)
data$Q35.3_count <- ifelse(data$Q35.3 >= 4, 1, 0)
data$Q35.4_count <- ifelse(data$Q35.4 >= 4, 1, 0)
data$Q35.5_count <- ifelse(data$Q35.5 >= 4, 1, 0)
data$Q35.6_count <- ifelse(data$Q35.6 >= 4, 1, 0)
data$Q35.7_count <- ifelse(data$Q35.7 >= 4, 1, 0)

data$temp <- data$Q35.1_count + data$Q35.2_count + data$Q35.3_count + 
  data$Q35.4_count + data$Q35.5_count + data$Q35.6_count + data$Q35.7_count

data <- data %>%
  mutate(vac_inbou = case_when(
    temp <= 0 ~ "0",
    temp <= 2 ~ "1",
    temp <= 4 ~ "2",
    temp <= 6 ~ "3",
    temp <= 7 ~ "4"))

table(data$vac_inbou)
table(data$vac_inbou, data$ikou)

# Copy to clipboard
temp <- table(data$vac_inbou, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level

# Binomial logistic regression for ikou
model <- glm(ikou ~ vac_inbou, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ vac_inbou + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### COVID conspiracy
# make group
# count
data$Q35.8_count <- ifelse(data$Q35.8 >= 4, 1, 0) 
data$Q35.9_count <- ifelse(data$Q35.9 >= 4, 1, 0)
data$Q35.10_count <- ifelse(data$Q35.10 >= 4, 1, 0)

data$temp <- data$Q35.8_count + data$Q35.9_count + data$Q35.10_count

data$korona_inbou <- data$temp

table(data$korona_inbou)
table(data$korona_inbou, data$ikou)

# Copy to clipboard
temp <- table(data$korona_inbou, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$korona_inbou <- as.factor(data$korona_inbou)
data$korona_inbou <- relevel(data$korona_inbou, ref = "0")

# Binomial logistic regression for ikou
model <- glm(ikou ~ korona_inbou, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ korona_inbou + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### ID knowledge
# make group
# count
data$Q36.1_count <- ifelse(data$Q36.1 == 1, 1, 0) 
data$Q36.2_count <- ifelse(data$Q36.2 == 1, 1, 0)
data$Q36.3_count <- ifelse(data$Q36.3 == 1, 1, 0)
data$Q36.4_count <- ifelse(data$Q36.4 == 1, 1, 0)
data$Q36.5_count <- ifelse(data$Q36.5 == 1, 1, 0)
data$Q36.6_count <- ifelse(data$Q36.6 == 1, 1, 0)
data$Q36.7_count <- ifelse(data$Q36.7 == 1, 1, 0)
data$Q36.8_count <- ifelse(data$Q36.8 == 1, 1, 0)
data$Q36.9_count <- ifelse(data$Q36.9 == 1, 1, 0)
data$Q36.10_count <- ifelse(data$Q36.10 == 1, 1, 0)
data$Q36.11_count <- ifelse(data$Q36.11 == 1, 1, 0)
data$Q36.12_count <- ifelse(data$Q36.12 == 1, 1, 0)
data$Q36.13_count <- ifelse(data$Q36.13 == 1, 1, 0)
data$Q36.14_count <- ifelse(data$Q36.14 == 1, 1, 0)
data$Q36.15_count <- ifelse(data$Q36.15 == 1, 1, 0)
data$Q36.16_count <- ifelse(data$Q36.16 == 1, 1, 0)
data$Q36.17_count <- ifelse(data$Q36.17 == 1, 1, 0)
data$Q36.18_count <- ifelse(data$Q36.18 == 1, 1, 0)
data$Q36.19_count <- ifelse(data$Q36.19 == 1, 1, 0)
data$Q36.20_count <- ifelse(data$Q36.20 == 1, 1, 0)

data$temp <- data$Q36.1_count + data$Q36.2_count + data$Q36.3_count + 
  data$Q36.4_count + data$Q36.5_count + data$Q36.6_count + 
  data$Q36.7_count + data$Q36.8_count + data$Q36.9_count + 
  data$Q36.10_count + data$Q36.11_count + data$Q36.12_count + 
  data$Q36.13_count + data$Q36.14_count + data$Q36.15_count + 
  data$Q36.16_count + data$Q36.17_count + data$Q36.18_count +
  data$Q36.19_count + data$Q36.20_count

data <- data %>%
  mutate(chishiki = case_when(
    temp <= 0 ~ "0",
    temp <= 10 ~ "1",
    TRUE ~ "2"))

table(data$chishiki)
table(data$chishiki, data$ikou)

# Copy to clipboard
temp <- table(data$chishiki, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$chishiki <- as.factor(data$chishiki)
data$chishiki <- relevel(data$chishiki, ref = "0")

# Binomial logistic regression for ikou
model <- glm(ikou ~ chishiki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ chishiki + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Measures
# make group
# count
data$Q58.1_count <- ifelse(data$Q58.1 <= 2, 1, 0)
data$Q58.2_count <- ifelse(data$Q58.2 <= 2, 1, 0)
data$Q58.3_count <- ifelse(data$Q58.3 <= 2, 1, 0)
data$Q58.4_count <- ifelse(data$Q58.4 <= 2, 1, 0)
data$Q58.5_count <- ifelse(data$Q58.5 <= 2, 1, 0)
data$Q58.6_count <- ifelse(data$Q58.6 <= 2, 1, 0)
data$Q58.7_count <- ifelse(data$Q58.7 <= 2, 1, 0)
data$Q58.8_count <- ifelse(data$Q58.8 <= 2, 1, 0)
data$Q58.9_count <- ifelse(data$Q58.9 <= 2, 1, 0)
data$Q58.10_count <- ifelse(data$Q58.10 <= 2, 1, 0)
data$Q58.11_count <- ifelse(data$Q58.11 <= 2, 1, 0)
data$Q58.12_count <- ifelse(data$Q58.12 <= 2, 1, 0)
data$Q58.13_count <- ifelse(data$Q58.13 <= 2, 1, 0)
data$Q58.14_count <- ifelse(data$Q58.14 <= 2, 1, 0)

data$temp <- data$Q58.1_count + data$Q58.2_count + data$Q58.3_count + 
  data$Q58.4_count + data$Q58.5_count + data$Q58.6_count + 
  data$Q58.7_count + data$Q58.8_count + data$Q58.9_count + 
  data$Q58.10_count + data$Q58.11_count + data$Q58.12_count + 
  data$Q58.13_count + data$Q58.14_count

data <- data %>%
  mutate(taisaku = case_when(
    temp <= 0 ~ "0",
    temp <= 7 ~ "1",
    TRUE ~ "2"))

table(data$taisaku)
table(data$taisaku, data$ikou)

# Copy to clipboard
temp <- table(data$taisaku, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$taisaku <- as.factor(data$taisaku)
data$taisaku <- relevel(data$taisaku, ref = "0")

# Binomial logistic regression for ikou
model <- glm(ikou ~ taisaku, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ taisaku + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Vaccine attitude
# make group
data <- data %>%
  mutate(taido01 = case_when(
    Q34.4 <= 3 ~ "1",
    Q34.4 == 4  ~ "2",
    TRUE ~ "3"))

data <- data %>%
  mutate(taido02 = case_when(
    Q34.5 <= 3 ~ "1",
    Q34.5 == 4  ~ "2",
    TRUE ~ "3"))

data <- data %>%
  mutate(taido03 = case_when(
    Q34.7 <= 3 ~ "1",
    Q34.7 == 4  ~ "2",
    TRUE ~ "3"))

data <- data %>%
  mutate(taido04 = case_when(
    Q34.8 <= 3 ~ "1",
    Q34.8 == 4  ~ "2",
    TRUE ~ "3"))

temp <- table(data$taido01, data$ikou)
temp <- rbind(temp, table(data$taido02, data$ikou))
temp <- rbind(temp, table(data$taido03, data$ikou))
temp <- rbind(temp, table(data$taido04, data$ikou))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
# data$taido01 <- as.factor(data$taido01)
# data$taido01 <- relevel(data$taido01, ref = "2")
# data$taido02 <- as.factor(data$taido02)
# data$taido02 <- relevel(data$taido02, ref = "2")
# data$taido03 <- as.factor(data$taido03)
# data$taido03 <- relevel(data$taido03, ref = "2")
# data$taido04 <- as.factor(data$taido04)
# data$taido04 <- relevel(data$taido04, ref = "2")

# Binomial logistic regression for ikou
model01 <- glm(ikou ~ taido01, data = data, family = binomial)
model02 <- glm(ikou ~ taido02, data = data, family = binomial)
model03 <- glm(ikou ~ taido03, data = data, family = binomial)
model04 <- glm(ikou ~ taido04, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))

temp <- rbind(temp01, temp02, temp03, temp04)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# 
# temp <- rbind(temp01, temp02, temp03, temp04)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
# Binomial logistic regression for ikou
model01 <- glm(ikou ~ taido01 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model02 <- glm(ikou ~ taido02 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model03 <- glm(ikou ~ taido03 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model04 <- glm(ikou ~ taido04 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))

temp <- rbind(temp01, temp02, temp03, temp04)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# 
# temp <- rbind(temp01, temp02, temp03, temp04)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Past vac 39
# make group
data <- data %>%
  mutate(korona_vac = case_when(
    Q39 == 1 ~ "1",
    Q39 == 2 ~ "2",
    Q39 == 4 |  Q39 == 5 ~ "45",
    TRUE ~ NA))

table(data$korona_vac)
table(data$korona_vac)

# Copy to clipboard
temp <- table(data$korona_vac, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$korona_vac <- as.factor(data$korona_vac)
data$korona_vac <- relevel(data$korona_vac, ref = "1")

# Binomial logistic regression for ikou
model <- glm(ikou ~ korona_vac, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ korona_vac + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Next pandemic 40
# make group
data <- data %>%
  mutate(next_pan = case_when(
    Q40 <= 3 ~ "1",
    Q40 == 4 ~ "4",
    Q40 == 5 ~ "5",
    Q40 == 6 ~ "6",
    Q40 == 7 ~ "7"))

table(data$next_pan)
table(data$next_pan, data$ikou)

# Copy to clipboard
temp <- table(data$next_pan, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$next_pan <- as.factor(data$next_pan)
data$next_pan <- relevel(data$next_pan, ref = "1")

# Binomial logistic regression for ikou
model <- glm(ikou ~ next_pan, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ next_pan + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Next Vac implementation when
# make group
data <- data %>%
  mutate(nextvac_when02 = case_when(
    Q41.2 == 1 ~ "1",
    Q41.2 == 2 ~ "2",
    Q41.2 == 3 ~ "3",
    Q41.2 == 4 ~ "4",
    Q41.2 == 5 ~ "5",
    Q41.2 == 6 ~ "6"))

table(data$nextvac_when02)
table(data$nextvac_when02, data$ikou)

# Copy to clipboard
temp <- table(data$nextvac_when02, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$nextvac_when02 <- as.factor(data$nextvac_when02)
data$nextvac_when02 <- relevel(data$nextvac_when02, ref = "3")

# Binomial logistic regression for ikou
model <- glm(ikou ~ nextvac_when02, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ nextvac_when02 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Info nothing
# make group
data <- data %>%
  mutate(infoNo = case_when(
    Q62.1 == 2 & Q62.2 == 2 & Q62.3 == 2 & Q62.4 == 2 & Q62.5 == 2 & Q62.6 == 2 & Q62.7 == 2 & Q62.8 == 2 & Q62.9 == 2 & Q62.10 == 2 & 
      Q62.11 == 2 & Q62.12 == 2 & Q62.13 == 2 & Q62.14 == 2 & Q62.15 == 2 & Q62.16 == 2 & Q62.17 == 2 & Q62.18 == 2 & Q62.19 == 2 & Q62.20 == 2 ~ "0",
    TRUE ~ "1"))

table(data$infoNo)
table(data$infoNo, data$ikou)

# Copy to clipboard
temp <- table(data$infoNo, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$infoNo <- as.factor(data$infoNo)
data$infoNo <- relevel(data$infoNo, ref = "1")

# Binomial logistic regression for ikou
model <- glm(ikou ~ infoNo, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- exp(confint(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ infoNo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# ALL Adjustment #
model <- glm(ikou ~ nenrei + SEX + nenshu + gakureki +
               shigoto + hitori + kekkon + kouryu + kodomo + roujin +
               shinpai + kyodo + ext_kyodo + sado + ext_sado + shiawase +
               info01 + info02 + info03 + info04 + info05 + info06 + info07 + info08 + info09 + info10 + info14 + info15 + info16 + info17 + info18 + info20 +
               inf_2021_all + adv1 + med01 + med02 + med03 + med04 +med05 + med06 + kakari + koronafuan + vac_inbou + korona_inbou + chishiki + next_pan,
             data = data, family = binomial)

model <- glm(ikou ~ nenrei + SEX + nenshu + gakureki +
               info01 + info02 + info03 + info04 + info05 + info06 + info07 + info08 + info09 + info10 + info14 + info15 + info16 + info17 + info18 + info20,
             data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

######### ORs for order #########
### NEW Ikou by Q42.1
data <- data %>%
  mutate(ikou = case_when(
    data$Q39 == 3 ~ NA,
    data$Q42.1 == 1 ~ 4,
    data$Q42.1 == 2 ~ 3,
    data$Q42.1 == 3 ~ 2,
    data$Q42.1 == 4 ~ 1,
    TRUE ~ NA))

# as order

data$ikou <- as.ordered(data$ikou)
unique(data$ikou)

###### Demographic ######

### Age
# make group
table(data$nenrei)
table(data$nenrei, data$ikou)

# Copy to clipboard
temp <- table(data$nenrei, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ nenrei, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Sex
table(data$SEX)
table(data$SEX, data$ikou)

# Copy to clipboard
temp <- table(data$SEX, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ SEX, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Income

table(data$nenshu, data$ikou)
temp <- table(data$nenshu, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ nenshu, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### School

table(data$gakureki, data$ikou)
temp <- table(data$gakureki, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

###### Adjustment ######
# Order logistic regression for ikou
model <- polr(ikou ~ nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Job

table(data$shigoto, data$ikou)
temp <- table(data$shigoto, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ shigoto, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ shigoto + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Living alone

temp <- table(data$hitori, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ hitori, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ hitori + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Married

temp <- table(data$kekkon, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ kekkon, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ kekkon + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Contact

temp <- table(data$kouryu, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ kouryu, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ kouryu + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Child contact

temp <- table(data$kodomo, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ kodomo, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ kodomo + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Elder contact

temp <- table(data$roujin, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ roujin, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ roujin + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Anxiety

temp <- table(data$shinpai, data$ikou)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ shinpai, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ shinpai + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Kyodo/Sado

temp <- table(data$kyodo, data$ikou)
temp <- rbind(temp, table(data$ext_kyodo, data$ikou))
temp <- rbind(temp, table(data$sado, data$ikou))
temp <- rbind(temp, table(data$ext_sado, data$ikou))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ kyodo, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- exp(confint(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ ext_kyodo, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- exp(confint(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ sado, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- exp(confint(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ ext_sado, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- exp(confint(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ kyodo + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ ext_kyodo + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ sado + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ ext_sado + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

### Happy

temp <- table(data$shiawase, data$ikou)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ shiawase, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ shiawase + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Information

temp <- table(data$info01, data$ikou)
temp <- rbind(temp, table(data$info02, data$ikou))
temp <- rbind(temp, table(data$info03, data$ikou))
temp <- rbind(temp, table(data$info04, data$ikou))
temp <- rbind(temp, table(data$info05, data$ikou))
temp <- rbind(temp, table(data$info06, data$ikou))
temp <- rbind(temp, table(data$info07, data$ikou))
temp <- rbind(temp, table(data$info08, data$ikou))
temp <- rbind(temp, table(data$info09, data$ikou))
temp <- rbind(temp, table(data$info10, data$ikou))
temp <- rbind(temp, table(data$info14, data$ikou))
temp <- rbind(temp, table(data$info15, data$ikou))
temp <- rbind(temp, table(data$info16, data$ikou))
temp <- rbind(temp, table(data$info17, data$ikou))
temp <- rbind(temp, table(data$info18, data$ikou))
temp <- rbind(temp, table(data$info20, data$ikou))

temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info01, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info02, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info03, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info04, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info05, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info06, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info07, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info08, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info09, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info10, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info14, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info15, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info16, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info17, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info18, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ info20, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ info01 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info02 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info03 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info04 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info05 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info06 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info07 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info08 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info09 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info10 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info14 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info15 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info16 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info17 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info18 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ info20 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################


### MULTI INFO ###
model <- polr(ikou ~ info01 + info02 + info03 + info04 + info05 + info06 + info07 + info08 + info09 + info10 + info14 + info15 + info16 + info17 + info18 + info20 +
                nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

### Past infection

temp <- table(data$inf_survey_2020_2_jac, data$ikou)
temp <- rbind(temp, table(data$inf_survey_2021_1_jas, data$ikou))
temp <- rbind(temp, table(data$inf_survey_2021_2_jac, data$ikou))
temp <- rbind(temp, table(data$inf_survey_2022_1_jas, data$ikou))
temp <- rbind(temp, table(data$inf_survey_2022_2_jac, data$ikou))
temp <- rbind(temp, table(data$inf_survey_2023_1_jas, data$ikou))
# temp <- rbind(temp, table(data$inf_survey_2023_2_jac, data$ikou))
# temp <- rbind(temp, table(data$inf_survey_2024_1_jas, data$ikou))
# temp <- rbind(temp, table(data$inf_survey_2024_2_jac, data$ikou))
temp <- rbind(temp, table(data$inf_2020_all, data$ikou))
temp <- rbind(temp, table(data$inf_2021_all, data$ikou))
temp <- rbind(temp, table(data$inf_2022_all, data$ikou))
# temp <- rbind(temp, table(data$inf_2023_all, data$ikou))
# temp <- rbind(temp, table(data$inf_2024_all, data$ikou))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ inf_2021_all, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ inf_2021_all + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Adv reaction

temp <- table(data$adv1, data$ikou)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ adv1, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ adv1 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Medical condition

temp <- table(data$med01, data$ikou)
temp <- rbind(temp, table(data$med02, data$ikou))
temp <- rbind(temp, table(data$med03, data$ikou))
temp <- rbind(temp, table(data$med04, data$ikou))
temp <- rbind(temp, table(data$med05, data$ikou))
temp <- rbind(temp, table(data$med06, data$ikou))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ med01, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ med02, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ med03, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ med04, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ med05, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ med06, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ med01 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ med02 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ med03 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ med04 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ med05 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

model <- polr(ikou ~ med06 + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)
##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################

### Regular doctor

temp <- table(data$kakari, data$ikou)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ kakari, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ kakari + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### COVID fear

temp <- table(data$koronafuan, data$ikou)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ koronafuan, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ koronafuan + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Vaccine misinformation
temp <- table(data$vac_inbou, data$ikou)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ vac_inbou, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ vac_inbou + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### COVID conspiracy

temp <- table(data$korona_inbou, data$ikou)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ korona_inbou, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ korona_inbou + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### ID knowledge

temp <- table(data$chishiki, data$ikou)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ chishiki, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ chishiki + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Next pandemic 40
temp <- table(data$next_pan, data$ikou)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Order logistic regression for ikou
model <- polr(ikou ~ next_pan, data = data, Hess = TRUE)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# P-value
ctable <- coef(summary(model))
p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
p_values
write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- polr(ikou ~ next_pan + nenrei + SEX + nenshu + gakureki, data = data, Hess = TRUE)

# OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

##### alternative #####
# 回帰係数
b <- coef(model)

# 標準誤差
se <- sqrt(diag(vcov(model)))[names(b)]

# Wald z value
z <- b / se

# p-value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# OR and 95% CI
OR <- exp(b)
lower <- exp(b - 1.96 * se)
upper <- exp(b + 1.96 * se)

result <- data.frame(
  variable = names(b),
  OR = OR,
  lower95 = lower,
  upper95 = upper,
  p_value = p
)

result
write.table(result, "clipboard", sep = "\t", row.names = FALSE)
##################################
# P-value
# ctable <- coef(summary(model))
# p_values <- 2 * pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)
# p_values
# write.table(p_values, "clipboard", sep = "\t", row.names = FALSE)

### Next Vac implementation when
# make group
data <- data %>%
  mutate(nextvac_when02 = case_when(
    Q41.2 == 1 ~ "1",
    Q41.2 == 2 ~ "2",
    Q41.2 == 3 ~ "3",
    Q41.2 == 4 ~ "4",
    Q41.2 == 5 ~ "5",
    Q41.2 == 6 ~ "6"))

table(data$nextvac_when02)
table(data$nextvac_when02, data$ikou)

# Copy to clipboard
temp <- table(data$nextvac_when02, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$nextvac_when02 <- as.factor(data$nextvac_when02)
data$nextvac_when02 <- relevel(data$nextvac_when02, ref = "3")

# Binomial logistic regression for ikou
model <- glm(ikou ~ nextvac_when02, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ nextvac_when02 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Info nothing
# make group
data <- data %>%
  mutate(infoNo = case_when(
    Q62.1 == 2 & Q62.2 == 2 & Q62.3 == 2 & Q62.4 == 2 & Q62.5 == 2 & Q62.6 == 2 & Q62.7 == 2 & Q62.8 == 2 & Q62.9 == 2 & Q62.10 == 2 & 
      Q62.11 == 2 & Q62.12 == 2 & Q62.13 == 2 & Q62.14 == 2 & Q62.15 == 2 & Q62.16 == 2 & Q62.17 == 2 & Q62.18 == 2 & Q62.19 == 2 & Q62.20 == 2 ~ "0",
    TRUE ~ "1"))

table(data$infoNo)
table(data$infoNo, data$ikou)

# Copy to clipboard
temp <- table(data$infoNo, data$ikou)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$infoNo <- as.factor(data$infoNo)
data$infoNo <- relevel(data$infoNo, ref = "1")

# Binomial logistic regression for ikou
model <- glm(ikou ~ infoNo, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- exp(confint(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjustment #
model <- glm(ikou ~ infoNo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

######### ORs #########
### Ikou by Q42.1
data <- data %>%
  mutate(ikou = case_when(
    data$Q39 == 3 ~ NA,
    data$Q42.1 == 1 ~ 1,
    data$Q42.1 == 2 ~ 1,
    data$Q42.1 == 3 ~ 0,
    data$Q42.1 == 4 ~ 0,
    TRUE ~ NA))

######### ABCD #########
# A: Did, will
# B: Didnt, will
# C: Did, wont
# D: Didnt, wont

# Data excluding Q39 = 3
data <- data %>%
  mutate(abcd = case_when(
    Q39 == 3 ~ NA,
    Q39 <= 2 & Q42.1 <= 2 ~ "A",
    Q39 >= 4 & Q42.1 <= 2 ~ "B",
    Q39 <= 2 & Q42.1 >= 3 ~ "C",
    Q39 >= 4 & Q42.1 >= 3 ~ "D",
    TRUE ~ NA))

table(data$abcd)

######### Facilitating #########
data <- data %>%
  mutate(bi43.1 = case_when(
    Q43.1 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.2 = case_when(
    Q43.2 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.3 = case_when(
    Q43.3 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.4 = case_when(
    Q43.4 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.5 = case_when(
    Q43.5 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.6 = case_when(
    Q43.6 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.7 = case_when(
    Q43.7 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.8 = case_when(
    Q43.8 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.9 = case_when(
    Q43.9 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.10 = case_when(
    Q43.10 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.11 = case_when(
    Q43.11 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.12 = case_when(
    Q43.12 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.13 = case_when(
    Q43.13 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.14 = case_when(
    Q43.14 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.15 = case_when(
    Q43.15 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.16 = case_when(
    Q43.16 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.17 = case_when(
    Q43.17 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.18 = case_when(
    Q43.18 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.19 = case_when(
    Q43.19 <= 2  ~ "1",
    TRUE ~ "0"))
data <- data %>%
  mutate(bi43.20 = case_when(
    Q43.20 <= 2  ~ "1",
    TRUE ~ "0"))

# Count the number of "1"
temp <- table(data$bi43.1, data$abcd)
temp <- rbind(temp, table(data$bi43.2, data$abcd))
temp <- rbind(temp, table(data$bi43.3, data$abcd))
temp <- rbind(temp, table(data$bi43.4, data$abcd))
temp <- rbind(temp, table(data$bi43.5, data$abcd))
temp <- rbind(temp, table(data$bi43.6, data$abcd))
temp <- rbind(temp, table(data$bi43.7, data$abcd))
temp <- rbind(temp, table(data$bi43.8, data$abcd))
temp <- rbind(temp, table(data$bi43.9, data$abcd))
temp <- rbind(temp, table(data$bi43.10, data$abcd))
temp <- rbind(temp, table(data$bi43.11, data$abcd))
temp <- rbind(temp, table(data$bi43.12, data$abcd))
temp <- rbind(temp, table(data$bi43.13, data$abcd))
temp <- rbind(temp, table(data$bi43.14, data$abcd))
temp <- rbind(temp, table(data$bi43.15, data$abcd))
temp <- rbind(temp, table(data$bi43.16, data$abcd))
temp <- rbind(temp, table(data$bi43.17, data$abcd))
temp <- rbind(temp, table(data$bi43.18, data$abcd))
temp <- rbind(temp, table(data$bi43.19, data$abcd))
temp <- rbind(temp, table(data$bi43.20, data$abcd))

# Copy to clipboard
temp
temp <- temp[c(FALSE, TRUE), ]
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Test for association among bi43 and abcd
temp01 <- chisq.test(table(data$bi43.1, data$abcd))$p.value
temp02 <- chisq.test(table(data$bi43.2, data$abcd))$p.value
temp03 <- chisq.test(table(data$bi43.3, data$abcd))$p.value
temp04 <- chisq.test(table(data$bi43.4, data$abcd))$p.value
temp05 <- chisq.test(table(data$bi43.5, data$abcd))$p.value
temp06 <- chisq.test(table(data$bi43.6, data$abcd))$p.value
temp07 <- chisq.test(table(data$bi43.7, data$abcd))$p.value
temp08 <- chisq.test(table(data$bi43.8, data$abcd))$p.value
temp09 <- chisq.test(table(data$bi43.9, data$abcd))$p.value
temp10 <- chisq.test(table(data$bi43.10, data$abcd))$p.value
temp11 <- chisq.test(table(data$bi43.11, data$abcd))$p.value
temp12 <- chisq.test(table(data$bi43.12, data$abcd))$p.value
temp13 <- chisq.test(table(data$bi43.13, data$abcd))$p.value
temp14 <- chisq.test(table(data$bi43.14, data$abcd))$p.value
temp15 <- chisq.test(table(data$bi43.15, data$abcd))$p.value
temp16 <- chisq.test(table(data$bi43.16, data$abcd))$p.value
temp17 <- chisq.test(table(data$bi43.17, data$abcd))$p.value
temp18 <- chisq.test(table(data$bi43.18, data$abcd))$p.value
temp19 <- chisq.test(table(data$bi43.19, data$abcd))$p.value
temp20 <- chisq.test(table(data$bi43.20, data$abcd))$p.value

temp <- c(temp01, temp02, temp03, temp04, temp05, temp06, 
          temp07, temp08, temp09, temp10, temp11, temp12,
          temp13, temp14, temp15, temp16, temp17, temp18,
          temp19, temp20)
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

###### Convert facilitating ranking ######

med43 <- c()

for (i in 1:nrow(data)) {
  # Calculate median
  temp_array <- c(data$Q43.1[i], data$Q43.2[i], data$Q43.3[i], 
                  data$Q43.4[i], data$Q43.5[i], data$Q43.6[i], 
                  data$Q43.7[i], data$Q43.8[i], data$Q43.9[i], 
                  data$Q43.10[i], data$Q43.11[i], data$Q43.12[i],
                  data$Q43.13[i], data$Q43.14[i], data$Q43.15[i],
                  data$Q43.16[i], data$Q43.17[i], data$Q43.18[i],
                  data$Q43.19[i], data$Q43.20[i])
  temp <- median(temp_array)
  # Check if that is integer or not
  if (temp %% 1 == 0) {
    med43[i] <- temp
  } else {
    count_p05 <- sum(temp_array == temp + 0.5)
    count_m05 <- sum(temp_array == temp - 0.5)
    if (count_p05 > count_m05) {
      med43[i] <- temp + 0.5
    } else {
      med43[i] <- temp - 0.5
    }
  }
}

# Convert to ranking
data$rk43.1 <- data$Q43.1 - med43
data$rk43.2 <- data$Q43.2 - med43
data$rk43.3 <- data$Q43.3 - med43
data$rk43.4 <- data$Q43.4 - med43
data$rk43.5 <- data$Q43.5 - med43
data$rk43.6 <- data$Q43.6 - med43
data$rk43.7 <- data$Q43.7 - med43
data$rk43.8 <- data$Q43.8 - med43
data$rk43.9 <- data$Q43.9 - med43
data$rk43.10 <- data$Q43.10 - med43
data$rk43.11 <- data$Q43.11 - med43
data$rk43.12 <- data$Q43.12 - med43
data$rk43.13 <- data$Q43.13 - med43
data$rk43.14 <- data$Q43.14 - med43
data$rk43.15 <- data$Q43.15 - med43
data$rk43.16 <- data$Q43.16 - med43
data$rk43.17 <- data$Q43.17 - med43
data$rk43.18 <- data$Q43.18 - med43
data$rk43.19 <- data$Q43.19 - med43
data$rk43.20 <- data$Q43.20 - med43

# Median and quartile rk43.1 by ABCD
temp01 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.1, na.rm = TRUE),
            q1 = quantile(rk43.1, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.1, 0.75, na.rm = TRUE))
temp01 <- as.data.frame(t(temp01))
temp_med <- temp01[2, ]
temp_q1 <- temp01[3, ]
temp_q3 <- temp01[4, ]
temp01 <- c(temp_med, temp_q1, temp_q3)

temp02 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.2, na.rm = TRUE),
            q1 = quantile(rk43.2, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.2, 0.75, na.rm = TRUE))
temp02 <- as.data.frame(t(temp02))
temp_med <- temp02[2, ]
temp_q1 <- temp02[3, ]
temp_q3 <- temp02[4, ]
temp02 <- c(temp_med, temp_q1, temp_q3)

temp03 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.3, na.rm = TRUE),
            q1 = quantile(rk43.3, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.3, 0.75, na.rm = TRUE))
temp03 <- as.data.frame(t(temp03))
temp_med <- temp03[2, ]
temp_q1 <- temp03[3, ]
temp_q3 <- temp03[4, ]
temp03 <- c(temp_med, temp_q1, temp_q3)

temp04 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.4, na.rm = TRUE),
            q1 = quantile(rk43.4, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.4, 0.75, na.rm = TRUE))
temp04 <- as.data.frame(t(temp04))
temp_med <- temp04[2, ]
temp_q1 <- temp04[3, ]
temp_q3 <- temp04[4, ]
temp04 <- c(temp_med, temp_q1, temp_q3)

temp05 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.5, na.rm = TRUE),
            q1 = quantile(rk43.5, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.5, 0.75, na.rm = TRUE))
temp05 <- as.data.frame(t(temp05))
temp_med <- temp05[2, ]
temp_q1 <- temp05[3, ]
temp_q3 <- temp05[4, ]
temp05 <- c(temp_med, temp_q1, temp_q3)

temp06 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.6, na.rm = TRUE),
            q1 = quantile(rk43.6, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.6, 0.75, na.rm = TRUE))
temp06 <- as.data.frame(t(temp06))
temp_med <- temp06[2, ]
temp_q1 <- temp06[3, ]
temp_q3 <- temp06[4, ]
temp06 <- c(temp_med, temp_q1, temp_q3)

temp07 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.7, na.rm = TRUE),
            q1 = quantile(rk43.7, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.7, 0.75, na.rm = TRUE))
temp07 <- as.data.frame(t(temp07))
temp_med <- temp07[2, ]
temp_q1 <- temp07[3, ]
temp_q3 <- temp07[4, ]
temp07 <- c(temp_med, temp_q1, temp_q3)

temp08 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.8, na.rm = TRUE),
            q1 = quantile(rk43.8, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.8, 0.75, na.rm = TRUE))
temp08 <- as.data.frame(t(temp08))
temp_med <- temp08[2, ]
temp_q1 <- temp08[3, ]
temp_q3 <- temp08[4, ]
temp08 <- c(temp_med, temp_q1, temp_q3)

temp09 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.9, na.rm = TRUE),
            q1 = quantile(rk43.9, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.9, 0.75, na.rm = TRUE))
temp09 <- as.data.frame(t(temp09))
temp_med <- temp09[2, ]
temp_q1 <- temp09[3, ]
temp_q3 <- temp09[4, ]
temp09 <- c(temp_med, temp_q1, temp_q3)

temp10 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.10, na.rm = TRUE),
            q1 = quantile(rk43.10, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.10, 0.75, na.rm = TRUE))
temp10 <- as.data.frame(t(temp10))
temp_med <- temp10[2, ]
temp_q1 <- temp10[3, ]
temp_q3 <- temp10[4, ]
temp10 <- c(temp_med, temp_q1, temp_q3)

temp11 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.11, na.rm = TRUE),
            q1 = quantile(rk43.11, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.11, 0.75, na.rm = TRUE))
temp11 <- as.data.frame(t(temp11))
temp_med <- temp11[2, ]
temp_q1 <- temp11[3, ]
temp_q3 <- temp11[4, ]
temp11 <- c(temp_med, temp_q1, temp_q3)

temp12 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.12, na.rm = TRUE),
            q1 = quantile(rk43.12, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.12, 0.75, na.rm = TRUE))
temp12 <- as.data.frame(t(temp12))
temp_med <- temp12[2, ]
temp_q1 <- temp12[3, ]
temp_q3 <- temp12[4, ]
temp12 <- c(temp_med, temp_q1, temp_q3)

temp13 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.13, na.rm = TRUE),
            q1 = quantile(rk43.13, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.13, 0.75, na.rm = TRUE))
temp13 <- as.data.frame(t(temp13))
temp_med <- temp13[2, ]
temp_q1 <- temp13[3, ]
temp_q3 <- temp13[4, ]
temp13 <- c(temp_med, temp_q1, temp_q3)

temp14 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.14, na.rm = TRUE),
            q1 = quantile(rk43.14, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.14, 0.75, na.rm = TRUE))
temp14 <- as.data.frame(t(temp14))
temp_med <- temp14[2, ]
temp_q1 <- temp14[3, ]
temp_q3 <- temp14[4, ]
temp14 <- c(temp_med, temp_q1, temp_q3)

temp15 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.15, na.rm = TRUE),
            q1 = quantile(rk43.15, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.15, 0.75, na.rm = TRUE))
temp15 <- as.data.frame(t(temp15))
temp_med <- temp15[2, ]
temp_q1 <- temp15[3, ]
temp_q3 <- temp15[4, ]
temp15 <- c(temp_med, temp_q1, temp_q3)

temp16 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.16, na.rm = TRUE),
            q1 = quantile(rk43.16, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.16, 0.75, na.rm = TRUE))
temp16 <- as.data.frame(t(temp16))
temp_med <- temp16[2, ]
temp_q1 <- temp16[3, ]
temp_q3 <- temp16[4, ]
temp16 <- c(temp_med, temp_q1, temp_q3)

temp17 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.17, na.rm = TRUE),
            q1 = quantile(rk43.17, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.17, 0.75, na.rm = TRUE))
temp17 <- as.data.frame(t(temp17))
temp_med <- temp17[2, ]
temp_q1 <- temp17[3, ]
temp_q3 <- temp17[4, ]
temp17 <- c(temp_med, temp_q1, temp_q3)

temp18 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.18, na.rm = TRUE),
            q1 = quantile(rk43.18, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.18, 0.75, na.rm = TRUE))
temp18 <- as.data.frame(t(temp18))
temp_med <- temp18[2, ]
temp_q1 <- temp18[3, ]
temp_q3 <- temp18[4, ]
temp18 <- c(temp_med, temp_q1, temp_q3)

temp19 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.19, na.rm = TRUE),
            q1 = quantile(rk43.19, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.19, 0.75, na.rm = TRUE))
temp19 <- as.data.frame(t(temp19))
temp_med <- temp19[2, ]
temp_q1 <- temp19[3, ]
temp_q3 <- temp19[4, ]
temp19 <- c(temp_med, temp_q1, temp_q3)

temp20 <- data %>%
  group_by(abcd) %>%
  summarise(median = median(rk43.20, na.rm = TRUE),
            q1 = quantile(rk43.20, 0.25, na.rm = TRUE),
            q3 = quantile(rk43.20, 0.75, na.rm = TRUE))
temp20 <- as.data.frame(t(temp20))
temp_med <- temp20[2, ]
temp_q1 <- temp20[3, ]
temp_q3 <- temp20[4, ]
temp20 <- c(temp_med, temp_q1, temp_q3)

# Combine all results
temp <- rbind(temp01, temp02, temp03, temp04, temp05, 
              temp06, temp07, temp08, temp09, temp10,
              temp11, temp12, temp13, temp14, temp15,
              temp16, temp17, temp18, temp19, temp20)
# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Count 0, more, less
temp01 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.1 == 0, na.rm = TRUE),
    count_more = sum(rk43.1 > 0, na.rm = TRUE),
    count_less = sum(rk43.1 < 0, na.rm = TRUE))
temp01 <- as.data.frame(t(temp01))
temp_0 <- temp01[2, ]
temp_high <- temp01[3, ]
temp_low <- temp01[4, ]
temp01 <- c(temp_0, temp_high, temp_low)

temp02 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.2 == 0, na.rm = TRUE),
    count_more = sum(rk43.2 > 0, na.rm = TRUE),
    count_less = sum(rk43.2 < 0, na.rm = TRUE))
temp02 <- as.data.frame(t(temp02))
temp_0 <- temp02[2, ]
temp_high <- temp02[3, ]
temp_low <- temp02[4, ]
temp02 <- c(temp_0, temp_high, temp_low)

temp03 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.3 == 0, na.rm = TRUE),
    count_more = sum(rk43.3 > 0, na.rm = TRUE),
    count_less = sum(rk43.3 < 0, na.rm = TRUE))
temp03 <- as.data.frame(t(temp03))
temp_0 <- temp03[2, ]
temp_high <- temp03[3, ]
temp_low <- temp03[4, ]
temp03 <- c(temp_0, temp_high, temp_low)

temp04 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.4 == 0, na.rm = TRUE),
    count_more = sum(rk43.4 > 0, na.rm = TRUE),
    count_less = sum(rk43.4 < 0, na.rm = TRUE))
temp04 <- as.data.frame(t(temp04))
temp_0 <- temp04[2, ]
temp_high <- temp04[3, ]
temp_low <- temp04[4, ]
temp04 <- c(temp_0, temp_high, temp_low)

temp05 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.5 == 0, na.rm = TRUE),
    count_more = sum(rk43.5 > 0, na.rm = TRUE),
    count_less = sum(rk43.5 < 0, na.rm = TRUE))
temp05 <- as.data.frame(t(temp05))
temp_0 <- temp05[2, ]
temp_high <- temp05[3, ]
temp_low <- temp05[4, ]
temp05 <- c(temp_0, temp_high, temp_low)

temp06 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.6 == 0, na.rm = TRUE),
    count_more = sum(rk43.6 > 0, na.rm = TRUE),
    count_less = sum(rk43.6 < 0, na.rm = TRUE))
temp06 <- as.data.frame(t(temp06))
temp_0 <- temp06[2, ]
temp_high <- temp06[3, ]
temp_low <- temp06[4, ]
temp06 <- c(temp_0, temp_high, temp_low)

temp07 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.7 == 0, na.rm = TRUE),
    count_more = sum(rk43.7 > 0, na.rm = TRUE),
    count_less = sum(rk43.7 < 0, na.rm = TRUE))
temp07 <- as.data.frame(t(temp07))
temp_0 <- temp07[2, ]
temp_high <- temp07[3, ]
temp_low <- temp07[4, ]
temp07 <- c(temp_0, temp_high, temp_low)

temp08 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.8 == 0, na.rm = TRUE),
    count_more = sum(rk43.8 > 0, na.rm = TRUE),
    count_less = sum(rk43.8 < 0, na.rm = TRUE))
temp08 <- as.data.frame(t(temp08))
temp_0 <- temp08[2, ]
temp_high <- temp08[3, ]
temp_low <- temp08[4, ]
temp08 <- c(temp_0, temp_high, temp_low)

temp09 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.9 == 0, na.rm = TRUE),
    count_more = sum(rk43.9 > 0, na.rm = TRUE),
    count_less = sum(rk43.9 < 0, na.rm = TRUE))
temp09 <- as.data.frame(t(temp09))
temp_0 <- temp09[2, ]
temp_high <- temp09[3, ]
temp_low <- temp09[4, ]
temp09 <- c(temp_0, temp_high, temp_low)

temp10 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.10 == 0, na.rm = TRUE),
    count_more = sum(rk43.10 > 0, na.rm = TRUE),
    count_less = sum(rk43.10 < 0, na.rm = TRUE))
temp10 <- as.data.frame(t(temp10))
temp_0 <- temp10[2, ]
temp_high <- temp10[3, ]
temp_low <- temp10[4, ]
temp10 <- c(temp_0, temp_high, temp_low)

temp11 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.11 == 0, na.rm = TRUE),
    count_more = sum(rk43.11 > 0, na.rm = TRUE),
    count_less = sum(rk43.11 < 0, na.rm = TRUE))
temp11 <- as.data.frame(t(temp11))
temp_0 <- temp11[2, ]
temp_high <- temp11[3, ]
temp_low <- temp11[4, ]
temp11 <- c(temp_0, temp_high, temp_low)

temp12 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.12 == 0, na.rm = TRUE),
    count_more = sum(rk43.12 > 0, na.rm = TRUE),
    count_less = sum(rk43.12 < 0, na.rm = TRUE))
temp12 <- as.data.frame(t(temp12))
temp_0 <- temp12[2, ]
temp_high <- temp12[3, ]
temp_low <- temp12[4, ]
temp12 <- c(temp_0, temp_high, temp_low)

temp13 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.13 == 0, na.rm = TRUE),
    count_more = sum(rk43.13 > 0, na.rm = TRUE),
    count_less = sum(rk43.13 < 0, na.rm = TRUE))
temp13 <- as.data.frame(t(temp13))
temp_0 <- temp13[2, ]
temp_high <- temp13[3, ]
temp_low <- temp13[4, ]
temp13 <- c(temp_0, temp_high, temp_low)

temp14 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.14 == 0, na.rm = TRUE),
    count_more = sum(rk43.14 > 0, na.rm = TRUE),
    count_less = sum(rk43.14 < 0, na.rm = TRUE))
temp14 <- as.data.frame(t(temp14))
temp_0 <- temp14[2, ]
temp_high <- temp14[3, ]
temp_low <- temp14[4, ]
temp14 <- c(temp_0, temp_high, temp_low)

temp15 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.15 == 0, na.rm = TRUE),
    count_more = sum(rk43.15 > 0, na.rm = TRUE),
    count_less = sum(rk43.15 < 0, na.rm = TRUE))
temp15 <- as.data.frame(t(temp15))
temp_0 <- temp15[2, ]
temp_high <- temp15[3, ]
temp_low <- temp15[4, ]
temp15 <- c(temp_0, temp_high, temp_low)

temp16 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.16 == 0, na.rm = TRUE),
    count_more = sum(rk43.16 > 0, na.rm = TRUE),
    count_less = sum(rk43.16 < 0, na.rm = TRUE))
temp16 <- as.data.frame(t(temp16))
temp_0 <- temp16[2, ]
temp_high <- temp16[3, ]
temp_low <- temp16[4, ]
temp16 <- c(temp_0, temp_high, temp_low)

temp17 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.17 == 0, na.rm = TRUE),
    count_more = sum(rk43.17 > 0, na.rm = TRUE),
    count_less = sum(rk43.17 < 0, na.rm = TRUE))
temp17 <- as.data.frame(t(temp17))
temp_0 <- temp17[2, ]
temp_high <- temp17[3, ]
temp_low <- temp17[4, ]
temp17 <- c(temp_0, temp_high, temp_low)

temp18 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.18 == 0, na.rm = TRUE),
    count_more = sum(rk43.18 > 0, na.rm = TRUE),
    count_less = sum(rk43.18 < 0, na.rm = TRUE))
temp18 <- as.data.frame(t(temp18))
temp_0 <- temp18[2, ]
temp_high <- temp18[3, ]
temp_low <- temp18[4, ]
temp18 <- c(temp_0, temp_high, temp_low)

temp19 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.19 == 0, na.rm = TRUE),
    count_more = sum(rk43.19 > 0, na.rm = TRUE),
    count_less = sum(rk43.19 < 0, na.rm = TRUE))
temp19 <- as.data.frame(t(temp19))
temp_0 <- temp19[2, ]
temp_high <- temp19[3, ]
temp_low <- temp19[4, ]
temp19 <- c(temp_0, temp_high, temp_low)

temp20 <- data %>%
  group_by(abcd) %>%
  summarise(
    count_0 = sum(rk43.20 == 0, na.rm = TRUE),
    count_more = sum(rk43.20 > 0, na.rm = TRUE),
    count_less = sum(rk43.20 < 0, na.rm = TRUE))
temp20 <- as.data.frame(t(temp20))
temp_0 <- temp20[2, ]
temp_high <- temp20[3, ]
temp_low <- temp20[4, ]
temp20 <- c(temp_0, temp_high, temp_low)

# Combine all results
temp <- rbind(temp01, temp02, temp03, temp04, temp05, 
              temp06, temp07, temp08, temp09, temp10,
              temp11, temp12, temp13, temp14, temp15,
              temp16, temp17, temp18, temp19, temp20)
# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Convert to ordered factor
data <- data %>%
  mutate(rk43.1f  = case_when(
    rk43.1 >= 2 ~ 2,
    rk43.1 == 1 ~ 1,
    rk43.1 == 0 ~ 0,
    rk43.1 == -1 ~ -1,
    rk43.1 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.2f  = case_when(
    rk43.2 >= 2 ~ 2,
    rk43.2 == 1 ~ 1,
    rk43.2 == 0 ~ 0,
    rk43.2 == -1 ~ -1,
    rk43.2 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.3f  = case_when(
    rk43.3 >= 2 ~ 2,
    rk43.3 == 1 ~ 1,
    rk43.3 == 0 ~ 0,
    rk43.3 == -1 ~ -1,
    rk43.3 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.4f  = case_when(
    rk43.4 >= 2 ~ 2,
    rk43.4 == 1 ~ 1,
    rk43.4 == 0 ~ 0,
    rk43.4 == -1 ~ -1,
    rk43.4 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.5f  = case_when(
    rk43.5 >= 2 ~ 2,
    rk43.5 == 1 ~ 1,
    rk43.5 == 0 ~ 0,
    rk43.5 == -1 ~ -1,
    rk43.5 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.6f  = case_when(
    rk43.6 >= 2 ~ 2,
    rk43.6 == 1 ~ 1,
    rk43.6 == 0 ~ 0,
    rk43.6 == -1 ~ -1,
    rk43.6 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.7f  = case_when(
    rk43.7 >= 2 ~ 2,
    rk43.7 == 1 ~ 1,
    rk43.7 == 0 ~ 0,
    rk43.7 == -1 ~ -1,
    rk43.7 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.8f  = case_when(
    rk43.8 >= 2 ~ 2,
    rk43.8 == 1 ~ 1,
    rk43.8 == 0 ~ 0,
    rk43.8 == -1 ~ -1,
    rk43.8 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.9f  = case_when(
    rk43.9 >= 2 ~ 2,
    rk43.9 == 1 ~ 1,
    rk43.9 == 0 ~ 0,
    rk43.9 == -1 ~ -1,
    rk43.9 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.10f = case_when(
    rk43.10 >= 2 ~ 2,
    rk43.10 == 1 ~ 1,
    rk43.10 == 0 ~ 0,
    rk43.10 == -1 ~ -1,
    rk43.10 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.11f = case_when(
    rk43.11 >= 2 ~ 2,
    rk43.11 == 1 ~ 1,
    rk43.11 == 0 ~ 0,
    rk43.11 == -1 ~ -1,
    rk43.11 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.12f = case_when(
    rk43.12 >= 2 ~ 2,
    rk43.12 == 1 ~ 1,
    rk43.12 == 0 ~ 0,
    rk43.12 == -1 ~ -1,
    rk43.12 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.13f = case_when(
    rk43.13 >= 2 ~ 2,
    rk43.13 == 1 ~ 1,
    rk43.13 == 0 ~ 0,
    rk43.13 == -1 ~ -1,
    rk43.13 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.14f = case_when(
    rk43.14 >= 2 ~ 2,
    rk43.14 == 1 ~ 1,
    rk43.14 == 0 ~ 0,
    rk43.14 == -1 ~ -1,
    rk43.14 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.15f = case_when(
    rk43.15 >= 2 ~ 2,
    rk43.15 == 1 ~ 1,
    rk43.15 == 0 ~ 0,
    rk43.15 == -1 ~ -1,
    rk43.15 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.16f = case_when(
    rk43.16 >= 2 ~ 2,
    rk43.16 == 1 ~ 1,
    rk43.16 == 0 ~ 0,
    rk43.16 == -1 ~ -1,
    rk43.16 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.17f = case_when(
    rk43.17 >= 2 ~ 2,
    rk43.17 == 1 ~ 1,
    rk43.17 == 0 ~ 0,
    rk43.17 == -1 ~ -1,
    rk43.17 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.18f = case_when(
    rk43.18 >= 2 ~ 2,
    rk43.18 == 1 ~ 1,
    rk43.18 == 0 ~ 0,
    rk43.18 == -1 ~ -1,
    rk43.18 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.19f = case_when(
    rk43.19 >= 2 ~ 2,
    rk43.19 == 1 ~ 1,
    rk43.19 == 0 ~ 0,
    rk43.19 == -1 ~ -1,
    rk43.19 <= -2 ~ -2
  ))
data <- data %>%
  mutate(rk43.20f = case_when(
    rk43.20 >= 2 ~ 2,
    rk43.20 == 1 ~ 1,
    rk43.20 == 0 ~ 0,
    rk43.20 == -1 ~ -1,
    rk43.20 <= -2 ~ -2
  ))

data$rk43.1f <- as.ordered(data$rk43.1f)
data$rk43.2f <- as.ordered(data$rk43.2f)
data$rk43.3f <- as.ordered(data$rk43.3f)
data$rk43.4f <- as.ordered(data$rk43.4f)
data$rk43.5f <- as.ordered(data$rk43.5f)
data$rk43.6f <- as.ordered(data$rk43.6f)
data$rk43.7f <- as.ordered(data$rk43.7f)
data$rk43.8f <- as.ordered(data$rk43.8f)
data$rk43.9f <- as.ordered(data$rk43.9f)
data$rk43.10f <- as.ordered(data$rk43.10f)
data$rk43.11f <- as.ordered(data$rk43.11f)
data$rk43.12f <- as.ordered(data$rk43.12f)
data$rk43.13f <- as.ordered(data$rk43.13f)
data$rk43.14f <- as.ordered(data$rk43.14f)
data$rk43.15f <- as.ordered(data$rk43.15f)
data$rk43.16f <- as.ordered(data$rk43.16f)
data$rk43.17f <- as.ordered(data$rk43.17f)
data$rk43.18f <- as.ordered(data$rk43.18f)
data$rk43.19f <- as.ordered(data$rk43.19f)
data$rk43.20f <- as.ordered(data$rk43.20f)

#
# Test difference among ABCD
temp01 <- kruskal.test(rk43.1f ~ abcd, data = data)$p.value
temp02 <- kruskal.test(rk43.2f ~ abcd, data = data)$p.value
temp03 <- kruskal.test(rk43.3f ~ abcd, data = data)$p.value
temp04 <- kruskal.test(rk43.4f ~ abcd, data = data)$p.value
temp05 <- kruskal.test(rk43.5f ~ abcd, data = data)$p.value
temp06 <- kruskal.test(rk43.6f ~ abcd, data = data)$p.value
temp07 <- kruskal.test(rk43.7f ~ abcd, data = data)$p.value
temp08 <- kruskal.test(rk43.8f ~ abcd, data = data)$p.value
temp09 <- kruskal.test(rk43.9f ~ abcd, data = data)$p.value
temp10 <- kruskal.test(rk43.10f ~ abcd, data = data)$p.value
temp11 <- kruskal.test(rk43.11f ~ abcd, data = data)$p.value
temp12 <- kruskal.test(rk43.12f ~ abcd, data = data)$p.value
temp13 <- kruskal.test(rk43.13f ~ abcd, data = data)$p.value
temp14 <- kruskal.test(rk43.14f ~ abcd, data = data)$p.value
temp15 <- kruskal.test(rk43.15f ~ abcd, data = data)$p.value
temp16 <- kruskal.test(rk43.16f ~ abcd, data = data)$p.value
temp17 <- kruskal.test(rk43.17f ~ abcd, data = data)$p.value
temp18 <- kruskal.test(rk43.18f ~ abcd, data = data)$p.value
temp19 <- kruskal.test(rk43.19f ~ abcd, data = data)$p.value
temp20 <- kruskal.test(rk43.20f ~ abcd, data = data)$p.value

temp <- c(temp01, temp02, temp03, temp04, temp05, 
          temp06, temp07, temp08, temp09, temp10,
          temp11, temp12, temp13, temp14, temp15,
          temp16, temp17, temp18, temp19, temp20)

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

###### Clustering by facilitating ranking ######

###### Option 1 Gower distance and clustering ######

# exclude Q39 == 3
df_temp <- data[data$Q39 != 3, ]

# select columns rk43.1f~20f
df_grower <- df_temp[, c("rk43.1f", "rk43.2f", "rk43.3f", 
                         "rk43.4f", "rk43.5f", "rk43.6f", 
                         "rk43.7f", "rk43.8f", "rk43.9f", 
                         "rk43.10f", "rk43.11f", "rk43.12f",
                         "rk43.13f", "rk43.14f", "rk43.15f",
                         "rk43.16f", "rk43.17f", "rk43.18f",
                         "rk43.19f", "rk43.20f")]

gower_dist <- daisy(df_grower, metric = "gower")

### Sensitivity analysis by Manhattan distance
# 型を確認
sapply(df_grower, class)

# factor/ordered factor が混ざっている場合は numeric に変換
df_grower <- df_grower %>%
  mutate(across(everything(), ~ as.numeric(as.character(.x))))

sapply(df_grower, class)

gower_dist <- daisy(df_grower, metric = "gower")

gower_dist <- daisy(df_grower, metric = "manhattan")

# クラスタ数の選定（シルエット幅）
set.seed(123)
k_range <- 2:20

sil_width <- sapply(k_range, function(k) pam(gower_dist, k = k)$silinfo$avg.width)

plot(k_range, sil_width, type = "b", xlab = "k", ylab = "Average silhouette width")

# plot the same using ggplot2
ggplot(data.frame(k = k_range, sil_width = sil_width), aes(x = k, y = sil_width)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "",
       x = "Number of clusters (k)",
       y = "Average silhouette width") +
  theme_minimal() +
  # draw x and y axis lines
  xlim(0, 20) +
  ylim(0, 0.3) +
  # font size for axis label, title
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 16, hjust = 0.5))

ggplot(data.frame(k = k_range, sil_width = sil_width), aes(x = k, y = sil_width)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "",
       x = "Number of clusters (k)",
       y = "Average silhouette width") +
  theme_minimal() +
  # draw x and y axis lines
  xlim(3, 20) +
  ylim(0, 0.3) +
  # font size for axis label, title
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 16, hjust = 0.5))

#500x500

best_k <- k_range[which.max(sil_width)]
best_k
best_k <- 8

# 最終クラスタリング（PAM）
set.seed(123)
# pam_fit <- pam(gower_dist, k = best_k)
# save(pam_fit, file = "pam_fit.RData")

# pam_fit <- pam(gower_dist, k = best_k)
# save(pam_fit, file = "pam_fit_num.RData")

# pam_fit <- pam(gower_dist, k = 6)
# save(pam_fit, file = "pam_fit6.RData")
# 
# pam_fit <- pam(gower_dist, k = 7)
# save(pam_fit, file = "pam_fit7.RData")
# 
# pam_fit <- pam(gower_dist, k = 9)
# save(pam_fit, file = "pam_fit9.RData")
# 
# pam_fit <- pam(gower_dist, k = 10)
# save(pam_fit, file = "pam_fit10.RData")

pam_fit <- pam(gower_dist, k = best_k)

# Attach original ID
df_grower$cluster <- factor(pam_fit$clustering)
df_grower$cluster <- as.factor(df_grower$cluster)

df_temp <- data[data$Q39 != 3, ]
df_temp <- cbind(Monitor_ID = df_temp$Monitor_ID, cluster = df_grower$cluster)
df_temp <- data.frame(df_temp)

data <- left_join(data, df_temp, by = "Monitor_ID")

data_temp <- data[data$Q39 != 3, ]

# クラスタ代表（medoid）の行番号と個体ID
pam_fit$id.med    # メドイドの行番号

pam_fit$medoids   # メドイドの行データ

table(data_temp$cluster)  # 各クラスタの個体数

# temp <- table(data$rk43.1f, data$cluster)  # 各クラスタのrk43.1fの分布
# temp <- rbind(temp, table(data$rk43.2f, data$cluster))
# temp <- rbind(temp, table(data$rk43.3f, data$cluster))
# temp <- rbind(temp, table(data$rk43.4f, data$cluster))
# temp <- rbind(temp, table(data$rk43.5f, data$cluster))
# temp <- rbind(temp, table(data$rk43.6f, data$cluster))
# temp <- rbind(temp, table(data$rk43.7f, data$cluster))
# temp <- rbind(temp, table(data$rk43.8f, data$cluster))
# temp <- rbind(temp, table(data$rk43.9f, data$cluster))
# temp <- rbind(temp, table(data$rk43.10f, data$cluster))
# temp <- rbind(temp, table(data$rk43.11f, data$cluster))
# temp <- rbind(temp, table(data$rk43.12f, data$cluster))
# temp <- rbind(temp, table(data$rk43.13f, data$cluster))
# temp <- rbind(temp, table(data$rk43.14f, data$cluster))
# temp <- rbind(temp, table(data$rk43.15f, data$cluster))
# temp <- rbind(temp, table(data$rk43.16f, data$cluster))
# temp <- rbind(temp, table(data$rk43.17f, data$cluster))
# temp <- rbind(temp, table(data$rk43.18f, data$cluster))
# temp <- rbind(temp, table(data$rk43.19f, data$cluster))
# temp <- rbind(temp, table(data$rk43.20f, data$cluster))
# 
# # Copy to clipboard
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Make ranking by cluster
temp <- table(data$rk43.1f, data$cluster)
high_prop <- (temp[1, ] + temp[2, ]) / colSums(temp)

temp <- table(data$rk43.2f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.3f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.4f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.5f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.6f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.7f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.8f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.9f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.10f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.11f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.12f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.13f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.14f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.15f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.16f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.17f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.18f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.19f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

temp <- table(data$rk43.20f, data$cluster)
temp <- (temp[1, ] + temp[2, ]) / colSums(temp)
high_prop <- rbind(high_prop, temp)

table(data$cluster)

# prop hesi / accept

table(data$cluster, data$ikou)
#proportion
temp <- table(data$cluster, data$ikou)
prop <- prop.table(temp, margin = 1)
prop

#sort by 0 prop
prop_rank <- c(rank(-prop[, 1], ties.method = "first"))
prop_rank

data <- data %>%
  mutate(cluster_re = case_when(
    cluster == "1" ~ paste0("Cluster ", prop_rank[1]),
    cluster == "2" ~ paste0("Cluster ", prop_rank[2]),
    cluster == "3" ~ paste0("Cluster ", prop_rank[3]),
    cluster == "4" ~ paste0("Cluster ", prop_rank[4]),
    cluster == "5" ~ paste0("Cluster ", prop_rank[5]),
    cluster == "6" ~ paste0("Cluster ", prop_rank[6]),
    cluster == "7" ~ paste0("Cluster ", prop_rank[7]),
    cluster == "8" ~ paste0("Cluster ", prop_rank[8])
    ))



# data_temp <- data[data$Q39 != 3, ]

high_prop <- as.data.frame(high_prop)

colnames(high_prop) <- c(paste0("Cluster ", prop_rank[1]),
                         paste0("Cluster ", prop_rank[2]),
                         paste0("Cluster ", prop_rank[3]),
                         paste0("Cluster ", prop_rank[4]),
                         paste0("Cluster ", prop_rank[5]),
                         paste0("Cluster ", prop_rank[6]),
                         paste0("Cluster ", prop_rank[7]),
                         paste0("Cluster ", prop_rank[8]))

rownames(high_prop) <- paste0("rk43.", 1:20)

rownames(high_prop) <- c("Clinical trial with Japanese",
                         "Big clinical trial with/without Japanese",
                         "Recommended by WHO",
                         "Recommended by Japanese government",
                         "Recommended by experts",
                         "Recommended by doctors",
                         "Recommended by family members",
                         "Recommended by friends",
                         "Acquaintances have been vaccinated",
                         "Developed in Japan",
                         "Produced in Japan",
                         "Conventional vaccine",
                         "RNA vaccine",
                         "Novel vaccine",
                         "Voucher sent to home",
                         "No need for voucher",
                         "Can be received at nearby healthcare facility",
                         "Can be received near home or workplace",
                         "Available in the evening and on weekends",
                         "Free of charge")

high_prop_rank <- apply(high_prop, 2, function(x) rank(-x, ties.method = "first"))

# Reorder columns by name
high_prop_rank <- high_prop_rank[, order(colnames(high_prop_rank))]

# Draw heatmap with clustering
col_fun <- colorRamp2(c(1, 10.5, 20), c("#228B22", "white", "orange"))

Heatmap(
  high_prop_rank,
  name = "value",
  col = col_fun,
  cluster_rows = TRUE,
  cluster_columns = FALSE,
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_dend_side = "right",
  row_names_side = "left",
  # row_names_gp = gpar(fontsize = 9),
  row_names_max_width = unit(300, "pt"),   
  heatmap_legend_param = list(
    at = c(1, 20),      # 目盛り位置
    labels = c("1", "20"),  # 表示ラベル
    title = "Rank",
    title_gp = gpar(fontsize = 16),
    labels_gp = gpar(fontsize = 16),
    legend_height = unit(80, "pt"),
    legend_width  = unit(40, "pt")) 
)

#800x600

###### features for clustering ######

### AGE
temp <- table(data$nenrei, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### SEX
temp <- table(data$SEX, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Income
temp <- table(data$nenshu, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Education
temp <- table(data$gakureki, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Job
temp <- table(data$shigoto, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Living alone
temp <- table(data$hitori, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Marital status
temp <- table(data$kekkon, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Contact with others
temp <- table(data$kouryu, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Contact with babies
temp <- table(data$kodomo, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Contact with elderly
temp <- table(data$roujin, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Anxiety
temp <- table(data$shinpai, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Communition
temp <- table(data$kyodo, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Extreme communition
temp <- table(data$ext_kyodo, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Agency
temp <- table(data$sado, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Extreme agency
temp <- table(data$ext_sado, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Happines
temp <- table(data$shiawase, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Information sources
temp <- table(data$info01, data$cluster_re)
temp <- rbind(temp, table(data$info02, data$cluster_re))
temp <- rbind(temp, table(data$info03, data$cluster_re))
temp <- rbind(temp, table(data$info04, data$cluster_re))
temp <- rbind(temp, table(data$info05, data$cluster_re))
temp <- rbind(temp, table(data$info06, data$cluster_re))
temp <- rbind(temp, table(data$info07, data$cluster_re))
temp <- rbind(temp, table(data$info08, data$cluster_re))
temp <- rbind(temp, table(data$info09, data$cluster_re))
temp <- rbind(temp, table(data$info10, data$cluster_re))
temp <- rbind(temp, table(data$info14, data$cluster_re))
temp <- rbind(temp, table(data$info15, data$cluster_re))
temp <- rbind(temp, table(data$info16, data$cluster_re))
temp <- rbind(temp, table(data$info17, data$cluster_re))
temp <- rbind(temp, table(data$info18, data$cluster_re))
temp <- rbind(temp, table(data$info20, data$cluster_re))
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Past infection
temp <- table(data$inf_survey_2020_2_jac, data$cluster_re)
temp <- rbind(temp, table(data$inf_survey_2021_1_jas, data$cluster_re))
temp <- rbind(temp, table(data$inf_survey_2021_2_jac, data$cluster_re))
temp <- rbind(temp, table(data$inf_survey_2022_1_jas, data$cluster_re))
temp <- rbind(temp, table(data$inf_survey_2022_2_jac, data$cluster_re))
temp <- rbind(temp, table(data$inf_survey_2023_1_jas, data$cluster_re))
temp <- rbind(temp, table(data$inf_2020_all, data$cluster_re))
temp <- rbind(temp, table(data$inf_2021_all, data$cluster_re))
temp <- rbind(temp, table(data$inf_2022_all, data$cluster_re))
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Adverse reaction
temp <- table(data$adv1, data$cluster_re)
temp <- rbind(temp, table(data$adv2, data$cluster_re))
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Medical condition
temp <- table(data$med01, data$cluster_re)
temp <- rbind(temp, table(data$med02, data$cluster_re))
temp <- rbind(temp, table(data$med03, data$cluster_re))
temp <- rbind(temp, table(data$med04, data$cluster_re))
temp <- rbind(temp, table(data$med05, data$cluster_re))
temp <- rbind(temp, table(data$med06, data$cluster_re))
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Kakaritsuke
temp <- table(data$kakari, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### COVID fear
temp <- table(data$koronafuan, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Vaccine misinformation
temp <- table(data$vac_inbou, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Covid conspiracy
temp <- table(data$korona_inbou, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### ID knowledge
temp <- table(data$chishiki, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### ID measures
temp <- table(data$taisaku, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Vaccine attitude
temp <- table(data$taido01, data$cluster_re)
temp <- rbind(temp, table(data$taido02, data$cluster_re))
temp <- rbind(temp, table(data$taido03, data$cluster_re))
temp <- rbind(temp, table(data$taido04, data$cluster_re))
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### COVID vaccine
temp <- table(data$korona_vac, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Next pandemic when
temp <- table(data$next_pan, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Next vaccine implementation
temp <- table(data$nextvac_when02, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### ABCD
temp <- table(data$abcd, data$cluster_re)
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Info No
temp <- table(data$infoNo, data$cluster_re)
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # クラスタ別の“典型プロファイル”（モード比率のヒートマップ）
# # 変数×クラスタで、最頻カテゴリ(level)とその割合を計算
# get_mode <- function(x) {
#   tab <- table(x, useNA = "no")
#   lvl <- names(which.max(tab))
#   prop <- max(tab) / sum(tab)
#   list(level = lvl, prop = prop)
# }
# 
# prof_list <- lapply(vars, function(v) {
#   by(df[[v]], df$cluster, get_mode)
# })
# 
# # 整形（各v, clusterで mode level と proportion）
# prof_df <- do.call(rbind, lapply(seq_along(vars), function(i){
#   v <- vars[i]
#   do.call(rbind, lapply(levels(df$cluster), function(cl){
#     res <- prof_list[[i]][[cl]]
#     data.frame(var=v, cluster=cl, mode=res$level, prop=res$prop)
#   }))
# }))
# 
# # モードの“確からしさ”を色で、モード内容をラベルで表示
# ggplot(prof_df, aes(x = cluster, y = var, fill = prop, label = mode)) +
#   geom_tile() +
#   geom_text(size = 3) +
#   scale_fill_viridis_c(name = "Modal proportion", limits = c(0,1)) +
#   theme_minimal() + labs(title = "Cluster profiles (mode level and its proportion)",
#                          x = "Cluster", y = "Variable")

######### ORs for Next perception sub 4 #########

### Next pandemic when
### Next pandemic 40
# make group for outcome

table(data$Q39)
table(data$Q40)

data <- data %>%
  mutate(next_pan_out = case_when(
    Q40 <= 3 ~ "123",
    Q40 == 4 ~ "4",
    Q40 <= 6 ~ "56",
    Q40 == 7 ~ "7"))
data$next_pan_out <- as.factor(data$next_pan_out)
table(data$next_pan_out)

###### Demographic ######

### Age
temp <- table(data$nenrei, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ nenrei, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### SEX
temp <- table(data$SEX, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ SEX, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Income
temp <- table(data$nenshu, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ nenshu, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Education
temp <- table(data$gakureki, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ gakureki, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Job health ### EXCLUDING STUDENT
table(data$shigoto)

data <- data %>%
  mutate(shigotoH = case_when(
    shigoto == "med" ~ "med",
    shigoto == "student" ~ NA,
    TRUE  ~ "nonmed"))

# Level
data$shigotoH <- factor(data$shigotoH, levels = c("nonmed", "med"))

temp <- table(data$shigotoH, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ shigotoH, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ shigotoH + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Anxiety
temp <- table(data$shinpai, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ shinpai, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ shinpai + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Kyodo
temp <- table(data$kyodo, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ kyodo, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ kyodo + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### ExtKyodo
temp <- table(data$ext_kyodo, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ ext_kyodo, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
temp <- summary(fit_vg)@coef3[,4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ ext_kyodo + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Sado
temp <- table(data$sado, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ sado, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ sado + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Ext-Sado
temp <- table(data$ext_sado, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ ext_sado, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ ext_sado + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Happiness
temp <- table(data$shiawase, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ shiawase, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ shiawase + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Info
temp <- table(data$info01, data$next_pan_out)
temp <- rbind(temp, table(data$info02, data$next_pan_out))
temp <- rbind(temp, table(data$info03, data$next_pan_out))
temp <- rbind(temp, table(data$info04, data$next_pan_out))
temp <- rbind(temp, table(data$info05, data$next_pan_out))
temp <- rbind(temp, table(data$info06, data$next_pan_out))
temp <- rbind(temp, table(data$info07, data$next_pan_out))
temp <- rbind(temp, table(data$info08, data$next_pan_out))
temp <- rbind(temp, table(data$info09, data$next_pan_out))
temp <- rbind(temp, table(data$info10, data$next_pan_out))
temp <- rbind(temp, table(data$info14, data$next_pan_out))
temp <- rbind(temp, table(data$info15, data$next_pan_out))
temp <- rbind(temp, table(data$info16, data$next_pan_out))
temp <- rbind(temp, table(data$info17, data$next_pan_out))
temp <- rbind(temp, table(data$info18, data$next_pan_out))
temp <- rbind(temp, table(data$info20, data$next_pan_out))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# regression
# model01 <- vglm(next_pan_out ~ info01, family = multinomial(refLevel = "4"), data = data)
# model02 <- vglm(next_pan_out ~ info02, family = multinomial(refLevel = "4"), data = data)
# model03 <- vglm(next_pan_out ~ info03, family = multinomial(refLevel = "4"), data = data)
# model04 <- vglm(next_pan_out ~ info04, family = multinomial(refLevel = "4"), data = data)
# model05 <- vglm(next_pan_out ~ info05, family = multinomial(refLevel = "4"), data = data)
# model06 <- vglm(next_pan_out ~ info06, family = multinomial(refLevel = "4"), data = data)
# model07 <- vglm(next_pan_out ~ info07, family = multinomial(refLevel = "4"), data = data)
# model08 <- vglm(next_pan_out ~ info08, family = multinomial(refLevel = "4"), data = data)
# model09 <- vglm(next_pan_out ~ info09, family = multinomial(refLevel = "4"), data = data)
# model10 <- vglm(next_pan_out ~ info10, family = multinomial(refLevel = "4"), data = data)
# model14 <- vglm(next_pan_out ~ info14, family = multinomial(refLevel = "4"), data = data)
# model15 <- vglm(next_pan_out ~ info15, family = multinomial(refLevel = "4"), data = data)
# model16 <- vglm(next_pan_out ~ info16, family = multinomial(refLevel = "4"), data = data)
# model17 <- vglm(next_pan_out ~ info17, family = multinomial(refLevel = "4"), data = data)
# model18 <- vglm(next_pan_out ~ info18, family = multinomial(refLevel = "4"), data = data)
# model20 <- vglm(next_pan_out ~ info20, family = multinomial(refLevel = "4"), data = data)

# OR, 95CI, p-value
# temp01 <- exp(coef(model01))
# temp02 <- exp(coef(model02))
# temp03 <- exp(coef(model03))
# temp04 <- exp(coef(model04))
# temp05 <- exp(coef(model05))
# temp06 <- exp(coef(model06))
# temp07 <- exp(coef(model07))
# temp08 <- exp(coef(model08))
# temp09 <- exp(coef(model09))
# temp10 <- exp(coef(model10))
# temp14 <- exp(coef(model14))
# temp15 <- exp(coef(model15))
# temp16 <- exp(coef(model16))
# temp17 <- exp(coef(model17))
# temp18 <- exp(coef(model18))
# temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- confint(model01) |> exp()
# temp02 <- confint(model02) |> exp()
# temp03 <- confint(model03) |> exp()
# temp04 <- confint(model04) |> exp()
# temp05 <- confint(model05) |> exp()
# temp06 <- confint(model06) |> exp()
# temp07 <- confint(model07) |> exp()
# temp08 <- confint(model08) |> exp()
# temp09 <- confint(model09) |> exp()
# temp10 <- confint(model10) |> exp()
# temp14 <- confint(model14) |> exp()
# temp15 <- confint(model15) |> exp()
# temp16 <- confint(model16) |> exp()
# temp17 <- confint(model17) |> exp()
# temp18 <- confint(model18) |> exp()
# temp20 <- confint(model20) |> exp()

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- summary(model01)@coef3[,4]
# temp02 <- summary(model02)@coef3[,4]
# temp03 <- summary(model03)@coef3[,4]
# temp04 <- summary(model04)@coef3[,4]
# temp05 <- summary(model05)@coef3[,4]
# temp06 <- summary(model06)@coef3[,4]
# temp07 <- summary(model07)@coef3[,4]
# temp08 <- summary(model08)@coef3[,4]
# temp09 <- summary(model09)@coef3[,4]
# temp10 <- summary(model10)@coef3[,4]
# temp14 <- summary(model14)@coef3[,4]
# temp15 <- summary(model15)@coef3[,4]
# temp16 <- summary(model16)@coef3[,4]
# temp17 <- summary(model17)@coef3[,4]
# temp18 <- summary(model18)@coef3[,4]
# temp20 <- summary(model20)@coef3[,4]
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted regression
# model01 <- vglm(next_pan_out ~ info01 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model02 <- vglm(next_pan_out ~ info02 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model03 <- vglm(next_pan_out ~ info03 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model04 <- vglm(next_pan_out ~ info04 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model05 <- vglm(next_pan_out ~ info05 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model06 <- vglm(next_pan_out ~ info06 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model07 <- vglm(next_pan_out ~ info07 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model08 <- vglm(next_pan_out ~ info08 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model09 <- vglm(next_pan_out ~ info09 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model10 <- vglm(next_pan_out ~ info10 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model14 <- vglm(next_pan_out ~ info14 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model15 <- vglm(next_pan_out ~ info15 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model16 <- vglm(next_pan_out ~ info16 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model17 <- vglm(next_pan_out ~ info17 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model18 <- vglm(next_pan_out ~ info18 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)
# model20 <- vglm(next_pan_out ~ info20 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# Adjusted OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))
temp10 <- exp(coef(model10))
temp14 <- exp(coef(model14))
temp15 <- exp(coef(model15))
temp16 <- exp(coef(model16))
temp17 <- exp(coef(model17))
temp18 <- exp(coef(model18))
temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- confint(model01) |> exp()
# temp02 <- confint(model02) |> exp()
# temp03 <- confint(model03) |> exp()
# temp04 <- confint(model04) |> exp()
# temp05 <- confint(model05) |> exp()
# temp06 <- confint(model06) |> exp()
# temp07 <- confint(model07) |> exp()
# temp08 <- confint(model08) |> exp()
# temp09 <- confint(model09) |> exp()
# temp10 <- confint(model10) |> exp()
# temp14 <- confint(model14) |> exp()
# temp15 <- confint(model15) |> exp()
# temp16 <- confint(model16) |> exp()
# temp17 <- confint(model17) |> exp()
# temp18 <- confint(model18) |> exp()
# temp20 <- confint(model20) |> exp()

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)

temp <- rbind(temp01, temp02, temp03, temp04)
temp <- rbind(temp05, temp06, temp07, temp08)
temp <- rbind(temp09, temp10, temp14, temp15)
temp <- rbind(temp16, temp17, temp18, temp20)

temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- summary(model01)@coef3[,4]
# temp02 <- summary(model02)@coef3[,4]
# temp03 <- summary(model03)@coef3[,4]
# temp04 <- summary(model04)@coef3[,4]
# temp05 <- summary(model05)@coef3[,4]
# temp06 <- summary(model06)@coef3[,4]
# temp07 <- summary(model07)@coef3[,4]
# temp08 <- summary(model08)@coef3[,4]
# temp09 <- summary(model09)@coef3[,4]
# temp10 <- summary(model10)@coef3[,4]
# temp14 <- summary(model14)@coef3[,4]
# temp15 <- summary(model15)@coef3[,4]
# temp16 <- summary(model16)@coef3[,4]
# temp17 <- summary(model17)@coef3[,4]
# temp18 <- summary(model18)@coef3[,4]
# temp20 <- summary(model20)@coef3[,4]
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Past inf
temp <- table(data$inf_2021_all, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ inf_2021_all, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ inf_2021_all + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### COVID fear
temp <- table(data$koronafuan, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ koronafuan, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ koronafuan + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Vac misinfo
temp <- table(data$vac_inbou, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ vac_inbou, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ vac_inbou + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### COVID conspi
temp <- table(data$korona_inbou, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ korona_inbou, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ korona_inbou + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### ID knowledge
temp <- table(data$chishiki, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ chishiki, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ chishiki + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### COVID vac
data <- data %>%
  mutate(korona_vac2 = case_when(
    Q39 <= 2 ~ "1",
    Q39 >= 4 ~ "0",
    TRUE ~ NA))

temp <- table(data$korona_vac2, data$next_pan_out)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out ~ korona_vac2, family = multinomial(refLevel = "4"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out ~ korona_vac2 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "4"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

######### ORs for Next perception sub3 #########

### Next pandemic when
### Next pandemic 40
# make group for outcome

table(data$Q39)
table(data$Q40)

data <- data %>%
  mutate(next_pan_out3 = case_when(
    Q40 <= 3 ~ "123",
    Q40 <= 5 ~ "45",
    Q40 <= 7 ~ "67"))

data$next_pan_out3 <- as.factor(data$next_pan_out3)
table(data$next_pan_out3)

###### Demographic ######

### Age
temp <- table(data$nenrei, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ nenrei, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### SEX
temp <- table(data$SEX, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ SEX, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Income
temp <- table(data$nenshu, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ nenshu, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Education
temp <- table(data$gakureki, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ gakureki, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Job health ### EXCLUDING STUDENT
# table(data$shigoto)
# 
# data <- data %>%
#   mutate(shigotoH = case_when(
#     shigoto == "med" ~ "med",
#     shigoto == "student" ~ NA,
#     TRUE  ~ "nonmed"))

# Level
# data$shigotoH <- factor(data$shigotoH, levels = c("nonmed", "med"))

table(data$shigoto, data$shigotoH)

temp <- table(data$shigotoH, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ shigotoH, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ shigotoH + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Anxiety
temp <- table(data$shinpai, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ shinpai, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ shinpai + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Kyodo
temp <- table(data$kyodo, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ kyodo, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ kyodo + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### ExtKyodo
temp <- table(data$ext_kyodo, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ ext_kyodo, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ ext_kyodo + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Sado
temp <- table(data$sado, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ sado, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ sado + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Ext-Sado
temp <- table(data$ext_sado, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ ext_sado, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ ext_sado + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Happiness
temp <- table(data$shiawase, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ shiawase, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ shiawase + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Info
temp <- table(data$info01, data$next_pan_out3)
temp <- rbind(temp, table(data$info02, data$next_pan_out3))
temp <- rbind(temp, table(data$info03, data$next_pan_out3))
temp <- rbind(temp, table(data$info04, data$next_pan_out3))
temp <- rbind(temp, table(data$info05, data$next_pan_out3))
temp <- rbind(temp, table(data$info06, data$next_pan_out3))
temp <- rbind(temp, table(data$info07, data$next_pan_out3))
temp <- rbind(temp, table(data$info08, data$next_pan_out3))
temp <- rbind(temp, table(data$info09, data$next_pan_out3))
temp <- rbind(temp, table(data$info10, data$next_pan_out3))
temp <- rbind(temp, table(data$info14, data$next_pan_out3))
temp <- rbind(temp, table(data$info15, data$next_pan_out3))
temp <- rbind(temp, table(data$info16, data$next_pan_out3))
temp <- rbind(temp, table(data$info17, data$next_pan_out3))
temp <- rbind(temp, table(data$info18, data$next_pan_out3))
temp <- rbind(temp, table(data$info20, data$next_pan_out3))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# regression
# model01 <- vglm(next_pan_out3 ~ info01, family = multinomial(refLevel = "45"), data = data)
# model02 <- vglm(next_pan_out3 ~ info02, family = multinomial(refLevel = "45"), data = data)
# model03 <- vglm(next_pan_out3 ~ info03, family = multinomial(refLevel = "45"), data = data)
# model04 <- vglm(next_pan_out3 ~ info04, family = multinomial(refLevel = "45"), data = data)
# model05 <- vglm(next_pan_out3 ~ info05, family = multinomial(refLevel = "45"), data = data)
# model06 <- vglm(next_pan_out3 ~ info06, family = multinomial(refLevel = "45"), data = data)
# model07 <- vglm(next_pan_out3 ~ info07, family = multinomial(refLevel = "45"), data = data)
# model08 <- vglm(next_pan_out3 ~ info08, family = multinomial(refLevel = "45"), data = data)
# model09 <- vglm(next_pan_out3 ~ info09, family = multinomial(refLevel = "45"), data = data)
# model10 <- vglm(next_pan_out3 ~ info10, family = multinomial(refLevel = "45"), data = data)
# model14 <- vglm(next_pan_out3 ~ info14, family = multinomial(refLevel = "45"), data = data)
# model15 <- vglm(next_pan_out3 ~ info15, family = multinomial(refLevel = "45"), data = data)
# model16 <- vglm(next_pan_out3 ~ info16, family = multinomial(refLevel = "45"), data = data)
# model17 <- vglm(next_pan_out3 ~ info17, family = multinomial(refLevel = "45"), data = data)
# model18 <- vglm(next_pan_out3 ~ info18, family = multinomial(refLevel = "45"), data = data)
# model20 <- vglm(next_pan_out3 ~ info20, family = multinomial(refLevel = "45"), data = data)

# OR, 95CI, p-value
# temp01 <- exp(coef(model01))
# temp02 <- exp(coef(model02))
# temp03 <- exp(coef(model03))
# temp04 <- exp(coef(model04))
# temp05 <- exp(coef(model05))
# temp06 <- exp(coef(model06))
# temp07 <- exp(coef(model07))
# temp08 <- exp(coef(model08))
# temp09 <- exp(coef(model09))
# temp10 <- exp(coef(model10))
# temp14 <- exp(coef(model14))
# temp15 <- exp(coef(model15))
# temp16 <- exp(coef(model16))
# temp17 <- exp(coef(model17))
# temp18 <- exp(coef(model18))
# temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- confint(model01) |> exp()
# temp02 <- confint(model02) |> exp()
# temp03 <- confint(model03) |> exp()
# temp04 <- confint(model04) |> exp()
# temp05 <- confint(model05) |> exp()
# temp06 <- confint(model06) |> exp()
# temp07 <- confint(model07) |> exp()
# temp08 <- confint(model08) |> exp()
# temp09 <- confint(model09) |> exp()
# temp10 <- confint(model10) |> exp()
# temp14 <- confint(model14) |> exp()
# temp15 <- confint(model15) |> exp()
# temp16 <- confint(model16) |> exp()
# temp17 <- confint(model17) |> exp()
# temp18 <- confint(model18) |> exp()
# temp20 <- confint(model20) |> exp()

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- summary(model01)@coef3[,4]
# temp02 <- summary(model02)@coef3[,4]
# temp03 <- summary(model03)@coef3[,4]
# temp04 <- summary(model04)@coef3[,4]
# temp05 <- summary(model05)@coef3[,4]
# temp06 <- summary(model06)@coef3[,4]
# temp07 <- summary(model07)@coef3[,4]
# temp08 <- summary(model08)@coef3[,4]
# temp09 <- summary(model09)@coef3[,4]
# temp10 <- summary(model10)@coef3[,4]
# temp14 <- summary(model14)@coef3[,4]
# temp15 <- summary(model15)@coef3[,4]
# temp16 <- summary(model16)@coef3[,4]
# temp17 <- summary(model17)@coef3[,4]
# temp18 <- summary(model18)@coef3[,4]
# temp20 <- summary(model20)@coef3[,4]
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted regression
# model01 <- vglm(next_pan_out3 ~ info01 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model02 <- vglm(next_pan_out3 ~ info02 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model03 <- vglm(next_pan_out3 ~ info03 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model04 <- vglm(next_pan_out3 ~ info04 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model05 <- vglm(next_pan_out3 ~ info05 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model06 <- vglm(next_pan_out3 ~ info06 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model07 <- vglm(next_pan_out3 ~ info07 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model08 <- vglm(next_pan_out3 ~ info08 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model09 <- vglm(next_pan_out3 ~ info09 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model10 <- vglm(next_pan_out3 ~ info10 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model14 <- vglm(next_pan_out3 ~ info14 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model15 <- vglm(next_pan_out3 ~ info15 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model16 <- vglm(next_pan_out3 ~ info16 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model17 <- vglm(next_pan_out3 ~ info17 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model18 <- vglm(next_pan_out3 ~ info18 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)
# model20 <- vglm(next_pan_out3 ~ info20 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# Adjusted OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))
temp10 <- exp(coef(model10))
temp14 <- exp(coef(model14))
temp15 <- exp(coef(model15))
temp16 <- exp(coef(model16))
temp17 <- exp(coef(model17))
temp18 <- exp(coef(model18))
temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- confint(model01) |> exp()
# temp02 <- confint(model02) |> exp()
# temp03 <- confint(model03) |> exp()
# temp04 <- confint(model04) |> exp()
# temp05 <- confint(model05) |> exp()
# temp06 <- confint(model06) |> exp()
# temp07 <- confint(model07) |> exp()
# temp08 <- confint(model08) |> exp()
# temp09 <- confint(model09) |> exp()
# temp10 <- confint(model10) |> exp()
# temp14 <- confint(model14) |> exp()
# temp15 <- confint(model15) |> exp()
# temp16 <- confint(model16) |> exp()
# temp17 <- confint(model17) |> exp()
# temp18 <- confint(model18) |> exp()
# temp20 <- confint(model20) |> exp()

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)

temp <- rbind(temp01, temp02, temp03, temp04)
temp <- rbind(temp05, temp06, temp07, temp08)
temp <- rbind(temp09, temp10, temp14, temp15)
temp <- rbind(temp16, temp17, temp18, temp20)

temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- summary(model01)@coef3[,4]
# temp02 <- summary(model02)@coef3[,4]
# temp03 <- summary(model03)@coef3[,4]
# temp04 <- summary(model04)@coef3[,4]
# temp05 <- summary(model05)@coef3[,4]
# temp06 <- summary(model06)@coef3[,4]
# temp07 <- summary(model07)@coef3[,4]
# temp08 <- summary(model08)@coef3[,4]
# temp09 <- summary(model09)@coef3[,4]
# temp10 <- summary(model10)@coef3[,4]
# temp14 <- summary(model14)@coef3[,4]
# temp15 <- summary(model15)@coef3[,4]
# temp16 <- summary(model16)@coef3[,4]
# temp17 <- summary(model17)@coef3[,4]
# temp18 <- summary(model18)@coef3[,4]
# temp20 <- summary(model20)@coef3[,4]
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Past inf
temp <- table(data$inf_2021_all, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ inf_2021_all, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ inf_2021_all + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### COVID fear
temp <- table(data$koronafuan, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ koronafuan, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ koronafuan + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Vac misinfo
temp <- table(data$vac_inbou, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ vac_inbou, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ vac_inbou + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### COVID conspi
temp <- table(data$korona_inbou, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ korona_inbou, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ korona_inbou + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### ID knowledge
temp <- table(data$chishiki, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ chishiki, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ chishiki + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### COVID vac
# data <- data %>%
#   mutate(korona_vac2 = case_when(
#     Q39 <= 2 ~ "1",
#     Q39 >= 4 ~ "0",
#     TRUE ~ NA))

temp <- table(data$korona_vac2, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ korona_vac2, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ korona_vac2 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Info No
# Relevel
data$infoNo <- relevel(as.factor(data$infoNo), ref = "0")

temp <- table(data$infoNo, data$next_pan_out3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ infoNo, family = multinomial(refLevel = "45"), data = data)

# OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# # P
temp <- summary(fit_vg)@coef3[,4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ infoNo + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# adjusted OR
temp <- exp(coef(fit_vg))
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted 95%CI
temp <- confint(fit_vg) |> exp()
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

# adjusted  P
# temp <- summary(fit_vg)@coef3[,4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Info all
# Relevel
# adjusted Poly logistic regression
fit_vg <- vglm(next_pan_out3 ~ info01 + info02 + info03 + info04 + info05 + info06 + info07 + info08 + info09 + info10 + info14 + info15 + info16 + info17 + info18 + info20 +
               nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "45"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### Next vac when never ###
### 41.2
# make group for outcome

table(data$Q41.2)
table(data$Q41.2, data$next_pan_out)

# EXCLUDE next pandemic never #
data <- data %>%
  mutate(next_vac_never = case_when(
    # Q40 == 7 ~ NA,
    Q41.2 == 6 ~ 0,
    TRUE ~ 1))

table(data$next_vac_never, data$next_pan_out)

###### Demographic ######

### Age
temp <- table(data$nenrei, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ nenrei, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### SEX
temp <- table(data$SEX, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ SEX, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Income
temp <- table(data$nenshu, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ nenshu, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Education
temp <- table(data$gakureki, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

###### Adjustment for demographic ######

# Binomial logistic regression
model <- glm(next_vac_never ~ nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Job health ### EXCLUDING STUDENT
table(data$shigoto)

data <- data %>%
  mutate(shigotoH = case_when(
    shigoto == "med" ~ "med",
    shigoto == "student" ~ NA,
    TRUE  ~ "nonmed"))

# data <- data %>%
#   mutate(shigotoH = case_when(
#     Q6 == 15 ~ "med",
#     shigoto == "student" ~ NA,
#     TRUE  ~ "nonmed"))

table(data$shigoto)
table(data$shigotoH)

# Level
data$shigotoH <- factor(data$shigotoH, levels = c("nonmed", "med"))

temp <- table(data$shigotoH, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ shigotoH, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ shigotoH + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Anxiety
temp <- table(data$shinpai, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ shinpai, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ shinpai + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)


### Kyodo
temp <- table(data$kyodo, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ kyodo, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ kyodo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Ext-Kyodo
temp <- table(data$ext_kyodo, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ ext_kyodo, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ ext_kyodo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Sado
temp <- table(data$sado, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ sado, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ sado + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Ext-Sado
temp <- table(data$ext_sado, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ ext_sado, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ ext_sado + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Happiness
temp <- table(data$shiawase, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ shiawase, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ shiawase + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Info
temp <- table(data$info01, data$next_vac_never)
temp <- rbind(temp, table(data$info02, data$next_vac_never))
temp <- rbind(temp, table(data$info03, data$next_vac_never))
temp <- rbind(temp, table(data$info04, data$next_vac_never))
temp <- rbind(temp, table(data$info05, data$next_vac_never))
temp <- rbind(temp, table(data$info06, data$next_vac_never))
temp <- rbind(temp, table(data$info07, data$next_vac_never))
temp <- rbind(temp, table(data$info08, data$next_vac_never))
temp <- rbind(temp, table(data$info09, data$next_vac_never))
temp <- rbind(temp, table(data$info10, data$next_vac_never))
temp <- rbind(temp, table(data$info14, data$next_vac_never))
temp <- rbind(temp, table(data$info15, data$next_vac_never))
temp <- rbind(temp, table(data$info16, data$next_vac_never))
temp <- rbind(temp, table(data$info17, data$next_vac_never))
temp <- rbind(temp, table(data$info18, data$next_vac_never))
temp <- rbind(temp, table(data$info20, data$next_vac_never))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# regression
model01 <- glm(next_vac_never ~ info01, data = data, family = binomial)
model02 <- glm(next_vac_never ~ info02, data = data, family = binomial)
model03 <- glm(next_vac_never ~ info03, data = data, family = binomial)
model04 <- glm(next_vac_never ~ info04, data = data, family = binomial)
model05 <- glm(next_vac_never ~ info05, data = data, family = binomial)
model06 <- glm(next_vac_never ~ info06, data = data, family = binomial)
model07 <- glm(next_vac_never ~ info07, data = data, family = binomial)
model08 <- glm(next_vac_never ~ info08, data = data, family = binomial)
model09 <- glm(next_vac_never ~ info09, data = data, family = binomial)
model10 <- glm(next_vac_never ~ info10, data = data, family = binomial)
model14 <- glm(next_vac_never ~ info14, data = data, family = binomial)
model15 <- glm(next_vac_never ~ info15, data = data, family = binomial)
model16 <- glm(next_vac_never ~ info16, data = data, family = binomial)
model17 <- glm(next_vac_never ~ info17, data = data, family = binomial)
model18 <- glm(next_vac_never ~ info18, data = data, family = binomial)
model20 <- glm(next_vac_never ~ info20, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))
temp10 <- exp(coef(model10))
temp14 <- exp(coef(model14))
temp15 <- exp(coef(model15))
temp16 <- exp(coef(model16))
temp17 <- exp(coef(model17))
temp18 <- exp(coef(model18))
temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# temp07 <- exp(confint(model07))
# temp08 <- exp(confint(model08))
# temp09 <- exp(confint(model09))
# temp10 <- exp(confint(model10))
# temp14 <- exp(confint(model14))
# temp15 <- exp(confint(model15))
# temp16 <- exp(confint(model16))
# temp17 <- exp(confint(model17))
# temp18 <- exp(confint(model18))
# temp20 <- exp(confint(model20))
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]
temp07 <- summary(model07)$coefficients[, 4]
temp08 <- summary(model08)$coefficients[, 4]
temp09 <- summary(model09)$coefficients[, 4]
temp10 <- summary(model10)$coefficients[, 4]
temp14 <- summary(model14)$coefficients[, 4]
temp15 <- summary(model15)$coefficients[, 4]
temp16 <- summary(model16)$coefficients[, 4]
temp17 <- summary(model17)$coefficients[, 4]
temp18 <- summary(model18)$coefficients[, 4]
temp20 <- summary(model20)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted regression
model01 <- glm(next_vac_never ~ info01 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model02 <- glm(next_vac_never ~ info02 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model03 <- glm(next_vac_never ~ info03 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model04 <- glm(next_vac_never ~ info04 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model05 <- glm(next_vac_never ~ info05 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model06 <- glm(next_vac_never ~ info06 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model07 <- glm(next_vac_never ~ info07 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model08 <- glm(next_vac_never ~ info08 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model09 <- glm(next_vac_never ~ info09 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model10 <- glm(next_vac_never ~ info10 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model14 <- glm(next_vac_never ~ info14 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model15 <- glm(next_vac_never ~ info15 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model16 <- glm(next_vac_never ~ info16 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model17 <- glm(next_vac_never ~ info17 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model18 <- glm(next_vac_never ~ info18 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model20 <- glm(next_vac_never ~ info20 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))
temp10 <- exp(coef(model10))
temp14 <- exp(coef(model14))
temp15 <- exp(coef(model15))
temp16 <- exp(coef(model16))
temp17 <- exp(coef(model17))
temp18 <- exp(coef(model18))
temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# temp07 <- exp(confint(model07))
# temp08 <- exp(confint(model08))
# temp09 <- exp(confint(model09))
# temp10 <- exp(confint(model10))
# temp14 <- exp(confint(model14))
# temp15 <- exp(confint(model15))
# temp16 <- exp(confint(model16))
# temp17 <- exp(confint(model17))
# temp18 <- exp(confint(model18))
# temp20 <- exp(confint(model20))
# 
temp <- rbind(temp01, temp02, temp03, temp04)
temp <- rbind(temp05, temp06, temp07, temp08)
temp <- rbind(temp09, temp10, temp14, temp15)
temp <- rbind(temp16, temp17, temp18, temp20)

temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]
temp07 <- summary(model07)$coefficients[, 4]
temp08 <- summary(model08)$coefficients[, 4]
temp09 <- summary(model09)$coefficients[, 4]
temp10 <- summary(model10)$coefficients[, 4]
temp14 <- summary(model14)$coefficients[, 4]
temp15 <- summary(model15)$coefficients[, 4]
temp16 <- summary(model16)$coefficients[, 4]
temp17 <- summary(model17)$coefficients[, 4]
temp18 <- summary(model18)$coefficients[, 4]
temp20 <- summary(model20)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Past inf
temp <- table(data$inf_2021_all, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ inf_2021_all, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ inf_2021_all + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Adv1
temp <- table(data$adv1, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ adv1, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ adv1 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### COVID fear
temp <- table(data$koronafuan, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ koronafuan, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ koronafuan + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Vac misinfo
temp <- table(data$vac_inbou, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ vac_inbou, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ vac_inbou + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### COVID conspi
temp <- table(data$korona_inbou, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ korona_inbou, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ korona_inbou + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### ID knowledge
temp <- table(data$chishiki, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ chishiki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ chishiki + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### COVID vac
# data <- data %>%
#   mutate(korona_vac2 = case_when(
#     Q39 <= 2 ~ "1",
#     Q39 >= 4 ~ "0",
#     TRUE ~ NA))

temp <- table(data$korona_vac2, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_never ~ korona_vac2, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ korona_vac2 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# ### Next pan when4
# temp <- table(data$next_pan, data$next_vac_never)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # Level
# data$next_pan <- factor(data$next_pan, levels = c("4", "1", "5", "6", "7"))
# 
# 
# # Binomial logistic regression
# model <- glm(next_vac_never ~ next_pan, data = data, family = binomial)
# 
# # OR, 95CI, p-value
# temp1 <- exp(coef(model))
# temp2 <- exp(confint(model))
# temp3 <- summary(model)$coefficients[, 4]
# 
# temp <- cbind(temp1, temp2, temp3)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# 
# 
# # Adjusted Binomial logistic regression
# model <- glm(next_vac_never ~ next_pan + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
# 
# # OR, 95CI, p-value
# temp1 <- exp(coef(model))
# temp2 <- exp(confint(model))
# temp3 <- summary(model)$coefficients[, 4]
# 
# temp <- cbind(temp1, temp2, temp3)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Next pan when3
temp <- table(data$next_pan_out3, data$next_vac_never)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$next_pan_out3 <- factor(data$next_pan_out3, levels = c("45", "123", "67"))

# Binomial logistic regression
model <- glm(next_vac_never ~ next_pan_out3, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_never ~ next_pan_out3 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# ### Info No
# temp <- table(data$infoNo, data$next_vac_never)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # Binomial logistic regression
# model <- glm(next_vac_never ~ infoNo, data = data, family = binomial)
# 
# # OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # temp <- exp(confint(model))
# # temp
# # write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# temp <- summary(model)$coefficients[, 4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # Adjusted Binomial logistic regression
# model <- glm(next_vac_never ~ infoNo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
# 
# # Adjusted OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # temp <- exp(confint(model))
# # temp
# # write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# temp <- summary(model)$coefficients[, 4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Infoall
model <- glm(next_vac_never ~ info01 + info02 + info03 + info04 + info05 + info06 + info07 + info08 + info09 + info10 + info14 + info15 + info16 + info17 + info18 + info20 +
               nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp1 <- exp(coef(model))
temp2 <- exp(confint(model))
temp3 <- summary(model)$coefficients[, 4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

###### Next vac 100 days

data <- data %>%
  mutate(next_vac_100 = case_when(
    Q40 == 7 ~ NA,
    Q41.2 == 6 ~ NA,
    Q41.2 == 1 ~ 1,
    TRUE ~ 0))

table(data$next_vac_100, data$next_pan_out)

###### Demographic ######

### Age
temp <- table(data$nenrei, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ nenrei, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### SEX
temp <- table(data$SEX, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ SEX, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Income
temp <- table(data$nenshu, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ nenshu, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Education
temp <- table(data$gakureki, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

###### Adjustment for demographic ######

# Binomial logistic regression
model <- glm(next_vac_100 ~ nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Job health ### EXCLUDING STUDENT
table(data$shigoto)

# data <- data %>%
#   mutate(shigotoH = case_when(
#     shigoto == "med" ~ "med",
#     shigoto == "student" ~ NA,
#     TRUE  ~ "nonmed"))

# data <- data %>%
#   mutate(shigotoH = case_when(
#     Q6 == 15 ~ "med",
#     shigoto == "student" ~ NA,
#     TRUE  ~ "nonmed"))

table(data$shigoto)
table(data$shigotoH)

# Level
# data$shigotoH <- factor(data$shigotoH, levels = c("nonmed", "med"))

temp <- table(data$shigotoH, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ shigotoH, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ shigotoH + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Anxiety
temp <- table(data$shinpai, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ shinpai, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ shinpai + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Kyodo
temp <- table(data$kyodo, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ kyodo, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ kyodo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Ext-Kyodo
temp <- table(data$ext_kyodo, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ ext_kyodo, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ ext_kyodo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Sado
temp <- table(data$sado, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ sado, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ sado + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Ext-Sado
temp <- table(data$ext_sado, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ ext_sado, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ ext_sado + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Happiness
temp <- table(data$shiawase, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ shiawase, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ shiawase + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Info
temp <- table(data$info01, data$next_vac_100)
temp <- rbind(temp, table(data$info02, data$next_vac_100))
temp <- rbind(temp, table(data$info03, data$next_vac_100))
temp <- rbind(temp, table(data$info04, data$next_vac_100))
temp <- rbind(temp, table(data$info05, data$next_vac_100))
temp <- rbind(temp, table(data$info06, data$next_vac_100))
temp <- rbind(temp, table(data$info07, data$next_vac_100))
temp <- rbind(temp, table(data$info08, data$next_vac_100))
temp <- rbind(temp, table(data$info09, data$next_vac_100))
temp <- rbind(temp, table(data$info10, data$next_vac_100))
temp <- rbind(temp, table(data$info14, data$next_vac_100))
temp <- rbind(temp, table(data$info15, data$next_vac_100))
temp <- rbind(temp, table(data$info16, data$next_vac_100))
temp <- rbind(temp, table(data$info17, data$next_vac_100))
temp <- rbind(temp, table(data$info18, data$next_vac_100))
temp <- rbind(temp, table(data$info20, data$next_vac_100))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# regression
model01 <- glm(next_vac_100 ~ info01, data = data, family = binomial)
model02 <- glm(next_vac_100 ~ info02, data = data, family = binomial)
model03 <- glm(next_vac_100 ~ info03, data = data, family = binomial)
model04 <- glm(next_vac_100 ~ info04, data = data, family = binomial)
model05 <- glm(next_vac_100 ~ info05, data = data, family = binomial)
model06 <- glm(next_vac_100 ~ info06, data = data, family = binomial)
model07 <- glm(next_vac_100 ~ info07, data = data, family = binomial)
model08 <- glm(next_vac_100 ~ info08, data = data, family = binomial)
model09 <- glm(next_vac_100 ~ info09, data = data, family = binomial)
model10 <- glm(next_vac_100 ~ info10, data = data, family = binomial)
model14 <- glm(next_vac_100 ~ info14, data = data, family = binomial)
model15 <- glm(next_vac_100 ~ info15, data = data, family = binomial)
model16 <- glm(next_vac_100 ~ info16, data = data, family = binomial)
model17 <- glm(next_vac_100 ~ info17, data = data, family = binomial)
model18 <- glm(next_vac_100 ~ info18, data = data, family = binomial)
model20 <- glm(next_vac_100 ~ info20, data = data, family = binomial)

# OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))
temp10 <- exp(coef(model10))
temp14 <- exp(coef(model14))
temp15 <- exp(coef(model15))
temp16 <- exp(coef(model16))
temp17 <- exp(coef(model17))
temp18 <- exp(coef(model18))
temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# temp07 <- exp(confint(model07))
# temp08 <- exp(confint(model08))
# temp09 <- exp(confint(model09))
# temp10 <- exp(confint(model10))
# temp14 <- exp(confint(model14))
# temp15 <- exp(confint(model15))
# temp16 <- exp(confint(model16))
# temp17 <- exp(confint(model17))
# temp18 <- exp(confint(model18))
# temp20 <- exp(confint(model20))
# 
# temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]
temp07 <- summary(model07)$coefficients[, 4]
temp08 <- summary(model08)$coefficients[, 4]
temp09 <- summary(model09)$coefficients[, 4]
temp10 <- summary(model10)$coefficients[, 4]
temp14 <- summary(model14)$coefficients[, 4]
temp15 <- summary(model15)$coefficients[, 4]
temp16 <- summary(model16)$coefficients[, 4]
temp17 <- summary(model17)$coefficients[, 4]
temp18 <- summary(model18)$coefficients[, 4]
temp20 <- summary(model20)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted regression
model01 <- glm(next_vac_100 ~ info01 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model02 <- glm(next_vac_100 ~ info02 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model03 <- glm(next_vac_100 ~ info03 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model04 <- glm(next_vac_100 ~ info04 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model05 <- glm(next_vac_100 ~ info05 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model06 <- glm(next_vac_100 ~ info06 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model07 <- glm(next_vac_100 ~ info07 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model08 <- glm(next_vac_100 ~ info08 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model09 <- glm(next_vac_100 ~ info09 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model10 <- glm(next_vac_100 ~ info10 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model14 <- glm(next_vac_100 ~ info14 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model15 <- glm(next_vac_100 ~ info15 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model16 <- glm(next_vac_100 ~ info16 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model17 <- glm(next_vac_100 ~ info17 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model18 <- glm(next_vac_100 ~ info18 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
model20 <- glm(next_vac_100 ~ info20 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp01 <- exp(coef(model01))
temp02 <- exp(coef(model02))
temp03 <- exp(coef(model03))
temp04 <- exp(coef(model04))
temp05 <- exp(coef(model05))
temp06 <- exp(coef(model06))
temp07 <- exp(coef(model07))
temp08 <- exp(coef(model08))
temp09 <- exp(coef(model09))
temp10 <- exp(coef(model10))
temp14 <- exp(coef(model14))
temp15 <- exp(coef(model15))
temp16 <- exp(coef(model16))
temp17 <- exp(coef(model17))
temp18 <- exp(coef(model18))
temp20 <- exp(coef(model20))

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)

temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp01 <- exp(confint(model01))
# temp02 <- exp(confint(model02))
# temp03 <- exp(confint(model03))
# temp04 <- exp(confint(model04))
# temp05 <- exp(confint(model05))
# temp06 <- exp(confint(model06))
# temp07 <- exp(confint(model07))
# temp08 <- exp(confint(model08))
# temp09 <- exp(confint(model09))
# temp10 <- exp(confint(model10))
# temp14 <- exp(confint(model14))
# temp15 <- exp(confint(model15))
# temp16 <- exp(confint(model16))
# temp17 <- exp(confint(model17))
# temp18 <- exp(confint(model18))
# temp20 <- exp(confint(model20))
# 
# temp <- rbind(temp01, temp02, temp03, temp04)
# temp <- rbind(temp05, temp06, temp07, temp08)
# temp <- rbind(temp09, temp10, temp14, temp15)
# temp <- rbind(temp16, temp17, temp18, temp20)
# 
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp01 <- summary(model01)$coefficients[, 4]
temp02 <- summary(model02)$coefficients[, 4]
temp03 <- summary(model03)$coefficients[, 4]
temp04 <- summary(model04)$coefficients[, 4]
temp05 <- summary(model05)$coefficients[, 4]
temp06 <- summary(model06)$coefficients[, 4]
temp07 <- summary(model07)$coefficients[, 4]
temp08 <- summary(model08)$coefficients[, 4]
temp09 <- summary(model09)$coefficients[, 4]
temp10 <- summary(model10)$coefficients[, 4]
temp14 <- summary(model14)$coefficients[, 4]
temp15 <- summary(model15)$coefficients[, 4]
temp16 <- summary(model16)$coefficients[, 4]
temp17 <- summary(model17)$coefficients[, 4]
temp18 <- summary(model18)$coefficients[, 4]
temp20 <- summary(model20)$coefficients[, 4]

temp <- rbind(temp01, temp02, temp03, temp04, temp05, temp06, temp07, temp08, temp09, temp10, temp14, temp15, temp16, temp17, temp18, temp20)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Past inf
temp <- table(data$inf_2021_all, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ inf_2021_all, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ inf_2021_all + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Adv1
temp <- table(data$adv1, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ adv1, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ adv1 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### COVID fear
temp <- table(data$koronafuan, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ koronafuan, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ koronafuan + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Vac misinfo
temp <- table(data$vac_inbou, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ vac_inbou, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ vac_inbou + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### COVID conspi
temp <- table(data$korona_inbou, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ korona_inbou, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ korona_inbou + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### ID knowledge
temp <- table(data$chishiki, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ chishiki, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ chishiki + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### COVID vac
# data <- data %>%
#   mutate(korona_vac2 = case_when(
#     Q39 <= 2 ~ "1",
#     Q39 >= 4 ~ "0",
#     TRUE ~ NA))

temp <- table(data$korona_vac2, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ korona_vac2, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ korona_vac2 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Next pan when
temp <- table(data$next_pan, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$next_pan <- factor(data$next_pan, levels = c("4", "1", "5", "6", "7"))
table(data$next_pan, data$next_vac_100)

# Binomial logistic regression
model <- glm(next_vac_100 ~ next_pan, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ next_pan + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Next pan when3
temp <- table(data$next_pan_out3, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$next_pan_out3 <- factor(data$next_pan_out3, levels = c("45", "123", "67"))

# Binomial logistic regression
model <- glm(next_vac_100 ~ next_pan_out3, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ next_pan_out3 + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Info No
temp <- table(data$infoNo, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Binomial logistic regression
model <- glm(next_vac_100 ~ infoNo, data = data, family = binomial)

# OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Adjusted Binomial logistic regression
model <- glm(next_vac_100 ~ infoNo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)

# Adjusted OR, 95CI, p-value
temp <- exp(coef(model))
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# temp <- exp(confint(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

temp <- summary(model)$coefficients[, 4]
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

###### Revise vac 100 days category

data <- data %>%
  mutate(next_vac_100 = case_when(
    # Q40 == 7 ~ NA,
    Q41.2 == 6 ~ NA_character_,
    Q41.2 == 1 ~ "03",
    Q41.2 == 2 ~ "06",
    Q41.2 == 3 ~ "12",
    Q41.2 == 4 ~ "24",
    Q41.2 == 5 ~ "48",
    TRUE ~ NA_character_))

# factor, ref = "12"
data$next_vac_100 <- factor(data$next_vac_100, levels = c("03", "06", "12", "24", "48"))

table(data$next_vac_100)

###### Demographic ######

### Age
temp <- table(data$nenrei, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ nenrei, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp
#write including row name
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

### SEX
temp <- table(data$SEX, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ SEX, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Income
temp <- table(data$nenshu, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ nenshu, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Education
temp <- table(data$gakureki, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Job health ### EXCLUDING STUDENT
table(data$shigoto)

# data <- data %>%
#   mutate(shigotoH = case_when(
#     shigoto == "med" ~ "med",
#     shigoto == "student" ~ NA,
#     TRUE  ~ "nonmed"))

# data <- data %>%
#   mutate(shigotoH = case_when(
#     Q6 == 15 ~ "med",
#     shigoto == "student" ~ NA,
#     TRUE  ~ "nonmed"))

table(data$shigoto)
table(data$shigotoH)

# Level
data$shigotoH <- factor(data$shigotoH, levels = c("nonmed", "med"))

temp <- table(data$shigotoH, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ shigotoH, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ shigotoH + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Anxiety
temp <- table(data$shinpai, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ shinpai, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ shinpai + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Kyodo
temp <- table(data$kyodo, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ kyodo, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ kyodo + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Ext-Kyodo
temp <- table(data$ext_kyodo, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ ext_kyodo, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ ext_kyodo + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Sado
temp <- table(data$sado, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ sado, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ sado + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Ext-Sado
temp <- table(data$ext_sado, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ ext_sado, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ ext_sado + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Happiness
temp <- table(data$shiawase, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ shiawase, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ shiawase + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Info
temp <- table(data$info01, data$next_vac_100)
temp <- rbind(temp, table(data$info02, data$next_vac_100))
temp <- rbind(temp, table(data$info03, data$next_vac_100))
temp <- rbind(temp, table(data$info04, data$next_vac_100))
temp <- rbind(temp, table(data$info05, data$next_vac_100))
temp <- rbind(temp, table(data$info06, data$next_vac_100))
temp <- rbind(temp, table(data$info07, data$next_vac_100))
temp <- rbind(temp, table(data$info08, data$next_vac_100))
temp <- rbind(temp, table(data$info09, data$next_vac_100))
temp <- rbind(temp, table(data$info10, data$next_vac_100))
temp <- rbind(temp, table(data$info14, data$next_vac_100))
temp <- rbind(temp, table(data$info15, data$next_vac_100))
temp <- rbind(temp, table(data$info16, data$next_vac_100))
temp <- rbind(temp, table(data$info17, data$next_vac_100))
temp <- rbind(temp, table(data$info18, data$next_vac_100))
temp <- rbind(temp, table(data$info20, data$next_vac_100))

# Copy to clipboard
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# regression
model01 <- vglm(next_vac_100 ~ info01, family = multinomial(refLevel = "12"), data = data)
model02 <- vglm(next_vac_100 ~ info02, family = multinomial(refLevel = "12"), data = data)
model03 <- vglm(next_vac_100 ~ info03, family = multinomial(refLevel = "12"), data = data)
model04 <- vglm(next_vac_100 ~ info04, family = multinomial(refLevel = "12"), data = data)
model05 <- vglm(next_vac_100 ~ info05, family = multinomial(refLevel = "12"), data = data)
model06 <- vglm(next_vac_100 ~ info06, family = multinomial(refLevel = "12"), data = data)
model07 <- vglm(next_vac_100 ~ info07, family = multinomial(refLevel = "12"), data = data)
model08 <- vglm(next_vac_100 ~ info08, family = multinomial(refLevel = "12"), data = data)
model09 <- vglm(next_vac_100 ~ info09, family = multinomial(refLevel = "12"), data = data)
model10 <- vglm(next_vac_100 ~ info10, family = multinomial(refLevel = "12"), data = data)
model14 <- vglm(next_vac_100 ~ info14, family = multinomial(refLevel = "12"), data = data)
model15 <- vglm(next_vac_100 ~ info15, family = multinomial(refLevel = "12"), data = data)
model16 <- vglm(next_vac_100 ~ info16, family = multinomial(refLevel = "12"), data = data)
model17 <- vglm(next_vac_100 ~ info17, family = multinomial(refLevel = "12"), data = data)
model18 <- vglm(next_vac_100 ~ info18, family = multinomial(refLevel = "12"), data = data)
model20 <- vglm(next_vac_100 ~ info20, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(model01))
temp2 <- confint(model01) |> exp()
temp3 <- summary(model01)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out01 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model02))
temp2 <- confint(model02) |> exp()
temp3 <- summary(model02)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out02 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model03))
temp2 <- confint(model03) |> exp()
temp3 <- summary(model03)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out03 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model04))
temp2 <- confint(model04) |> exp()
temp3 <- summary(model04)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out04 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model05))
temp2 <- confint(model05) |> exp()
temp3 <- summary(model05)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out05 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model06))
temp2 <- confint(model06) |> exp()
temp3 <- summary(model06)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out06 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model07))
temp2 <- confint(model07) |> exp()
temp3 <- summary(model07)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out07 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model08))
temp2 <- confint(model08) |> exp()
temp3 <- summary(model08)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out08 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model09))
temp2 <- confint(model09) |> exp()
temp3 <- summary(model09)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out09 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model10))
temp2 <- confint(model10) |> exp()
temp3 <- summary(model10)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out10 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model14))
temp2 <- confint(model14) |> exp()
temp3 <- summary(model14)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out14 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model15))
temp2 <- confint(model15) |> exp()
temp3 <- summary(model15)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out15 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model16))
temp2 <- confint(model16) |> exp()
temp3 <- summary(model16)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out16 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model17))
temp2 <- confint(model17) |> exp()
temp3 <- summary(model17)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out17 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model18))
temp2 <- confint(model18) |> exp()
temp3 <- summary(model18)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out18 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
temp1 <- exp(coef(model20))
temp2 <- confint(model20) |> exp()
temp3 <- summary(model20)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out20 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)

out <- rbind(out01, out02, out03, out04, out05, out06, out07, out08, out09, out10, out14, out15, out16, out17, out18, out20)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

model01 <- vglm(next_vac_100 ~ info01 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model02 <- vglm(next_vac_100 ~ info02 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model03 <- vglm(next_vac_100 ~ info03 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model04 <- vglm(next_vac_100 ~ info04 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model05 <- vglm(next_vac_100 ~ info05 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model06 <- vglm(next_vac_100 ~ info06 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model07 <- vglm(next_vac_100 ~ info07 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model08 <- vglm(next_vac_100 ~ info08 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model09 <- vglm(next_vac_100 ~ info09 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model10 <- vglm(next_vac_100 ~ info10 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model14 <- vglm(next_vac_100 ~ info14 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model15 <- vglm(next_vac_100 ~ info15 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model16 <- vglm(next_vac_100 ~ info16 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model17 <- vglm(next_vac_100 ~ info17 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model18 <- vglm(next_vac_100 ~ info18 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)
model20 <- vglm(next_vac_100 ~ info20 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(model01))
temp2 <- confint(model01) |> exp()
temp3 <- summary(model01)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
temp
stopifnot(nrow(temp) %% 4 == 0)
out01 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out01 <- out01[1:2,]
temp1 <- exp(coef(model02))
temp2 <- confint(model02) |> exp()
temp3 <- summary(model02)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out02 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out02 <- out02[1:2,]
temp1 <- exp(coef(model03))
temp2 <- confint(model03) |> exp()
temp3 <- summary(model03)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out03 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out03 <- out03[1:2,]
temp1 <- exp(coef(model04))
temp2 <- confint(model04) |> exp()
temp3 <- summary(model04)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out04 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out04 <- out04[1:2,]
temp1 <- exp(coef(model05))
temp2 <- confint(model05) |> exp()
temp3 <- summary(model05)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out05 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out05 <- out05[1:2,]
temp1 <- exp(coef(model06))
temp2 <- confint(model06) |> exp()
temp3 <- summary(model06)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out06 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out06 <- out06[1:2,]
temp1 <- exp(coef(model07))
temp2 <- confint(model07) |> exp()
temp3 <- summary(model07)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out07 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out07 <- out07[1:2,]
temp1 <- exp(coef(model08))
temp2 <- confint(model08) |> exp()
temp3 <- summary(model08)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out08 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out08 <- out08[1:2,]
temp1 <- exp(coef(model09))
temp2 <- confint(model09) |> exp()
temp3 <- summary(model09)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out09 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out09 <- out09[1:2,]
temp1 <- exp(coef(model10))
temp2 <- confint(model10) |> exp()
temp3 <- summary(model10)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out10 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out10 <- out10[1:2,]
temp1 <- exp(coef(model14))
temp2 <- confint(model14) |> exp()
temp3 <- summary(model14)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out14 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out14 <- out14[1:2,]
temp1 <- exp(coef(model15))
temp2 <- confint(model15) |> exp()
temp3 <- summary(model15)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out15 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out15 <- out15[1:2,]
temp1 <- exp(coef(model16))
temp2 <- confint(model16) |> exp()
temp3 <- summary(model16)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out16 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out16 <- out16[1:2,]
temp1 <- exp(coef(model17))
temp2 <- confint(model17) |> exp()
temp3 <- summary(model17)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out17 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out17 <- out17[1:2,]
temp1 <- exp(coef(model18))
temp2 <- confint(model18) |> exp()
temp3 <- summary(model18)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out18 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out18 <- out18[1:2,]
temp1 <- exp(coef(model20))
temp2 <- confint(model20) |> exp()
temp3 <- summary(model20)@coef3[,4]
temp <- cbind(temp1, temp2, temp3)
stopifnot(nrow(temp) %% 4 == 0)
out20 <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out20 <- out20[1:2,]

out <- rbind(out01, out02, out03, out04, out05, out06, out07, out08, out09, out10, out14, out15, out16, out17, out18, out20)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Past inf
temp <- table(data$inf_2021_all, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ inf_2021_all, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ inf_2021_all + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Adv1
temp <- table(data$adv1, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ adv1, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ adv1 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### COVID fear
temp <- table(data$koronafuan, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ koronafuan, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ koronafuan + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### Vac misinfo
temp <- table(data$vac_inbou, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ vac_inbou, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ vac_inbou + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### COVID conspi
temp <- table(data$korona_inbou, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ korona_inbou, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ korona_inbou + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### ID knowledge
temp <- table(data$chishiki, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ chishiki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ chishiki + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

### COVID vac
# data <- data %>%
#   mutate(korona_vac2 = case_when(
#     Q39 <= 2 ~ "1",
#     Q39 >= 4 ~ "0",
#     TRUE ~ NA))

temp <- table(data$korona_vac2, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ korona_vac2, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ korona_vac2 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

# ### Next pan when4
# temp <- table(data$next_pan, data$next_vac_never)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # Level
# data$next_pan <- factor(data$next_pan, levels = c("4", "1", "5", "6", "7"))
# 
# 
# # Binomial logistic regression
# model <- glm(next_vac_never ~ next_pan, data = data, family = binomial)
# 
# # OR, 95CI, p-value
# temp1 <- exp(coef(model))
# temp2 <- exp(confint(model))
# temp3 <- summary(model)$coefficients[, 4]
# 
# temp <- cbind(temp1, temp2, temp3)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# 
# 
# # Adjusted Binomial logistic regression
# model <- glm(next_vac_never ~ next_pan + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
# 
# # OR, 95CI, p-value
# temp1 <- exp(coef(model))
# temp2 <- exp(confint(model))
# temp3 <- summary(model)$coefficients[, 4]
# 
# temp <- cbind(temp1, temp2, temp3)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Next pan when3
temp <- table(data$next_pan_out3, data$next_vac_100)
temp
write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

# Level
data$next_pan_out3 <- factor(data$next_pan_out3, levels = c("45", "123", "67"))

# multinomial logistic regression
fit_vg <- vglm(next_vac_100 ~ next_pan_out3, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

###### Adjustment for demographic ######

fit_vg <- vglm(next_vac_100 ~ next_pan_out3 + nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

# ### Info No
# temp <- table(data$infoNo, data$next_vac_never)
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # Binomial logistic regression
# model <- glm(next_vac_never ~ infoNo, data = data, family = binomial)
# 
# # OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # temp <- exp(confint(model))
# # temp
# # write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# temp <- summary(model)$coefficients[, 4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # Adjusted Binomial logistic regression
# model <- glm(next_vac_never ~ infoNo + nenrei + SEX + nenshu + gakureki, data = data, family = binomial)
# 
# # Adjusted OR, 95CI, p-value
# temp <- exp(coef(model))
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# # temp <- exp(confint(model))
# # temp
# # write.table(temp, "clipboard", sep = "\t", row.names = FALSE)
# 
# temp <- summary(model)$coefficients[, 4]
# temp
# write.table(temp, "clipboard", sep = "\t", row.names = FALSE)

### Info all
fit_vg <- vglm(next_vac_100 ~ info01 + info02 + info03 + info04 + info05 + info06 + info07 + info08 + info09 + info10 + info14 + info15 + info16 + info17 + info18 + info20 +
                 nenrei + SEX + nenshu + gakureki, family = multinomial(refLevel = "12"), data = data)

# OR
temp1 <- exp(coef(fit_vg))
temp2 <- confint(fit_vg) |> exp()
temp3 <- summary(fit_vg)@coef3[,4]

temp <- cbind(temp1, temp2, temp3)
temp
write.table(temp, "clipboard", sep = "\t", row.names = TRUE)

stopifnot(nrow(temp) %% 4 == 0)
out <- as.data.frame(
  matrix(
    as.vector(t(as.matrix(temp))),
    ncol = 16,
    byrow = TRUE
  )
)
out
write.table(out, "clipboard", sep = "\t", row.names = TRUE)

