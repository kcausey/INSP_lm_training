## EMPTY THE ENVIRONMENT
rm(list = ls())

## LOAD PACKAGES
#install.packages("data.table")
library(data.table)
library(ggplot2)

## LOAD DATA USING DATA.TABLE
data <- fread("J:/temp/jledes2/insp/data/mexico.csv")

## CONVERT DATA.FRAME INTO DATA.TABLE
data <- read.csv("J:/temp/jledes2/insp/data/mexico.csv")
data <- data.table(data)

## SUBSET TO MEXICO CITY AND DIABETES
city <- data[location_name == "Mexico City" & cause_name == "Diabetes mellitus" & year_id > 2010]
city

## DATA.FRAME WAY
city <- data[data$location_name == "Mexico City" & data$cause_name == "Diabetes mellitus" & data$year_id > 2010, ]
city

## BEFORE ARRANGING ROWS
data

## USING THE ORDER FUNCTION
data <- data[order(location_name)]
data

## AND REVERSE ORDER
data <- data[order(-location_name)]
data

## AND ORDER MULTIPLE ROWS
data <- data[order(location_name, cause_name)]
data

## CREATE A NEW COLUMN - RATE PER 100,000
data[, rate := val*100000]
data

## THE DATA FRAME WAY
data$rate <- data$val*100000

## CREATE FEMALE DUMMY COLUMN
data[sex == "Male", female := 0]
data[sex == "Female", female := 1]

## REMOVE COLUMNS
data[, female := NULL]

## SUBSET COLUMNS
new <- data[, .(cause_name, location_name, year_id, sex, val)]
new

## DATA FRAME WAY
new <- data[, c("cause_name", "location_name", "year_id", "sex", "val")]
new

## OTHER USEFUL FUNCTIONS
unique(data[, location_name])

## RENAME COLUMNS
setnames(data, old = "age_group_name", new = "age")
setnames(data, old = c("year_id", "location_name"), new = c("year", "loc"))

## %IN% AS USEFUL ALTERNATIVE TO OR STATMENT
data[cause_name %in% c("Stroke", "HIV/AIDS")]


## TESTING
## LOAD DATA USING DATA.TABLE
data <- fread("J:/temp/jledes2/insp/data/mexico.csv")
cov  <- fread("J:/temp/jledes2/insp/data/mean_BMI.csv")

## CLEAN
input <- data[cause_name == "Diabetes mellitus" & sex == "Male"]
input <- input[, .(location_name, location_id, year_id, sex, age_group_name, val)]
cov   <- cov[, .(location_id, year_id, age_group_name, sex, mean_value)]

## MERGE
input <- merge(input, cov, all.y=T)
input <- input[sex == "Male"]
input[, diabetes_mort := val*100000]
setnames(input, "mean_value", "bmi")

## RUN REGRESSION IN LOG SPACE
bmi_mod <- lm(data = input, formula = log(diabetes_mort)~bmi)
summary(bmi_mod)

## PREDICTIONS
input[, pred := predict(bmi_mod, input)]
input[location_id == 4649 & year_id >= 2010]

## PREDICTIONS WITH UNCERTAINTY
input[, c("pred", "lower", "upper") := data.table(predict(bmi_mod, input, interval = "prediction"))]
input[location_id == 4649 & year_id >= 2010]

## HISTOGRAMS
ggplot(data=input, aes(x=log(diabetes_mort))) +
  geom_histogram(color="darkblue", fill="lightblue") +
  labs(x="All-age Diabetes mortality rate per 100,000 population", y="Frequency") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20, face = "bold"))

## RUN REGRESSION IN LOG SPACE
bmi_mod <- lm(data = input, formula = log(diabetes_mort)~bmi)
summary(bmi_mod)

## PREDICTION FROM LOG MODEL
input[, pred := exp(predict(bmi_mod, input))]
input[location_id == 4649 & year_id >= 2010]

## PLOT
ggplot(data=input, aes(x=bmi, y=diabetes_mort)) +
  geom_point(size=3, alpha=0.5) +
  geom_line(aes(x=bmi, y=pred), size=1.5, color="chartreuse4") +
  #(method='lm', formula= y~x, se = F, size=1.5, color="chartreuse4") +
  theme_bw() +
  labs(x="Mean BMI", y="All-age Diabetes mortality rate per 100,000 population") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20, face = "bold"))

## DUMMY REGRESSION
## LOAD DATA USING DATA.TABLE
input <- data[cause_name == "Diabetes mellitus"]
input <- input[, .(location_name, location_id, year_id, sex, age_group_name, val)]
input[, diabetes_mort := val*100000]

## RUN REGRESSION WITH DUMMY VARIABLE
dummy_reg <- lm(data = input, formula = diabetes_mort~factor(sex))
summary(dummy_reg)

## MULTIPLE REGRESSION IN R
data  <- fread("J:/temp/jledes2/insp/mexico.csv")
bmi   <- fread("J:/temp/jledes2/insp/data/mean_BMI.csv")
chole <- fread("J:/temp/jledes2/insp/data/mean_cholesterol.csv")

##
input <- data[cause_name == "Diabetes mellitus"]
input <- input[, .(location_name, location_id, year_id, sex, age_group_name, val)]
bmi   <- bmi[, .(location_id, year_id, age_group_name, sex, mean_value)]
chole <- chole[, .(location_id, year_id, age_group_name, sex, mean_value)]

##
input[, diabetes_mort := val*100000]
setnames(bmi, "mean_value", "bmi")
setnames(chole, "mean_value", "cholesterol")

##
input <- merge(input, bmi, all.y=T)
input <- merge(input, chole, all.y=T)

## RUN MULTIPLE REGRESSION
model_1 <- lm(data=input, formula = log(diabetes_mort) ~ bmi + cholesterol + factor(sex))
summary(model_1)

## SDI
data  <- fread("J:/temp/jledes2/insp/data/mexico.csv")
sdi   <- fread("J:/temp/jledes2/insp/data/sdi.csv")

##
input <- data[cause_name == "Tuberculosis" & year_id == 2005]
input <- input[, .(location_name, location_id, year_id, sex, age_group_name, val)]
sdi   <- sdi[, .(location_id, year_id, age_group_name, mean_value)]

##
input[, tb := val*100000]
setnames(sdi, "mean_value", "sdi")

##
input <- merge(input, sdi)

## install package
#install.packages("ggplot2")
#library(ggplot2)


## PLOT
ggplot(data=input, aes(x=sdi, y=tb)) +
  geom_point(color="red", alpha=0.5, size=3.5)


## PLOT with smoother
ggplot(data=input, aes(x=sdi, y=tb)) +
  geom_point(color="red", alpha=0.5, size=3.5) +
  stat_smooth()


## PLOT with linear regression
ggplot(data=input, aes(x=sdi, y=tb)) +
  geom_point(color="red", alpha=0.5, size=3.5) +
  stat_smooth(method ="lm", alpha = 0.5, colour = 'black', size = .5)


## PLOT with log-transformed linear regression
ggplot(data=input, aes(x=sdi, y=tb)) +
  geom_point(color="red", alpha=0.5, size=3.5) +
  stat_smooth(method = 'lm', formula = y ~ log(x),
              alpha = 0.5, colour = 'black', size = .5)
