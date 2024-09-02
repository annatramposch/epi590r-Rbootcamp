# R Bootcamp Day 1 Practice Coding
# Programmer: Anna Tramposch
# Contact: anna.tramposch@emory.edu
# Github: annatramposch, annatramposch@gmail.com
# Date Created: Mon 8/19/2014
# Last Update: Tues 8/20/2014

--------------------------------------------------------------------------------------------------------------

# RTHEMES


library(rsthemes)

# Set default themes

set_theme_light ("Flat White {rsthemes}")
set_theme_dark("One Dark {rsthemes}")
	"One Dark {rsthemes}"
	"base16 Chalk {rsthemes}"
	"Material Darker {rsthemes}"
	"Serendipity Dark {rsthemes}"
	"Elm dark {rsthemes}"
	"Night Owl Dark {rsthemes}"

set_theme_favorite

# Toggle themes on and off, or put on an auto schedule
use_theme_light(quietly = FALSE)
use_theme_dark(quietly = FALSE)
use_theme_toggle()
use_theme_auto(
	dark_start = "19:00",
	dark_end = "8:00",
	lat = NULL,
	lon = NULL,
	quietly = FALSE
)

# Apply a theme or scroll through the installed themes
try_rsthemes("Serendipity Dark {rsthemes}")
try_rsthemes(style = "all", include_base16 = TRUE, delay = 0)
list_rsthemes(style = "all", include_base16 = TRUE, list_installed = TRUE)


--------------------------------------------------------------------------------------------------------------

# DAY ONE: What we covered in class


# TABLE ONE EXAMPLES
--------------------

library(tidyverse)
library(here)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")


# CLASS EXCERCISE

# Make a tbl_summary()
# Include categorical region, race/ethnicity, income, and the sleep variables (use a helper function to select those)
# Make sure they are nicely labeled.
# Stratify the table by sex. Add a p-value comparing the sexes and an overall column combining both sexes.
# For the income variable, show the 10th and 90th percentiles of income with 3 digits,
# For the sleep variables, show the min and the max with 1 digit.
# Add a footnote to the race/ethnicity variable with a link to the page describing how NLSY classified participants: https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c("race_eth_cat", "region_cat", "income", starts_with("sleep")), # Use helper functions to select variables
	label = list( # Label variables in table
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		income ~ "Income (10%, 90%)",
		sleep_wkdy ~ "Sleep, Weekday",
		sleep_wknd ~ "Sleep, Wkend"
	),
	missing_text = "Missing",
	statistic = list(
		income = "{mean} ({p10}, {p90})",
		sleep_wkdy ~ "min = {min}; max = {max}",
		sleep_wknd ~ "min = {min}; max = {max}"),
	digits = list(
		income ~ c(3, 3),
		sleep_wkdy ~ c(1, 1),
		sleep_wknd ~ c(1, 1)
	)) |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_table_styling(
		columns = label,
		rows = label == "Race/ethnicity",
		footnote = "How NLSY classified participants: https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data") |>
	modify_header(label = "**Variable**", p.value = "**P**")




# REGRESSION EXAMPLES
---------------------

library(tidyverse)
library(gtsummary)
install.packages("broom.helpers")
library(broom.helpers)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Univariate regression

tbl_uvregression(
	nlsy,
	y = income,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, income, age_bir),
	method = lm)

# The code above is really doing
lm(income ~ sex_cat, data = nlsy)
lm(income ~ race_eth_cat, data = nlsy)
lm(income ~ eyesight_cat, data = nlsy)
lm(income ~ income, data = nlsy)
lm(income ~ age_bir, data = nlsy)

# Logistic Regression

tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE)

## Class Practice - Create a univariate regression table looking at the association between sex (sex_cat)
# as the x = variable and each of nsibs, sleep_wkdy, and sleep_wknd, and income.

tbl_uvregression(
	nlsy,
	x = sex_cat,
	include = c(nsibs, sleep_wkdy, sleep_wknd, income),
	method = lm)

# Which is really like doing
lm(income ~ sex_cat, data = nlsy)
lm(nsibs ~ sex_cat, data = nlsy)
lm(sleep_wkdy ~ sex_cat, data = nlsy)
lm(sleep_wknd ~ sex_cat, data = nlsy)

## Multivariable regressions

## Some regressions

linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
									 data = nlsy)


linear_model_int <- lm(income ~ sex_cat*age_bir + race_eth_cat,
											 data = nlsy)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())

# Fit a Poisson regression (family = poisson()) for the number of siblings, using at least 3 predictors of your choice.
# Create a nice table displaying your Poisson regression and its exponentiated coefficients.

poisson_model <- glm(nsibs ~ sex_cat + race_eth_cat + age_bir,
										 data = nlsy, family = poisson())

tbl_regression(
	poisson_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


## Tables

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth",
		`sex_cat:age_bir` ~ "Sex/age interaction"
	))

## Table comparing the models with and without interaction

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**Model 1**", "**Model 2**"))




--------------------------------------------------------------------------------------------------------------
# # DAY TWO: What we covered in class

# QUARTO
--------

# can make slides, documents & presentations based on R script with code & text interspersed
# Quarto (newer) vs R Markdown (older)
# render vs knit


# FUNCTIONS
-----------
	# start out with a number to test
	x <- 3

	# name your function <- and write your function equation
	square <- function(x) {
	x^2
	}

	# test it out
	square(x)
	square(53)
	53^2 # does this match? yes.


	# Another Example - New Mean

	new_mean <- function(x) {
		n <- length(x)
		mean_val <- sum(x) / n
		return(mean_val)
	}

	# test it out
	b <- c(1,2,3,4,5)
	new_mean(b)


	# Another Exmaple - Raise

	x <- 3
	y <- 4
	x^y # this is the equation

	raise <- function(base, power = 2) {
	answer <- base ^ power
	return(answer)
	}

	# test with
	raise(base = 2, power = 4)
	# should give you
	2^4
	# if I don't specify power, will it default to 2?
	raise(base = 5)



