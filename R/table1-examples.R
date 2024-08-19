library(tidyverse)
library(gtsummary)

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



# In Class Practice

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


