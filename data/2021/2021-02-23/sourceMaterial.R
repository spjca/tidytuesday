#basic url
# https://www.bls.gov/cps/aa2020/cpsaat17.xlsx

library(tidyverse)
library(glue)


get_bls_report <- function(year){
  
  report_url <- glue::glue("https://www.bls.gov/cps/aa{year}/cpsaat17.xlsx")
  
  download.file(report_url, destfile = glue("2021/2021-02-23/bls-{year}.xlsx"))
}

ex_2019 <- readxl::read_excel("2021/2021-02-23/bls-2019.xlsx")

2015:2019 %>% 
  walk(get_bls_report)

# 2020 has no year in front of it
download.file(
  "https://www.bls.gov/cps/cpsaat17.xlsx", 
  destfile = "2021/2021-02-23/bls-2020.xlsx"
)

# Raw BLS -----------------------------------------------------------------

raw_2020 <- readxl::read_excel("2021/2021-02-23/bls-2020.xlsx", skip = 3) %>% 
  slice(1:(n()-2))

major_grp <- raw_2020 %>% 
  slice(1) %>% 
  select(3:last_col()) %>% 
  set_names(nm = glue::glue("...{1:ncol(.)}")) %>% 
  pivot_longer(cols = everything(), values_to = "major_grp") 

minor_grp <- raw_2020 %>% 
  slice(2) %>% 
  select(3:last_col()) %>% 
  set_names(nm = glue::glue("...{1:ncol(.)}")) %>% 
  pivot_longer(cols = everything(), values_to = "minor_grp") 

combo_grp <- left_join(major_grp, minor_grp, by = "name") %>% 
  mutate(across(.fns = ~str_replace_all(.x, "\n", " "))) %>% 
  mutate(across(.fns = ~str_remove_all(.x, "\r"))) %>% 
  mutate(across(.fns = ~str_replace_all(.x, "- ", ""))) %>% 
  tidyr::fill(major_grp)

name_fill <- c("race_gender", "category", "total", glue("...{1:11}"))

clean_2020 <- raw_2020 %>% 
  rename(category = 1) %>% 
  mutate(
    race_gender = if_else(
      str_detect(category, "Agriculture and related"),
      lag(category),
      NA_character_
    ),
    .before = category
  ) %>% 
  fill(race_gender) %>% 
  slice(5:n()) %>% 
  set_names(nm = name_fill) %>% 
  pivot_longer(cols = contains("..."), names_to = "name", values_to = "employ_n") %>% 
  left_join(combo_grp, by = "name") %>% 
  mutate(year = 2020) %>% 
  select(category, major_grp, minor_grp, race_gender, cat_total = total, employ_n)

# Make a function!

clean_bls <- function(year){
  
  raw_df <- readxl::read_excel(glue("2021/2021-02-23/bls-{year}.xlsx"), skip = 3) %>% 
    slice(1:(n()-2))
  
  major_grp <- raw_df %>% 
    slice(1) %>% 
    select(3:last_col()) %>% 
    set_names(nm = glue::glue("...{1:ncol(.)}")) %>% 
    pivot_longer(cols = everything(), values_to = "major_grp") 
  
  minor_grp <- raw_df %>% 
    slice(2) %>% 
    select(3:last_col()) %>% 
    set_names(nm = glue::glue("...{1:ncol(.)}")) %>% 
    pivot_longer(cols = everything(), values_to = "minor_grp") 
  
  combo_grp <- left_join(major_grp, minor_grp, by = "name") %>% 
    mutate(across(.fns = ~str_replace_all(.x, "\n", " "))) %>% 
    mutate(across(.fns = ~str_remove_all(.x, "\r"))) %>% 
    mutate(across(.fns = ~str_replace_all(.x, "- ", ""))) %>% 
    tidyr::fill(major_grp)
  
  name_fill <- c("race_gender", "category", "total", glue("...{1:11}"))
  
  clean_df <- raw_df %>% 
    rename(category = 1) %>% 
    mutate(
      race_gender = if_else(
        str_detect(category, "Agriculture and related"),
        lag(category),
        NA_character_
      ),
      .before = category
    ) %>% 
    fill(race_gender) %>% 
    slice(5:n()) %>% 
    set_names(nm = name_fill) %>% 
    pivot_longer(cols = contains("..."), names_to = "name", values_to = "employ_n") %>% 
    left_join(combo_grp, by = "name") %>% 
    select(industry = category, major_occupation = major_grp, minor_occupation = minor_grp, race_gender, industry_total = total, employ_n) %>% 
    mutate(year = year)
  
  clean_df
  
}

# combine the data

all_bls <- 2015:2020 %>% 
  map_dfr(clean_bls) %>% 
  arrange(desc(year)) %>% 
  mutate(
    industry_total = as.integer(industry_total) * 1000,
    employ_n = as.integer(employ_n) * 1000
  )

# sanity check plot

all_bls %>% 
  filter(race_gender == "Black or African American") %>% 
  filter(minor_occupation == "Sales and related occupations") %>% 
  ggplot(aes(x = year, y = employ_n, group = industry)) +
  geom_line()

all_bls %>% 
  write_csv("2021/2021-02-23/employed.csv")

### Here I'm reading against the HTML file included

library(rvest)

raw_html <- read_html("2021/2021-02-23/bls-all.htm")

raw_html %>% 
  html_nodes("table.catalog") %>% 
  html_table()

all_catalog_raw <- raw_html %>% 
  html_nodes("table.catalog") %>% 
  html_table() 


all_catalog_clean <- all_catalog_raw %>% 
  map(clean_catalog)

clean_catalog <- function(table){
  
  table %>% 
    pivot_wider(names_from = X1, values_from = X2) %>% 
    janitor::clean_names()
  
}

clean_catalog(all_catalog_raw[[2]])


all_table_raw <- raw_html %>% 
  html_nodes("table.regular-data")%>% 
  html_table() 

all_table_clean <- all_table_raw %>% 
  map_dfr(clean_table, .id = "id") %>% 
  mutate(id = as.integer(id))

clean_table <- function(table){
  
  table %>% 
    filter(str_detect(Year, "Corrected", negate = TRUE)) %>% 
    mutate(across(everything(), ~str_remove(.x, "\\(C\\)"))) %>% 
    pivot_longer(Qtr1:Qtr4, names_to = "quarter", values_to = "value") %>% 
    rename(year = Year)
  
}

combine_tables <- bind_rows(all_table_clean) %>% 
  mutate(id = 1:n())

combined_data <- all_catalog_clean %>% 
  bind_rows() %>% 
  mutate(id = row_number()) %>% 
  left_join(all_table_clean, by = "id") %>% 
  mutate(
    year = as.integer(year), 
    quarter = str_remove(quarter, "Qtr") %>% as.integer(),
    value = as.integer(value)
  ) 

data_earn <- combined_data %>% 
  filter(earnings != "Person counts (number in thousands)") %>% 
  rename(median_weekly_earn = value) %>% 
  select(industry:last_col(), -id)

data_earn 

final_bls_earn <- combined_data %>%
  filter(earnings == "Person counts (number in thousands)") %>%
  rename(n_persons = value) %>%
  mutate(n_persons = n_persons * 1000) %>%
  select(industry:quarter, n_persons, -id) %>%
  left_join(
    data_earn,
    by = c(
      "industry", "occupation", "sex", "race", "ethnic_origin", "age", 
      "education", "class_of_worker", "labor_force_status", "year", "quarter"
    )
  ) %>% 
  select(sex, race, ethnic_origin, age, year:median_weekly_earn)

final_bls_earn %>% 
  write_csv("2021/2021-02-23/earn.csv")

# sanity check
final_bls_earn %>% 
  filter(quarter == 2, sex == "Both Sexes", race != "All Races") %>% 
  ggplot(aes(x = year, y = median_weekly_earn, color = race)) +
  geom_line() +
  facet_wrap(~age)
distinct(quarter)
