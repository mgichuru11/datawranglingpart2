# load packages -----------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)


# # download files ----------------------------------------------------------
# download.file(url = "https://github.com/rfortherestofus/going-deeper/raw/master/data-raw/enrollment-18-19.xlsx",destfile = "data-raw/enrollment-18-19.xlsx",mode = "wb")
# 
# download.file(url = "https://github.com/rfortherestofus/going-deeper/raw/master/data-raw/enrollment-17-18.xlsx",destfile = "data-raw/enrollment-17-18.xlsx", mode = "wb")
# 
# Merging data ------------------------------------------------------------
# download.file(url ="https://github.com/rfortherestofus/going-deeper/raw/master/data-raw/oregon-districts.xlsx",
#               destfile = "data-raw/oregon-districts.xlsx", mode="wb")


# import files ------------------------------------------------------------
enrollment_17_18 <- read_excel(path ="data-raw/enrollment-17-18.xlsx",sheet = "Sheet 1")

enrollment_18_19 <-read_excel(path = "data-raw/enrollment-18-19.xlsx", sheet = "Sheet 1")

# import data
oregon_districts<- read_excel("data-raw/oregon-districts.xlsx") %>%
  clean_names()


# # clean data --------------------------------------------------------------
# enrollment_18_19 <- enrollment_18_19%>%
#   select(!contains("x2018_19_grade_"))%>%
#   select(!contains("kindergarten"))%>%
#   select(!contains("percent"))%>%
#   # mutate('x2018_19_american_indian_alaska_native':'x2018_19_grade_twelve',
#   # ~parse_number(.))%>%
#   pivot_longer(cols = -district_id,
#                names_to = "race_ethnicity", 
#                values_to = "number_of_students")%>%
#   mutate(number_of_students= na_if(number_of_students, "-"))%>%
#   mutate(number_of_students = as.numeric(number_of_students))%>%
#   mutate(number_of_students= replace_na(number_of_students, 0))
# #summarise(total = sum(number_of_students))
# 
# 
# # # Advanced variable creation --------------------------------------------
# enrollment_18_19 <-enrollment_18_19 %>%
#   mutate(race_ethnicity = str_remove(race_ethnicity,"x2018_19_"))%>%
#   mutate(race_ethnicity = case_when(
#     race_ethnicity == "american_indian_alaska_native" ~ "American Indian/ Alaska Natiev", 
#     race_ethnicity == "black_african_american" ~ "Black African American", 
#     race_ethnicity == "native_hawaiian_pacific_islander" ~ "Native Hawaiian Pacific Islander",
#     race_ethnicity == "hispanic_latino" ~ "Hispanic Latino",
#     race_ethnicity == "asian" ~ "Asian",
#     race_ethnicity == "multiracial" ~ "Multiracial",
#     race_ethnicity == "white" ~ "White",
#   ))
# 
# 
# # Advanced summarizing ----------------------------------------------------
# enrollment_18_19<-enrollment_18_19 %>%
#   group_by(district_id)%>%
#   mutate(pct = number_of_students/sum(number_of_students))%>%
#   ungroup()%>%
#   drop_na()%>%
#   mutate(year = "2018-2019")
# 
# 
# 
# # PART 2 ------------------------------------------------------------------
# # clean data --------------------------------------------------------------
# enrollment_17_18 <- enrollment_17_18%>%
#   select(!contains("x2017_18_grade_"))%>%
#   select(!contains("kindergarten"))%>%
#   select(!contains("percent"))%>%
#   # mutate('x2018_19_american_indian_alaska_native':'x2018_19_grade_twelve',
#   # ~parse_number(.))%>%
#   pivot_longer(cols = -district_id,
#                names_to = "race_ethnicity", 
#                values_to = "number_of_students")%>%
#   mutate(number_of_students= na_if(number_of_students, "-"))%>%
#   mutate(number_of_students = as.numeric(number_of_students))%>%
#   mutate(number_of_students= replace_na(number_of_students, 0))
# #summarise(total = sum(number_of_students))
# 
# 
# # # Advanced variable creation --------------------------------------------
# enrollment_17_18 <-enrollment_17_18 %>%
#   mutate(race_ethnicity = str_remove(race_ethnicity,"x2017_18_"))%>%
#   mutate(race_ethnicity = case_when(
#     race_ethnicity == "american_indian_alaska_native" ~ "American Indian/ Alaska Natiev", 
#     race_ethnicity == "black_african_american" ~ "Black African American", 
#     race_ethnicity == "native_hawaiian_pacific_islander" ~ "Native Hawaiian Pacific Islander",
#     race_ethnicity == "hispanic_latino" ~ "Hispanic Latino",
#     race_ethnicity == "asian" ~ "Asian",
#     race_ethnicity == "multiracial" ~ "Multiracial",
#     race_ethnicity == "white" ~ "White",
#   ))
# 
# 
# # Advanced summarizing ----------------------------------------------------
# enrollment_17_18<-enrollment_17_18 %>%
#   group_by(district_id)%>%
#   mutate(pct = number_of_students/sum(number_of_students))%>%
#   ungroup()%>%
#   drop_na()%>%
#   mutate(year="2017-2018")
# 
# 
# 
# 
# # Binding data frames -----------------------------------------------------
# enrollment_by_race_ethnicity <- bind_rows(enrollment_17_18,enrollment_18_19)
# 

#"x2018_19_grade_"
#"x2018_19_"

# creating a function -----------------------------------------------------
clean_race_data <- function(raw_data,contains_x,string_x,data_year){
  raw_data <- raw_data%>%
  select(!contains(contains_x))%>%
  select(!contains("kindergarten"))%>%
  select(!contains("percent"))%>%
  # mutate('x2018_19_american_indian_alaska_native':'x2018_19_grade_twelve',
  # ~parse_number(.))%>%
  pivot_longer(cols = -district_id,
               names_to = "race_ethnicity", 
               values_to = "number_of_students")%>%
  mutate(number_of_students= na_if(number_of_students, "-"))%>%
  mutate(number_of_students = as.numeric(number_of_students))%>%
  mutate(number_of_students= replace_na(number_of_students, 0))
#summarise(total = sum(number_of_students))


# # Advanced variable creation --------------------------------------------
raw_data <-raw_data %>%
  mutate(race_ethnicity = str_remove(race_ethnicity,string_x))%>%
  mutate(race_ethnicity = case_when(
    race_ethnicity == "american_indian_alaska_native" ~ "American Indian/ Alaska Natiev", 
    race_ethnicity == "black_african_american" ~ "Black African American", 
    race_ethnicity == "native_hawaiian_pacific_islander" ~ "Native Hawaiian Pacific Islander",
    race_ethnicity == "hispanic_latino" ~ "Hispanic Latino",
    race_ethnicity == "asian" ~ "Asian",
    race_ethnicity == "multiracial" ~ "Multiracial",
    race_ethnicity == "white" ~ "White",
  ))


# Advanced summarizing ----------------------------------------------------
raw_data<-raw_data %>%
  group_by(district_id)%>%
  mutate(pct = number_of_students/sum(number_of_students))%>%
  ungroup()%>%
  drop_na()%>%
  mutate(year = data_year)

}

# clean1819 data 
enrollment_18_19<-clean_race_data(raw_data=enrollment_18_19,contains_x="x2018_19_grade_",string_x="x2018_19_",data_year="2018-2019")

# clean1718 data 
enrollment_17_18 <-clean_race_data(raw_data=enrollment_17_18, contains_x="x2017_18_grade_",string_x ="x2017_18_" ,data_year ="2017-2018")

# bind rows
enrollment_by_race_ethnicity <- bind_rows(enrollment_17_18,enrollment_18_19)%>%
  left_join(oregon_districts, 
            by=c("district_id" = "attending_district_institutional_id")) %>%
  set_names("district_id", "ethnicity_race", "number_of_students", "proportion", "year", "district_name")


# save data 
# write_rds(enrollment_by_race_ethnicity, 
#           file = "data/enrollment-by-race-ethnicity.rds")
#   



