library(tidyverse)
library(readxl)

reserve_nek <- bind_rows(
    read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/TimeEdit_1001NE_Nationalekonomi_A_2017-10-12_19_11.xls", 
        col_types = c("text", "text", "text", 
            "text", "text", "text", "text", "text", 
            "skip", "text", "text", "skip", 
            "text", "text"), skip = 5),
    
    reserve_nek <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/TimeEdit_1073NE_Avancerad_nationalekonomi_1077NE_Avancerad_nationalekonomi__18_Kurs_20_2017-10-12_19_08.xls", 
        col_types = c("text", "text", "text", 
            "text", "text", "text", "text", "text", 
            "skip", "text", "text", "skip", 
            "text", "text"), skip = 5)
) %>% 
    filter(!is.na(Vecka))

saveRDS(reserve_nek, file = "~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/reservations.rds")

# Set libPaths.
.libPaths("/Users/jonasbjornerstedt/.exploratory/R/3.4")

# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(exploratory)

# Steps to produce reservera_A
`reservera_A` <- exploratory::select_columns(exploratory::clean_data_frame(exploratory::read_excel_file( "/Users/jonasbjornerstedt/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/TimeEdit_1001NE_Nationalekonomi_A_2017-10-12_19_11.xls", sheet = "TimeEdit", na="NA", skip=5, col_names=TRUE, trim_ws=FALSE)),"Vecka","Veckodag","Startdatum","Starttid","Slutdatum","Sluttid","Lokal","Undervisningstyp","Kurs","Delkurs","Person") %>% exploratory::clean_data_frame()

# Steps to produce the output
exploratory::select_columns(exploratory::clean_data_frame(exploratory::read_excel_file( "/Users/jonasbjornerstedt/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/TimeEdit_1073NE_Avancerad_nationalekonomi_1077NE_Avancerad_nationalekonomi__18_Kurs_20_2017-10-12_19_08.xls", sheet = "TimeEdit", na="NA", skip=5, col_names=TRUE, trim_ws=TRUE)),"Vecka","Veckodag","Startdatum","Starttid","Slutdatum","Sluttid","Lokal","Undervisningstyp","Kurs","Delkurs","Person","Anmärkning") %>% exploratory::clean_data_frame() %>%
    
    # Add A course
    bind_rows(reservera_A) %>%
    filter(!is.na(Vecka)) %>%
    mutate(Startdatum = ymd(Startdatum), Slutdatum = ymd(Slutdatum), Starttid = parse_time(Starttid), Sluttid = parse_time(Sluttid), Timmar = (Sluttid - Starttid) / 3600) %>%
    separate(Vecka, into = c("Vecka", "Veckonr"), sep = "\\s+", remove = TRUE, convert = TRUE) %>%
    mutate(Period = if_else(Veckonr < 25,1,2), `År` = year(Startdatum)) %>%
    select(-Slutdatum, -Vecka, -Anmärkning, -Veckonr) %>%
    separate(Kurs, into = c("Kurs"), sep = "\\s*\\,\\s*", remove = TRUE, convert = TRUE, extra = "drop") %>%
    separate(Delkurs, into = c("Delkurs"), sep = "\\s*\\,\\s*", remove = TRUE, extra = "drop") %>%
    mutate(Delkurs = str_replace(Delkurs, "Valbar kurs: ", "")) %>%
    mutate(Timmar = as.numeric(Timmar)) %>%
    group_by(År, Period, Person, Delkurs) %>%
    summarize(Timmar = sum(Timmar, na.rm = TRUE)) %>%
    arrange(År, Period, Delkurs, Person)

