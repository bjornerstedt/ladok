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

saveRDS(reserve_nek, file = "~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/reervations.rds")