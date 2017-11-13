# cr_plats.R
# Skapa platstal för hela ISV

library(tidyverse)
library(readxl)
library(janitor)


cr_platstermin = function(år, termin) {
    filename = sprintf("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/platser-%s%d.xlsx", termin, år)
    print(filename)
    read_excel(filename, 
               col_names = c("inst", "prog_kurs", "anmkod", "kursid", "titel", "poäng", "ämnesgrupp", 
                             "sökande_prio1", "sökande_totalt", "antagning", "fristående", "platser_totalt", "godkänt"),
               col_types = c("numeric", "text", "numeric", 
                             "text", "text", "numeric", "text", 
                             "numeric", "numeric", "numeric", 
                             "numeric", "numeric", "text"), skip = 3) %>% 
        mutate(
            år = 2000 + år,
            termin = termin
        ) %>% 
        select(år, termin, everything())
}

# Läs in alla tabeller
platser = data_frame()
for (år in 16:17) {
    for (termin in 1:2) {
        
        platser = bind_rows(platser, cr_platstermin(år, c("vt", "ht")[termin] ))
        
    }
}
platser = bind_rows(platser, cr_platstermin(18, "vt"))

platserNek = platser %>% 
    filter(ämnesgrupp == "Nationalekonomi", poäng == 30) %>% 
    select(-inst, -prog_kurs, -anmkod, -ämnesgrupp, -godkänt) %>% 
    select(år, termin, kursid, titel, everything()) %>% 
    arrange(titel, termin)

saveRDS(platserNek, "~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/platser.rds")
