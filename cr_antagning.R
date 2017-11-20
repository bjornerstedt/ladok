# cr_antagning.R
# Skapa antagningsdata för hela ISV

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

cr_antagningstermin = function(år, termin) {
    col_types_ant = c("text", "text", "text", 
                      "text", "text", "text", "text", "numeric", 
                      "text", "text", "text", "text", "text", 
                      "text", "text", "text", "text", "text", 
                      "text", "text", "text", "text", "text", 
                      "text", "text", "text")
    terminer = c("vt", "ht")
    filename = 
        sprintf("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/antagning_%s%d.xls", terminer[termin], år)
    read_excel(filename, col_types = col_types_ant) %>% 
        mutate(
            antagningsår = 2000 + år,
            antagningstermin = termin
        ) %>% 
        select(antagningsår, antagningstermin, everything())
}

# Läs in alla tabeller
antagning = data_frame()
for (år in 13:17) {
    for (termin in 1:2) {
        antagning = bind_rows(antagning, cr_antagningstermin(år, termin))
    }
}
antagning = bind_rows(antagning, cr_antagningstermin(18, 1))

antagning = antagning %>% 
    select(-Anmälningskod) %>%
    mutate(Anmälningsdatum = ymd(Anmälningsdatum)) %>%
    clean_names() %>% 
    mutate(
        pnr = str_c(str_sub(personnummer, 3, 8), str_sub(personnummer, 10)),
        BI = as.numeric( str_extract(str_extract(meritvärde, "BI (.*)"), "[\\d.]+")),
        HP = as.numeric( str_extract(str_extract(meritvärde, "HP (.*)"), "[\\d.]+")),
        HPGR = as.numeric( str_extract(str_extract(meritvärde, "HPGR (.*)"), "[\\d.]+")),
        BIEX = as.numeric( str_extract(str_extract(meritvärde, "BIEX (.*)"), "[\\d.]+")),
        BII = as.numeric( str_extract(str_extract(meritvärde, "BII (.*)"), "[\\d.]+")),
        SA = as.numeric( str_extract(str_extract(meritvärde, "SA (.*)"), "[\\d.]+")),
        grundläggande_behörighet = str_to_title(grundläggande_behörighet),
        # reservnummer = str_extract(reservnummer, "+* (0)"),
        reserv = (str_sub(resultat, 1, 6) == "Reserv"), 
        antagen = (resultat == "Antagen") 
    ) %>%
    separate(reservnummer, into = c("antagningsgrupp", "antagningsbetyg"), remove = FALSE) %>% 
    mutate(
        antagningsbetyg = as.numeric(antagningsbetyg),
        antagningsbetyg = coalesce(antagningsbetyg, -1),
        antagningsgrupp = coalesce(antagningsgrupp, ""),
        antagningsbetyg = if_else( antagningsbetyg == 0, 
            as.numeric( str_extract(str_extract(meritvärde, str_c(antagningsgrupp, " (.*)")), "[\\d.]+")),
            -1)
    ) %>% 
    mutate(
        `behörighet` = str_extract(grundläggande_behörighet, "[:alpha:]+"),
        behörighet = str_replace(behörighet, "sprogrammet", ""),
        behörighet = str_replace(behörighet, "programmet", ""),
        behörighet = if_else(is.na(fritext), behörighet, "Internationell"),
        behörighet = str_replace(behörighet, "Natur.*", "Natur"),
        behörighet = str_replace(behörighet, "Samhäll.*", "Samhälle"),
        behörighet = str_replace(behörighet, "Ekonom.*", "Ekonomi"),
        behörighet = coalesce(behörighet, "Annan"),
        behörighet = if_else(
            str_detect(behörighet, "Sam|Nat|Komvux|Ekonomi|Internat|Friskol"),
            behörighet,
            "Annan"
        )
    ) %>% 
    arrange(pnr, anmälningsdatum)

saveRDS(antagning, "~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/antagning.rds")

KurserProgram =
    read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/KurserProgram.xlsx") %>% 
    filter(Nek_kurs == 1) %>% 
    distinct(Kod)

saveRDS(antagning 
    %>% filter(kurs_programkod %in% KurserProgram$Kod), 
    #  %>% filter(str_sub(kurs_programkod, 5,6) == "NE"),
    "~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/antagning-ne.rds")

antagningProgram = antagning %>% 
    filter(str_sub(kurs_programkod, 1,1) == "P")
