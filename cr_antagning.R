# cr_antagning.R
# Skapa antagningsdata för hela ISV

library(tidyverse)
library(readxl)
library(janitor)

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
    clean_names()
    
saveRDS(antagning, "~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/antagning.rds")

antagningNE = antagning %>% 
    filter(str_sub(kurs_programkod, 5,6) == "NE") %>% 
    mutate(
        pnr = str_c(str_sub(personnummer, 3, 8), str_sub(personnummer, 10)),
        BI = str_extract(str_extract(meritvärde, "BI (.*)"), "[\\d.]+"),
        HP = str_extract(str_extract(meritvärde, "HP (.*)"), "[\\d.]+"),
        HPGR = str_extract(str_extract(meritvärde, "HPGR (.*)"), "[\\d.]+"),
        BIEX = str_extract(str_extract(meritvärde, "BIEX (.*)"), "[\\d.]+"),
        BII = str_extract(str_extract(meritvärde, "BII (.*)"), "[\\d.]+"),
        SA = str_extract(str_extract(meritvärde, "SA (.*)"), "[\\d.]+"),
        grundläggande_behörighet = str_to_title(grundläggande_behörighet)
    ) %>%
    mutate(
        `grundläggande_behörighet` = str_to_title(grundläggande_behörighet),
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
    rename(kurs = kurs_programkod)

saveRDS(antagningNE, "~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/antagning-ne.rds")

