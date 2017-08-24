library(tidyverse)
library(readxl)
library(lubridate)

con = dbConnect(RMySQL::MySQL(),
                 host = "localhost",
                 user = "root",
                 dbname = "ladok",
                 password = "pwdpwd"
                 # password = rstudioapi::askForPassword("Database password")
)

kurs = 
    tbl(con, "kurs") %>% 
    filter(inst == 43) %>% 
    select(kursid = kod, kursnamn = benamne) 

prov = 
    tbl(con, "prov") %>% 
    select(kursid = kurs, prov, delkurs=benamne) %>% 
    inner_join(kurs, by = "kursid") 

ffgkurs = 
    tbl(con, "ffgkurs") %>% 
    select(pnr, kursid = kurs, progr, termin, kurstakt, ort, omgang, regdatum=idatum) %>% 
    inner_join(kurs, by = "kursid") %>% 
    collect() %>% 
    mutate(
        ar = trunc(termin/10),
        terminid = termin - ar*10,
        regdatum = as_date(regdatum),
        akademiskt_ar = if_else(terminid == 2, ar, ar - 1)
    ) %>% 
    select(-termin, -kurstakt, -termin, -ort, -omgang)

godkprov = 
    tbl(con, "godkprov") %>% 
    select(kursid=kurs, pnr,  prov, betyg, idatum) %>% 
    inner_join(prov, by = c("kursid", "prov")) %>% 
    collect() %>% 
    mutate(
        idatum = as_date(idatum)
    ) %>% 
    left_join(ffgkurs, by = c("kursid", "pnr") ) %>% 
    mutate(
        duration = as.integer(idatum - regdatum)
    )

godkkurs = 
    tbl(con, "godkkurs") %>% 
    select( kursid=kurs, pnr,  betyg, datum=idatum) %>% 
    collect() %>% 
    mutate(
        datum = as_date(datum)
    )

kursresult = 
    left_join(ffgkurs, godkkurs, by = c("pnr", "kursid")) %>% 
    mutate(
        betyg = coalesce(betyg, "U"),
        duration = as.integer(datum - regdatum),
        termindatum = make_date(ar, 1 + (terminid - 1) * 6, 1 )
    ) 

save(ffgkurs, godkprov, kursresult, file = "~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/ladok.Rdata")

dbDisconnect(con) 
