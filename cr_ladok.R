library(tidyverse)
library(readxl)
library(lubridate)

con <- dbConnect(RMySQL::MySQL(),
                 host = "localhost",
                 user = "root",
                 dbname = "ladok",
                 password = "pwdpwd"
                 # password = rstudioapi::askForPassword("Database password")
)

kursbeskrivning <- 
    tbl(con, "kurs") %>% 
    collect() %>% 
    select(kursid = kod, kursnamn = benamne) 

kursnamn <- 
    tbl(con, "prov") %>% 
    collect() %>% 
    select(kursid = kurs, prov, benamn) %>% 
    left_join(kursbeskrivning, by = "kursid")

ffgkurs <- 
    tbl(con, "ffgkurs") %>% 
    collect() %>% 
    select(pnr, kursid = kurs, progr, termin, kurstakt, ort, omgang, regdatum=idatum) %>% 
    # left_join(kursnamn %>% select(kursid = kurs, kurs = kursnamn), by = "kursid") %>% 
    mutate(
        ar = trunc(termin/10),
        terminid = termin - ar*10,
        regdatum = as_date(regdatum),
        akademiskt_ar = if_else(terminid == 2, ar, ar - 1)
    )

godkprov <- 
    tbl(con, "godkprov") %>% 
    collect() %>% 
    select(kursid=kurs, pnr,  prov, betyg, progr, idatum) %>% 
    left_join(kursnamn, by = c("kursid", "prov")) 

godkkurs <- 
    tbl(con, "godkkurs") %>% 
    collect() %>% 
    select( kursid=kurs, pnr,  betyg, datum) %>% 
    mutate(
        datum = as_date(datum)
    )

kursresult = 
    left_join(ffgkurs, godkkurs, by = c("pnr", "kursid")) %>% 
    mutate(
        betyg = coalesce(betyg, "U"),
        duration = as.integer(datum - regdatum),
        termindatum = make_date(ar, 1 + (terminid - 1)*6, 1 )
    ) %>% 
    left_join(kursbeskrivning, by = "kursid")

save(ffgkurs, godkprov, kursresult, file = "~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/ladok.Rdata")

dbDisconnect(con) 
