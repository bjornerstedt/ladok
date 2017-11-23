library(tidyverse)
library(lubridate)
library(stringr)

load("~/Library/Mobile Documents/com~apple~CloudDocs/Work/data/admin/nekdata.rda")

kurs = 
    kurs %>% 
    select(kursid = kod, kursnamn = benamne) 

prov = 
    prov %>% 
    select(kursid = kurs, prov, delkurs=benamne) %>% 
    inner_join(kurs, by = "kursid") 

undkprov = 
    undkprov %>% 
    rename(kursid=kurs) %>% 
    group_by( pnr, kursid, prov) %>% 
    summarise(u = n(), idatum = last(idatum), betyg = last(betyg))
    
underkänd = undkprov %>% 
    group_by(pnr, kursid) %>% 
    summarise(u = sum(u))
    
ffgkurs = 
    ffgkurs %>% 
    select(pnr, kursid = kurs, progr, termin, kurstakt, ort, omgang, regdatum=idatum) %>% 
    inner_join(kurs, by = "kursid") %>% 
    left_join(underkänd, by = c("kursid", "pnr") ) %>% 
    collect() %>% 
    mutate(
        ar = trunc(termin/10),
        terminid = termin - ar*10,
        regdatum = as_date(regdatum),
        akademiskt_ar = if_else(terminid == 2, ar, ar - 1),
        pnr_chr = is.na(as.numeric(str_sub( pnr , 7,7))), 
        kvinna = as.numeric(str_sub( pnr , 9, 9)) %% 2 == 0,
        birthyear = as.numeric(str_sub( pnr , 1, 2)),
        alder = 2017 - birthyear - if_else(birthyear < 20, 2000, 1900),
        u = coalesce(u, 0L)
    ) %>% 
    rename(utot = u) %>% 
    select(-termin, -kurstakt, -termin, -ort, -omgang, -birthyear)

godkprov = godkprov %>% 
    select(kursid=kurs, pnr,  prov, betyg, idatum) 
    
godkprov = 
    godkprov %>% 
    left_join(undkprov %>% select(-betyg, -idatum) , by = c("pnr", "kursid", "prov")) %>% 
    bind_rows(undkprov %>% anti_join(godkprov, by = c("pnr", "kursid", "prov") )) %>% 
    mutate(u = coalesce(u, 0L)) %>% 
    inner_join(prov, by = c("kursid", "prov")) %>% 
    mutate(
        idatum = as_date(idatum)
    ) %>% 
    left_join(ffgkurs, by = c("kursid", "kursnamn","pnr") ) %>% 
    mutate(
        duration = as.integer(idatum - regdatum),
        kursnamn = if_else( is.na(str_match(kursnamn, "^Economics .$")), "Masters", kursnamn )
    )

godkkurs = 
    godkkurs  %>% 
    select( kursid=kurs, pnr,  betyg, datum=idatum) %>% 
    collect() %>% 
    mutate(
        datum = as_date(datum)
    )

# Testa att alla godkända var registrerade
if(anti_join(godkkurs, ffgkurs, by = c("pnr", "kursid")) %>% tally > 0) stop("Godkända som inte registrerats")

kursresult = 
    left_join(ffgkurs, godkkurs, by = c("pnr", "kursid")) %>% 
    left_join(inregffg %>% select(pnr, kursid = kurs, avbrott = avbrdat), by = c("pnr", "kursid")) %>% 
    mutate(
        betyg = coalesce(betyg, "U"),
        duration = as.integer(datum - regdatum),
        termindatum = make_date(ar, 1 + (terminid - 1) * 6, 1 ),
        avbrott = !is.na(avbrott),
        kursnamn = if_else( is.na(str_match(kursnamn, "^Economics .$")), "Masters", kursnamn )
    ) 

# Beräkna godkända per delkurs
andelar_prov = godkprov %>% 
    group_by(kursid, prov, betyg) %>% 
    summarise(antal = n(), u = sum(u)) %>% 
    group_by(kursid, prov) %>% 
    mutate(
        andel = antal / sum(antal) * 100,
        betyg = factor(betyg, levels = c("U","VG","G"))
    ) 

# Beräkna godkända per kurs
andelar = kursresult %>% 
    filter(betyg!="U" | utot!=0) %>%  # Ta bort de som inte tenterar alls
    group_by(kursnamn, termindatum, betyg) %>% 
    summarise(antal = n()) %>% 
    group_by(kursnamn, termindatum) %>% 
    mutate(
        andel = antal / sum(antal) * 100,
        betyg = factor(betyg, levels = c("U","VG","G"))
    ) %>% 
    filter( year(termindatum) < 2017)

andelar_program = kursresult %>% 
    filter(betyg!="U" | utot!=0) %>%  # Ta bort de som inte tenterar alls
    mutate(
        program = !is.na(progr)
    ) %>% 
    group_by(kursnamn, termindatum, program, betyg) %>% 
    summarise(antal = n()) %>% 
    group_by(kursnamn, termindatum, program) %>% 
    mutate(
        andel = antal / sum(antal) * 100,
        betyg = factor(betyg, levels = c("U","VG","G"))
    ) %>% 
    filter( year(termindatum) < 2017)

save(ffgkurs, godkprov, kursresult, andelar, andelar_program, andelar_prov,
    file = "ladok2.Rdata")

