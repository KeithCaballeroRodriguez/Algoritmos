library(lubridate)
library(tidyverse)
library(readxl)
#install.packages("dtplyr")

df_p <- read_csv(url("https://cloud.minsa.gob.pe/s/Y8w3wHsEdYQSZRp/download"))
df_m <- read_csv(url("https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download"))


dp <- df_p %>% 
  drop_na()

dm <- df_m %>% 
  drop_na(FECHA_FALLECIMIENTO)


# POSITIVISMO -------------------------------------------------------------

dp <- dp %>% 
  mutate(FECHA_RESULTADO=dmy(FECHA_RESULTADO)) %>% 
  mutate(
    semana = isoweek(FECHA_RESULTADO),
    etap=case_when(
      EDAD>=0 & EDAD <11 ~ "NINOS",
      EDAD>=11 & EDAD <17 ~ "ADOLESCENTES",
      EDAD>=17 & EDAD <29 ~ "JOVENES",
      EDAD>=29 & EDAD <59 ~ "ADULTOS",
      EDAD>=59 & EDAD <130 ~ "ADULTOS_MAYORES"
    ),id=1
  )

d_p <- dp %>% 
  mutate(id=1) %>% spread(METODODX,id) %>% 
  mutate(id=1) %>% spread(SEXO,id) %>% 
  mutate(id=1) %>% spread(etap,id) %>%
  select(-UUID,-PROVINCIA,-DISTRITO,-EDAD) %>%
  group_by(DEPARTAMENTO,FECHA_RESULTADO,semana) %>% 
  summarise_at(vars(PCR:NINOS),sum) %>% 
  replace(is.na(.), 0)
  


# FALLECIDOS --------------------------------------------------------------


dm <- dm %>% 
  mutate(
    FECHA_RESULTADO=dmy(FECHA_FALLECIMIENTO),
    semana = isoweek(dmy(FECHA_FALLECIMIENTO))
  )

d_m <- dm %>% 
  select(-UUID,-PROVINCIA,-DISTRITO,-SEXO,-EDAD_DECLARADA,-FECHA_NAC) %>%
  count(DEPARTAMENTO,FECHA_FALLECIMIENTO,semana) %>% 
  mutate(FECHA_FALLECIMIENTO = dmy(FECHA_FALLECIMIENTO))

d_p
d_m


dias <- seq(ymd("2020-03-06"),ymd(today()),by = "days")
depa <- unique(d_p$DEPARTAMENTO)

final <- tibble(expand.grid(DEPARTAMENTO=depa,DIA=dias))
final %>% 
  left_join(d_p,by=c("DEPARTAMENTO"="DEPARTAMENTO",
                     "DIA"="FECHA_RESULTADO")) %>% 
  left_join(d_m,by=c("DEPARTAMENTO"="DEPARTAMENTO",
                     "DIA"="FECHA_FALLECIMIENTO",
                     "semana"="semana")) %>% 
  rename(FALLECIDOS=n) %>% 
  mutate(Total=PCR+PR) %>% 
print(n=24)

















