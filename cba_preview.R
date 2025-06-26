## Cost benefit preview
library(tidyverse)

account <- c(
  "Infrastruktuurin investointi",
  "Infrastruktuurin kunnossapito",
  "Liikennöinti",
  "Lipputulot",
  "Ajoneuvon käyttö",
  "Matka-aika",
  "Terveys",
  "Terveydenhuolto",
  "Onnettomuus",
  "Ilmastonmuutos",
  "Ilmansaaste",
  "Melu"
)
stakeholder <- c(
  rep("Kaupunki", 4),
  rep("Yksilö", 3),
  rep("Yhteiskunta",5)
)

bus <- c(
  15687000, 
  5228000, 
  77274000, 
  -146013000, 
  0, 
  631014000,
  0, 
  0, 
  3607000, 
  11627000, 
  3608000, 
  21313000
)

df <- data.frame(
  Stakeholder=stakeholder,
  Account=account,
  Bus=bus
)

ggplot(df, aes(x=Stakeholder, weight=Bus/1000000, fill=Account))+geom_bar()+
  labs(
    title = "Bussiliikenteen sidosryhmäkohtaiset kustannukset",
    y = "Kustannukset, MEUR"
  )
