library(jsonlite)
library(tidyverse)

j <- read_json("data/indicators/stats.json")
df <- data.frame()
for(instance in 1:length(j)) {
  j[[instance]]["indicators_by_category"] <- NULL
  df[instance, "instance"] <- names(j[instance])
  for(col in names(j[[instance]])) {
    df[instance, col] <- j[[instance]][col]
  }
}
df$fraction_indicatored_actions <- df$actions_connected_to_an_indicator / df$actions * 100
select <- ! df$instance %in% c("hnh2035", "hame-ilmasto")

ggplot(data=df[select,], aes(x=actions, y=indicators, size=fraction_indicatored_actions,
                             color=fraction_indicatored_actions))+geom_point(shape=1)+
  guides(color="none")+
  labs(
#    title="Toimenpiteiden ja mittareiden määrät eri ilmastovahdeissa",
#    x="Toimenpiteitä (kpl)",
#    y="Mittareita (kpl)",
#    size="Toimenpiteellä mittari (%)")
    title="Numbers of actions and indicators at various Kausal Platforms",
    x="Actions (#)",
    y="Indicators (#)",
    size="Actions with indicators (%)")
ggsave("Actions and indicators at Kausal Platforms.svg", width=8, height=4)

ggplot(data=df[order(-df$fraction_indicatored_actions),],
       aes(x=1:nrow(df), y=fraction_indicatored_actions, group=instance))+geom_point()+
  labs(
#    title="Kunnittain niiden toimenpiteiden osuus, joihin kytkeytyy mittari",
#    x="Kunta",
#    y="Osuus toimenpiteistä, joilla on mittari (%)"
    title="Fraction of actions with indicators, by municipality",
    x="Municipality",
    y="Fraction of actions with an indicator (%)"
  )
ggsave("Fractions of actions with indicators at Kausal Platforms.svg", width=8, height=4)
