rm(list = ls())
dev.off()

Sys.setenv(TZ= "UTC")

try_require <- function(package, fun){
  if(requireNamespace(package, quietly = TRUE)){
    library(package, character.only = TRUE)
    return(invisible())
  }
}

try_require("maps", "map_data")


library(tidyverse)
library(maps)
library(mapproj)


# donnees cartographique des USA
usa_tbl <- map_data("state")%>% as_tibble()

# carte des USA
usa_tbl%>%
  ggplot(aes(long, lat, map_id = region))+
  geom_map(
    map = usa_tbl,
    color = "gray80", fill = "gray30", size = 0.3
  )+
  coord_map("ortho", orientation = c(39, -98, 0))


# merge data
usa <- readxl::read_excel("usa.xlsx")
repulican_ratio_tbl <- usa%>%
  mutate(etat = str_to_lower(etat))

usa_ratio_tbl <- usa_tbl%>%
  left_join(repulican_ratio_tbl, by = c("region" = "etat"))
usa_ratio_tbl


# cartographie et data
usa_ratio_tbl$subregion <- NA

usa_graph_1 <- usa_ratio_tbl%>%
  ggplot(aes(long, lat, group = subregion))+
  geom_map(
    aes(map_id = region),
    map = usa_tbl,
    color = "gray80", fill = "gray30", size = 0.9
  )+
  coord_map("ortho", orientation = c(39, -98, 0))+
  geom_polygon(aes(group = group, fill = ratio1_cas_covid), color = "white")+
  scale_fill_gradient2(low = "blue", mid = "red", high = "yellow",
                       midpoint = 0.5)+
  labs(
    title = "ratio cas de covid et ratio des vaccines par Etat aux Etats-unis",
    x = "", y = "", fill = "ratio1_covid_cases"
  )+
  theme(
    plot.title = element_text(size = 8, face = "bold", color = "red3"),
    legend.position = "right" 
  )

usa_graph_1+
  geom_point(aes(x=long, y=lat, size= ratio2_vax_doses), colour="purple", 
             fill="pink", pch="+",
             alpa=I(0.05)
  )










