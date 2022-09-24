library(tidyverse)
library(patchwork)
library(geofi)
library(pxweb)

# How deadly are the different months? ----------------------------------------

url_1 = "https://statfin.stat.fi:443/PxWeb/api/v1/fi/StatFin/kuol/statfin_kuol_pxt_12ah.px"

query_1 = list("Tapahtumakuukausi" = c("M01", "M02", "M03", "M04", "M05", "M06",
                                       "M07", "M08", "M09", "M10", "M11", "M12"),
               "Tiedot"=c("*"),
               "Vuosi"=c("*"))

px_data_1 <- pxweb_get(url = url_1, query = query_1)
monthly <- as.data.frame(px_data_1, column.name.type = "text", variable.value.type = "text")

translate_months  <- month.abb 
names(translate_months) <- unique(monthly$Tapahtumakuukausi) 


monthly <- monthly |>
  as_tibble() |>
  rename(year = Vuosi, month = Tapahtumakuukausi, deaths = Kuolleet) |>
  mutate(month = str_replace_all(month, translate_months)) |>
  mutate(month = as_factor(month))

avg_days_per_month = c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# Total
monthly |>
  group_by(month) |>
  summarise(deaths = mean(deaths)) |>
  mutate(deaths_daily = as.integer(deaths / avg_days_per_month)) |>
  ggplot(aes(month, deaths_daily)) +
  geom_col() +
  geom_text(aes(label = deaths_daily), vjust = 1.5,
            color = "white", size = 3.5) +
  labs(title = "Daily deaths in Finland per month (1945-2021)",
       subtitle = "Death rates are higher in winter months",
       y = NULL,
       x = NULL) -> p1

# Per decade
monthly |>
  mutate(year = as.integer(year)) |>
  mutate(decade = year - year %% 10) |>
  group_by(decade, month) |>
  filter(decade <= 2010) |>
  mutate(decade = as.character(paste0(decade, "s")),
         decade = str_replace_all(decade, "1940s", "1945-1949")) |>
  summarise(deaths = mean(deaths)) |>
  mutate(deaths_daily = as.integer(deaths / avg_days_per_month)) |>
  ggplot(aes(month, deaths_daily)) +
  geom_col() +
  facet_wrap(vars(decade), nrow = 2) +
  geom_text(aes(label = deaths_daily),
            color = "white", size = 3.5, angle = 270, hjust = -0.15) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(subtitle = "Per decade",
       y = NULL,
       x = NULL,
       caption = "source: Tilastokeskus - Kuolleet kuukausittain, 1945-2021") -> p2

p1 / p2


# Different causes of deaths per region ------------------------------------

url_2 = "https://statfin.stat.fi:443/PxWeb/api/v1/fi/StatFin/vaerak/statfin_vaerak_pxt_11ra.px"

regions = c("MK01", "MK02", "MK04", "MK05",
         "MK06", "MK07", "MK08", "MK09",
         "MK10", "MK11", "MK12", "MK13",
         "MK14", "MK15", "MK16", "MK17",
         "MK18", "MK19", "MK21")

years = c("2016", "2017", "2018", "2019", "2020")

query_2 = list("Alue" = regions,
               "Tiedot" = c("vaesto"),
               "Vuosi" = years)

px_data_2 <- pxweb_get(url = url_2, query = query_2)
population <- as.data.frame(px_data_2, column.name.type = "text", variable.value.type = "text")

url_3 = "https://statfin.stat.fi:443/PxWeb/api/v1/fi/StatFin/ksyyt/statfin_ksyyt_pxt_11bt.px"
query_3 = list("Maakunta" = regions,
               "Tilaston peruskuolemansyy (aikasarjaluokitus)" = c("*"),
               "Vuosi" = years,
               "Tiedot" = c("*"))

px_data_3 <- pxweb_get(url = url_3, query = query_3)
causes <- as.data.frame(px_data_3, column.name.type = "text", variable.value.type = "text")


causes <- causes |>
  rename(Alue = Maakunta) |>
  left_join(population, by = c("Alue", "Vuosi")) |>
  rename(region = Alue, cause = "Tilaston peruskuolemansyy (aikasarjaluokitus)",
         year = Vuosi, deaths = Kuolleet, population = "Väestö 31.12.") |>
        mutate(region = str_remove_all(region, "MK.. "))
  

causes <- causes |>
  mutate(deaths_per_100k = deaths/population * 100000) |>
  group_by(region, cause) |>
  summarise(deaths_per_100k = mean(deaths_per_100k))

municipalities <- get_municipalities(year = 2020, scale = 4500)

municipalities <- municipalities |>
  group_by(maakunta_name_fi) |>
  summarise() |>
  rename(region = maakunta_name_fi)

attributes(municipalities)

causes_coord <- causes |>
  left_join(municipalities, by = "region")

causes_coord |>
  filter(cause == "00-54 Yhteensä") |>
  ggplot() + 
   geom_sf(aes(geometry = geom, fill = deaths_per_100k)) +
  scale_fill_distiller(palette = "Spectral")

# Color option for color blind:
# scale_fill_viridis_c(option = "turbo")


