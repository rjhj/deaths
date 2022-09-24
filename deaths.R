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


# Different causes of deaths by region ------------------------------------
# source: 11bt -- Deaths by underlying cause of death (time series classification) and region, 1969-2020
# https://statfin.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__ksyyt/statfin_ksyyt_pxt_11bt.px/

# For population per region
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


# For causes of death per region
url_3 = "https://statfin.stat.fi:443/PxWeb/api/v1/fi/StatFin/ksyyt/statfin_ksyyt_pxt_11bt.px"

# For English translations of causes
url_4 = "https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/ksyyt/statfin_ksyyt_pxt_11bt.px"

query_3 = list("Maakunta" = regions,
               "Tilaston peruskuolemansyy (aikasarjaluokitus)" = c("*"),
               "Vuosi" = years,
               "Tiedot" = c("*"))

px_data_3 <- pxweb_get(url = url_3, query = query_3)
px_data_4 <- pxweb_get(url = url_4, query = query_3)

causes <- as_tibble(as.data.frame(px_data_3, column.name.type = "text", variable.value.type = "text"))
eng_translation <- as.data.frame(px_data_4, column.name.type = "text", variable.value.type = "text")

eng_translation <- eng_translation |>
  select("Underlying cause of death (time series classification)")

causes <- causes |>
  bind_cols(eng_translation) |>
  left_join(population, by = c("Maakunta" = "Alue", "Vuosi" = "Vuosi")) |>
  mutate(Maakunta = str_remove_all(Maakunta, "MK.. ")) |>
  rename(region = Maakunta,
         cause = "Tilaston peruskuolemansyy (aikasarjaluokitus)",
         cause_eng = "Underlying cause of death (time series classification)",
         year = Vuosi, deaths = Kuolleet, population = "Väestö 31.12.") |>
  mutate(deaths_per_100k = deaths/population * 100000)

causes_summary <- causes |>
  group_by(region, cause_eng) |>
  summarise(deaths_per_100k = mean(deaths_per_100k))

municipalities <- get_municipalities(year = 2020, scale = 4500)

municipalities <- municipalities |>
  group_by(maakunta_name_fi) |>
  summarise() |>
  rename(region = maakunta_name_fi)

causes_coord <- municipalities |>
  left_join(causes_summary, by = "region")

add_linebreak <- function(s1){
  
  middle_position = (nchar(s1) -1) / 2
  
  second_half = str_sub(s1, middle_position)
  
  space_location_second_half = str_locate(second_half, " ") - 1
  
  space_location = middle_position + space_location_second_half[[1]]
  
  substr(s1, space_location, space_location ) <- "\n"
  s1
}

by_cause <- function(cause_1){
  
    title_1 = add_linebreak(cause_1)
    title_2 = "48 Accidental poisonings excl. accidental poisoning\n by alcohol (X40-X44, X46-X49, Y10-Y15)"
    causes_coord |>
      filter(cause_eng == cause_1) |>
      ggplot() + 
      geom_sf(aes(geometry = geom, fill = deaths_per_100k)) +
      scale_fill_distiller(palette = "Spectral") +
      labs(subtitle = title_1, fill = NULL) +
      theme(legend.position = c(0.2, 0.6),
            legend.background = element_blank())
    # Color option for color blind:
    # scale_fill_viridis_c(option = "turbo")
}

by_cause("00-54 Total")
by_cause("01-03 Certain infectious and parasitic diseases (A00-B99, J65)")
by_cause("48 Accidental poisonings excl. accidental poisoning by alcohol (X40-X44, X46-X49, Y10-Y15)")
by_cause("35 Other diseases of the respiratory system (J00-J06, J20-J39, J60-J64, J66-J848, J85-J99)")




causes_coord |>
  filter(cause == "50 Itsemurhat (X60-X84, Y870)") |>
  ggplot() + 
  geom_sf(aes(geometry = geom, fill = deaths_per_100k)) +
  scale_fill_distiller(palette = "Spectral")



(p1 | (p2 / p3)) + 
  plot_annotation(title = "Cause of death (2016-2020) by region, (yearly deaths / 100,000 people)")
