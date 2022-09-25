library(tidyverse)
library(patchwork)
library(geofi)
library(pxweb)
library(scales)

# Little summary here:
# tell the purpose of this work and few important stats


# Overview of deaths in Finland -------------------------------------------

url = "https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/vaerak/statfin_vaerak_pxt_11rb.px"
query = list("Sukupuoli" = c("1", "2"), "Tiedot"=c("vaesto"), "Vuosi"=c("*"))
px_data <- pxweb_get(url = url, query = query)
df <- as_tibble(as.data.frame(px_data, column.name.type = "text", variable.value.type = "text"))

url = "https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/kuol/statfin_kuol_pxt_12af.px"
query = list("Sukupuoli" = c("1", "2"), "Tiedot"=c("*"), "Vuosi"=c("*"))
px_data <- pxweb_get(url = url, query = query)
deaths <- as_tibble(as.data.frame(px_data, column.name.type = "text", variable.value.type = "text"))



df <- df |>
  filter(Year != "1750") |>
  left_join(deaths) |>
  rename(population = "Population 31 Dec",
         year = Year,
         sex = Sex,
         deaths = Deaths) |>
  mutate(year = as.integer(year),
         sex = as_factor(sex))

# Source: http://www.saunalahti.fi/arnoldus/kuolovuo.html
labels = tribble(
  ~year, ~deaths, ~label,
  1868,  137720, "Finnish famine (1866–1868)",
  1918,  95102,  "Civil War (1918)",
  1948,  71846,  "Winter, Continuation,\nLapland Wars (1939–45)",
  1833,  63738,  "Smallpox, dysentery\n& influenza (1833)",
  1806,  53942,  "Finnish War (1808-09)"
)

df_year_deaths <- df |>
  group_by(year) |>
  summarise(deaths = sum(deaths), population = sum(population))


ggplot(df_year_deaths, aes(year, deaths)) +
  geom_line(size = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label=label), size=3.5, vjust = -0.5, data=labels) +
  labs(title = "Deaths in Finland (1751 - 2021)",
       subtitle = "In 2021 there were 57,659 deaths in Finland, highest since 1940s",
       y = NULL,
       x = NULL) -> p1

df |>
  filter(year >= 1920) |>
ggplot(aes(year, deaths, color = sex)) +
  geom_line(size = 1.1) +
  scale_colour_hue(direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_blank()) +
  labs(title = "Deaths by sex (1920 - 2021)",
       subtitle = "Many men died during 1939–45 wars",
       y = NULL,
       x = NULL) -> p2

df_year_deaths |>
  mutate(death_rate = round(deaths / population * 100000)) |>
         ggplot(aes(year, death_rate)) +
  geom_line(size = 1.1) +
  labs(title = "Deaths / 100,000 people",
       subtitle = "Life has been getting safer and people live longer",
       y = NULL,
       x = NULL) -> p3


p1 /
(p2 | p3)


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


# What are the causes of deaths by region? ------------------------------------
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


p1 <- by_cause("00-54 Total")
p2 <- by_cause("04-22 Neoplasms (C00-D48)")
p3 <- by_cause("23-24 Endocrine, nutritional and metabolic diseases (E00-E90)")

p4 <- by_cause("25 Dementia, Alzheimers disease (F01, F03, G30, R54)")
p5 <- by_cause("27-30 Diseases of the circulatory system excl. alcohol-related (I00-I425, I427-I99)")
p6 <- by_cause("31-35 Diseases of the respiratory system (J00-J64, J66-J99)")

p7 <- by_cause("41 Alcohol-related diseases and accidental poisoning by alcohol")
p8 <- by_cause("42 Land traffic accidents")
p9 <- by_cause("47 Accidental drownings (W65-W74)")

p10 <- by_cause("50 Suicides (X60-X84, Y870)")
p11 <- by_cause("51 Assault (X85-Y09, Y871)")
p12 <- by_cause("54 No death certificate")

((p1 | p2 | p3) /  
(p4 | p5 | p6)) + 
  plot_annotation(title = "Underlying cause of death (/ 100,000 people)")

(p7 | p8 | p9) /
(p10 | p11 | p12)


# What is the life expectancy by age and sex? -----------------------------


url_5 = "https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/kuol/statfin_kuol_pxt_12ap.px"

query_5 = list("Sukupuoli" = c("1", "2"),
               "Tiedot"=c("*"),
               "Vuosi"=c("2020"),
               "Ikä" = c("*"))

px_data_5 <- pxweb_get(url = url_5, query = query_5)
life <- as_tibble(as.data.frame(px_data_5, column.name.type = "text", variable.value.type = "text"))

names(life)
life <- life |>
  mutate(Age = as.integer(Age),
         Sex = as_factor(Sex)) |>
  rename(life_expentancy = "Life expectancy, years",
         survivors = "Survivors of 100,000 born alive",
         death_prob = "Probability of death, per mille")

ggplot(life, aes(Age, death_prob, color = Sex)) +
  geom_point()
