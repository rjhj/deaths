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
  geom_text(aes(label = deaths_daily), vjust = 1.5, color = "white") +
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
  theme(axis.text.x = element_text(angle=90)) +
  labs(subtitle = "Per decade",
       y = NULL,
       x = NULL,
       caption = "source: Tilastokeskus, Kuolleet kuukausittain, 1945-2021") -> p2

p1 / p2
