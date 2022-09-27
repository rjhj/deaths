library(tidyverse)
library(patchwork)
library(geofi)
library(pxweb)
library(scales)
library(lubridate)

# Little summary here:
# tell the purpose of this work and few important stats


# Yearly deaths --------------------------------------------------- 
px_12at <- pxweb_get(url = 
"https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/kuol/statfin_kuol_pxt_12at.px",
          query = list("Tiedot"=c("vm01", "vm11", "vaesto"), "Vuosi"=c("*")))
  
df_12at <- as_tibble(as.data.frame(px_12at, column.name.type = "text", variable.value.type = "text"))

df_12at <- df_12at |>
  rename(Live_births = "Live births") |>
  mutate(Year = as.integer(Year),
         Death_rate = Deaths / Population * 100000)

# Source: http://www.saunalahti.fi/arnoldus/kuolovuo.html
labels = tribble(
  ~Year, ~Deaths, ~Label,
  1868,  137720, "Finnish famine (1866–1868)",
  1918,  95102,  "Civil War (1918)",
  1948,  71846,  "Winter, Continuation &\nLapland Wars (1939–45)",
  1833,  63738,  "Smallpox, dysentery\n& influenza (1833)",
  1806,  53942,  "Finnish War (1808-09)",
  2005,  52659,  "In 2021 there were\n57,659 deaths in Finland,\nhighest since 1940s"
)

ggplot(df_12at, aes(Year, Deaths)) +
  geom_line(size = 0.5) +
  scale_x_continuous(breaks = seq(1750, 2020, 25)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 150000, 10000)) +
  geom_text(aes(label=Label), size = 1, vjust = -0.5, data=labels) +
  labs(subtitle = "Deaths",
       y = NULL,
       x = NULL) +
  theme_void() +
  theme(axis.text = element_text(size = 3.5),
        title = element_text(size = 5)
        ) -> yearly_deaths_plot

yearly_plot <- function(df, y_stat, subtitle) {
  ggplot(df, aes(Year, {{y_stat}})) +
    geom_line(size = 0.3) +
    scale_x_continuous(breaks = seq(1750, 2020, 50)) +
    scale_y_continuous(labels = scales::comma) +
    labs(subtitle = subtitle,
         y = NULL,
         x = NULL) +
    theme(text = element_text(size = 4))
}

yearly_population_plot <- yearly_plot(df_12at, Population, "Population")
yearly_births_plot <- yearly_plot(df_12at, Live_births, "Live births")
yearly_death_rate_plot <- yearly_plot(df_12at, Death_rate, "Deaths / 100,000 people")

layout <- "
AAA
AAA
BCD
"

plot_yearly <- (yearly_deaths_plot + yearly_population_plot +
  yearly_births_plot + yearly_death_rate_plot +
    plot_layout(design = layout))+
  plot_annotation(subtitle = "Death & population statistics, Finland, 1749 - 2021",
                  caption = "source: Tilastokeskus 12at -- Vital statistics and population, 1749-2021",
                  theme = theme(plot.subtitle = element_text(size = 6),
                                plot.caption = element_text(size = 3)))

ggsave("images//plot_yearly.png", plot_yearly, device = "png",
       width = 8, height = 8, units = c("cm"))

# How deadly are the different months? ----------------------------------------

px_12ah <- pxweb_get(url = 
"https://statfin.stat.fi:443/PxWeb/api/v1/fi/StatFin/kuol/statfin_kuol_pxt_12ah.px",
 query = list("Tapahtumakuukausi" = c("M01", "M02", "M03", "M04", "M05", "M06",
                                      "M07", "M08", "M09", "M10", "M11", "M12"),
              "Tiedot"=c("*"), "Vuosi"=c("*")))

df_12ah <- as_tibble(as.data.frame(px_12ah, column.name.type = "text", variable.value.type = "text"))

short_month_names  <- month.abb 
names(short_month_names) <- unique(df_12ah$Tapahtumakuukausi)

df_12ah <- df_12ah |>
  mutate(Tapahtumakuukausi = str_replace_all(Tapahtumakuukausi, short_month_names)) |>
  mutate(Tapahtumakuukausi = as_factor(Tapahtumakuukausi))

avg_days_per_month = c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# Total
df_12ah |>
  group_by(Tapahtumakuukausi) |>
  summarise(Kuolleet = mean(Kuolleet)) |>
  mutate(deaths_daily = as.integer(Kuolleet / avg_days_per_month)) |>
  ggplot(aes(Tapahtumakuukausi, deaths_daily)) +
  geom_col() +
  geom_text(aes(label = deaths_daily), vjust = 1.5,
            color = "white", size = 3.5) +
  labs(title = "Daily deaths in Finland by month (1945-2021)",
       subtitle = "Death rates are higher in winter months",
       y = NULL,
       x = NULL) -> daily_deaths_month_plot

# Per decade
df_12ah |>
  mutate(Vuosi = as.integer(Vuosi)) |> 
  mutate(decade = Vuosi - Vuosi %% 10) |>
  filter(decade <= 2010) |>
  mutate(decade = as.character(paste0(decade, "s")),
         decade = str_replace_all(decade, "1940s", "1945-1949")) |>
  group_by(decade, Tapahtumakuukausi) |>
  summarise(deaths = mean(Kuolleet)) |>
  mutate(deaths_daily = as.integer(deaths / avg_days_per_month)) |>
  ggplot(aes(Tapahtumakuukausi, deaths_daily)) +
  geom_col() +
  facet_wrap(vars(decade), nrow = 2) +
  geom_text(aes(label = deaths_daily),
            color = "white", size = 3.5, angle = 270, hjust = -0.15) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(subtitle = "By decade",
       y = NULL,
       x = NULL,
       caption = "source: Tilastokeskus 12ah -- Kuolleet kuukausittain, 1945-2021"
       ) -> daily_deaths_decade_plot

plot_months <- daily_deaths_month_plot / daily_deaths_decade_plot

ggsave("images//plot_months.png", plot_months, device = "png", scale = 1.8)


# Causes of death per region --------------------------------------------------

# For causes of death per region
px_11bt <- pxweb_get(url = 
"https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/ksyyt/statfin_ksyyt_pxt_11bt.px",
query = list(
"Maakunta" = c("MK01", "MK02", "MK04", "MK05", "MK06", "MK07", "MK08", "MK09", "MK10", 
  "MK11", "MK12", "MK13", "MK14", "MK15", "MK16", "MK17", "MK18", "MK19", "MK21"),
"Tilaston peruskuolemansyy (aikasarjaluokitus)" = c("*"),
"Vuosi" = c("2016", "2017", "2018", "2019", "2020"),
"Tiedot" = c("*")))

df_11bt <- as_tibble(as.data.frame(px_11bt, column.name.type = "text", variable.value.type = "text"))

df_11bt <- df_11bt |>
  rename(Cause = "Underlying cause of death (time series classification)") |>
  mutate(Year = as.integer(Year)) |>
  separate(Region, into = c("ID", "Region"), sep = " ", extra = "merge")

# For population per region
px_11ra <- pxweb_get(url = 
"https://statfin.stat.fi:443/PxWeb/api/v1/fi/StatFin/vaerak/statfin_vaerak_pxt_11ra.px",
query = list(
"Alue" = c("MK01", "MK02", "MK04", "MK05", "MK06", "MK07", "MK08", "MK09", "MK10", 
  "MK11", "MK12", "MK13", "MK14", "MK15", "MK16", "MK17", "MK18", "MK19", "MK21"),
"Vuosi" = c("2016", "2017", "2018", "2019", "2020"),
"Tiedot" = c("vaesto")))

df_11ra <- as_tibble(as.data.frame(px_11ra, column.name.type = "text", variable.value.type = "text"))

df_11ra <- df_11ra |>
  mutate(Vuosi = as.integer(Vuosi)) |>
  rename(Population = "Väestö 31.12.") |>
  separate(Alue, into = c("ID", "Region_fi"), sep = " ", extra = "merge")

df_cause_region <- df_11bt |>
  left_join(df_11ra, by = c("ID" = "ID", "Year" = "Vuosi")) |>
  mutate(Deaths_per_100k = Deaths / Population * 100000) |>
  group_by(Region, Cause) |>
  summarise(Deaths_per_100k = mean(Deaths_per_100k), .groups = "drop")

df_region_map <- get_municipalities(year = 2020, scale = 4500)

df_region_map <- df_region_map |>
  group_by(maakunta_name_en) |>
  summarise() |>
  rename(Region = maakunta_name_en)

df_cause_region <- df_region_map |>
  left_join(df_cause_region, by = "Region")


add_linebreak <- function(s1){
  
  middle_position = (nchar(s1) -1) / 2
  
  second_half = str_sub(s1, middle_position)
  
  space_location_second_half = str_locate(second_half, " ") - 1
  
  space_location = middle_position + space_location_second_half[[1]]
  
  substr(s1, space_location, space_location ) <- "\n"
  s1
}

by_cause <- function(cause_1){
  
  title_1 <- str_remove(cause_1, " \\(.+\\)" )
  
  print(title_1)
  
  if(nchar(title_1) > 30) {
    title_1 = add_linebreak(title_1)
  }
  
  df_cause_region |>
    filter(Cause == cause_1) |>
    ggplot() + 
    geom_sf(aes(geometry = geom, fill = Deaths_per_100k)) +
    scale_fill_distiller(palette = "Spectral") +
    labs(subtitle = title_1, fill = NULL) +
    theme(legend.position = c(0.2, 0.6),
          legend.background = element_blank(),
          plot.subtitle = element_text(size = 8))
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

plot_regions_1 <- ((p1 | p2 | p3) /  
                     (p4 | p5 | p6)) + 
  plot_annotation(title = "Total and diseases related deaths",
                  subtitle = paste0("Yearly mean for 5 year period (2016-2020) ",
                                    "by underlying cause of death and region per 100,000 inhabitants."),
                  caption = "source: Tilastokeskus 11bt -- Deaths by underlying cause",
                  theme = theme(plot.subtitle = element_text(size = 10)))

plot_regions_2 <- (p7 | p8 | p9) /
  (p10 | p11 | p12) +
  plot_annotation(title = "Alchohol, accidental, suicide and other deaths",
                  subtitle = paste0("Yearly mean for 5 year period (2016-2020) ",
                                    "by underlying cause of death and region per 100,000 inhabitants."),
                  caption = "source: Tilastokeskus 11bt -- Deaths by underlying cause",
                  theme = theme(plot.subtitle = element_text(size = 10)))

ggsave("images//plot_regions_1.png", plot_regions_1, device = "png", scale = 1.0)
ggsave("images//plot_regions_2.png", plot_regions_2, device = "png", scale = 1.0)

# Life expectancy at birth---------------- -----------------------------------

px_12am <- pxweb_get(url = 
"https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/kuol/statfin_kuol_pxt_12am.px",
query = list( "Sukupuoli" = c("1", "2"), "Vuosi" = c("*"), "Tiedot" = c("*")))

df_12am <- as_tibble(as.data.frame(px_12am, column.name.type = "text", variable.value.type = "text"))
names(df_12am)
df_12am <- df_12am |>
  rename(Life_exp = "Life expectancy at birth, years") |>
  mutate(Year = str_sub(Year, 1, 4)) |>
  mutate(Year = as.integer(Year),
         Sex = as_factor(Sex))

ggplot(df_12am, aes(Year, Life_exp, color = Sex)) +
  geom_line(size = 1.1) +
  scale_colour_hue(direction = -1) +
  theme(legend.position = c(0.2, 0.81),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.06),) +
  labs(title = "By year (1751-2021)",
       y = NULL, x = NULL,
       caption = "source: Tilastokeskus 12am -- Life expectancy at birth by sex, 1751-2021"
       ) -> life_exp_plot

# Life expectancy by region
px_12an <- pxweb_get(url = 
"https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/kuol/statfin_kuol_pxt_12an.px",
query = list(
"Maakunta" = c("MK01", "MK02", "MK04", "MK05", "MK06", "MK07", "MK08", "MK09", "MK10", 
          "MK11", "MK12", "MK13", "MK14", "MK15", "MK16", "MK17", "MK18", "MK19", "MK21"),
"Vuosi" = c("2020"),
"Sukupuoli" = c("1", "2"),
"Tiedot" = c("*")))

df_12an <- as_tibble(as.data.frame(px_12an, column.name.type = "text", variable.value.type = "text"))

df_12an <- df_12an |> 
  rename(Life_exp = "Life expectancy at birth, years") |>
  mutate(Region = str_remove_all(Region, "MK.. "))
  
df_life_region <- df_region_map |>
  left_join(df_12an, by = c("Region"))

df_life_region |>
  filter(Sex == "Females") |>
  ggplot() + 
  geom_sf(aes(geometry = geom, fill = Life_exp)) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  labs(title = "By region (2018-2020)",
       subtitle = "         Females", fill = NULL) +
  theme(legend.position = c(0.2, 0.6),
        legend.background = element_blank(),
        panel.background = element_rect(colour = "#CC79A7"),
        plot.title.position = "plot",
        plot.margin = margin(r = 2.5, unit = "cm")) -> life_exp_region_f_plot

df_life_region |>
  filter(Sex == "Males") |>
  ggplot() + 
  geom_sf(aes(geometry = geom, fill = Life_exp)) +
  scale_fill_distiller(palette = "Spectral", direction = 1) +
  labs(subtitle = "Males", fill = NULL) +
  theme(legend.position = c(0.2, 0.6),
        legend.background = element_blank(),
          panel.background = element_rect(colour = "#0072B2")) -> life_exp_region_m_plot

plot_life_exp <- (life_exp_plot) /
  (life_exp_region_f_plot + life_exp_region_m_plot) + 
  plot_annotation(title = "Life expectancy (years) at birth by sex",
  caption = "source: Tilastokeskus 12an -- Life expectancy at birth by sex and region") +
  plot_layout(heights = c(1.5, 1.9))

ggsave("images//plot_life_exp.png", plot_life_exp, device = "png", scale = 1.8)

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
  1948,  71846,  "Winter, Continuation &\nLapland Wars (1939–45)",
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


df_year_deaths |>
  ggplot(aes(year, population)) +
  geom_line(size = 1.1) +
  scale_y_continuous(labels = scales::comma) +
  labs(subtitle = "Population",
       y = NULL,
       x = NULL) -> p2

df_year_deaths |>
  mutate(death_rate = round(deaths / population * 100000)) |>
  ggplot(aes(year, death_rate)) +
  geom_line(size = 1.1) +
  labs(subtitle = "Deaths / 100,000 people",
       y = NULL,
       x = NULL) -> p3

p1 /
(p2 | p3)


df |>
  filter(year >= 1920) |>
  ggplot(aes(year, deaths, color = sex)) +
  geom_line(size = 1.1) +
  scale_colour_hue(direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_blank()) +
  labs(title = "Deaths by sex (1920 - 2021)",
       subtitle = "Lots of male deaths during the war time",
       y = NULL,
       x = NULL) -> p4

p4

url = "https://statfin.stat.fi:443/PxWeb/api/v1/fi/StatFin/ksyyt/statfin_ksyyt_pxt_12z6.px"
query = list("Sukupuoli" = c("1", "2"), "Tiedot"=c("*"), "Kuukausi"=c("*"),
             "Tilaston peruskuolemansyy (aikasarjaluokitus)" = c("*"))
px_data <- pxweb_get(url = url, query = query)
deaths_cause <- as_tibble(as.data.frame(px_data, column.name.type = "text", variable.value.type = "text"))

deaths_cause <- deaths_cause |>
  rename(month = Kuukausi,
         cause = "Tilaston peruskuolemansyy (aikasarjaluokitus)",
         sex = Sukupuoli,
         deaths = Kuolleet) |>
  mutate(sex = if_else(sex == "Miehet", "males", "females")) |>
  mutate(sex = as_factor(sex))
         

avg_days_per_month = c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

deaths_cause |>
  mutate(month = str_remove_all(month, "....M")) |>
  filter(cause == "50 Itsemurhat (X60-X84, Y870)") |>
  group_by(month) |>
  summarise(deaths = mean(deaths)) |>
  mutate(daily_deaths = deaths / avg_days_per_month) |>
  ggplot(aes(month, daily_deaths)) +
  geom_col() +
  labs(title = "Daily suicides per month (data: Jan 1971 - Dec 2020)",
       subtitle = "More suicides occur during summer months",
       y = NULL,
       x = NULL)


deaths_cause |>
  mutate(month = ym(month))

deaths_cause |>
  filter(cause == "50 Itsemurhat (X60-X84, Y870)") |>
  ggplot(aes(month, deaths, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  geom_smooth(size = 2, method = "loess", formula = "y ~ x") +
  scale_colour_hue(direction = -1) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_blank()) +
  labs(subtitle = "Suicides monthly by sex (Jan 1971 - Dec 2020)",
       y = NULL,
       x = NULL)

deaths_cause |>
  filter(cause == "25 Dementia, Alzheimerin tauti (F01, F03, G30, R54)") |>
  ggplot(aes(month, deaths, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  geom_smooth(size = 2, method = "loess", formula = "y ~ x") +
  scale_colour_hue(direction = -1) +
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_blank()) +
  labs(subtitle = "Dementia, Alzheimer's deaths monthly by sex (Jan 1971 - Dec 2020)",
       y = NULL,
       x = NULL)

deaths_cause |>
  filter(cause == "42-53 Tapaturmat ja väkivalta pl. tapaturmainen alkoholimyrkytys (V01-X44, X46-Y89)") |>
  ggplot(aes(month, deaths, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  geom_smooth(size = 2, method = "loess", formula = "y ~ x") +
  scale_colour_hue(direction = -1) +
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_blank()) +
  labs(subtitle = ".... deaths monthly by sex (Jan 1971 - Dec 2020)",
       y = NULL,
       x = NULL)


# plot_annotation(title = "..",
#                 subtitle = paste0("Yearly mean for 5 year period (2016-2020) ",
#                                   "by underlying cause of death and region per 100,000 inhabitants."),
#                 caption = "source: Tilastokeskus 11bt - Deaths by underlying cause")






# Suicides ----------------------------------------------------------------

url = "https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/ksyyt/statfin_ksyyt_pxt_11by.px"
query = list("Sukupuoli" = c("1", "2"), "Tiedot"=c("*"), "Vuosi"=c("*"), "Ikä" = c("*"))
px_data <- pxweb_get(url = url, query = query)
suicides <- as_tibble(as.data.frame(px_data, column.name.type = "text", variable.value.type = "text"))

suicides <- suicides |>
  rename(age = Age, year = Year, sex = Gender, suicides = Suicides) |>
  mutate(year = as.integer(year),
         sex = as_factor(sex))

suicides |>
  filter(age == "Total") |>
  ggplot(aes(year, suicides, color = sex)) +
  geom_line(size = 1.1) +
  scale_colour_hue(direction = -1) +
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_blank()) +
  labs(subtitle = "Suicides / year (1921 - 2020)",
       y = NULL,
       x = NULL)
  

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
