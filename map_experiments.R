## Experimental county-level map visualizations for the NY SRC data.
## Renders candidate PNGs into maps/ so we can eyeball what works.

suppressMessages({
  library(tidyverse)
  library(ggplot2)
  library(maps)
  library(scales)
})

dir.create("maps", showWarnings = FALSE)

norm_county <- function(x) {
  x |>
    str_remove(" County$") |>
    str_to_lower() |>
    str_replace_all("\\.", "") |>
    str_replace_all("st ", "st ") |>   # keep "st lawrence"
    str_trim()
}

math <- read_csv("data/Annual_EM_MATH.csv", show_col_types = FALSE)
ela  <- read_csv("data/Annual_EM_ELA.csv",  show_col_types = FALSE)
abs  <- read_csv("data/Chronic_Absenteeism.csv", show_col_types = FALSE)

grades_m <- c("MATH3","MATH4","MATH5","MATH6","MATH7","MATH8")
grades_e <- c("ELA3","ELA4","ELA5","ELA6","ELA7","ELA8")

county_rows <- function(df, grades) {
  df |>
    filter(grepl(" County$", ENTITY_NAME),
           ASSESSMENT_NAME %in% grades,
           YEAR == 2024)
}

## ---- county metrics ----
math_score <- county_rows(math, grades_m) |>
  filter(SUBGROUP_NAME == "All Students") |>
  group_by(ENTITY_NAME) |>
  summarize(math_score = mean(as.numeric(MEAN_SCORE), na.rm = TRUE), .groups = "drop")

ela_score <- county_rows(ela, grades_e) |>
  filter(SUBGROUP_NAME == "All Students") |>
  group_by(ENTITY_NAME) |>
  summarize(ela_score = mean(as.numeric(MEAN_SCORE), na.rm = TRUE), .groups = "drop")

pct_ed <- county_rows(math, grades_m) |>
  filter(SUBGROUP_NAME %in% c("All Students","Economically Disadvantaged")) |>
  group_by(ENTITY_NAME, SUBGROUP_NAME) |>
  summarize(n = sum(as.numeric(TOTAL_COUNT), na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = SUBGROUP_NAME, values_from = n) |>
  mutate(pct_ed = 100 * `Economically Disadvantaged` / `All Students`) |>
  select(ENTITY_NAME, pct_ed)

absentee <- abs |>
  filter(grepl(" County$", ENTITY_NAME), YEAR == 2024, SUBGROUP_NAME == "All Students") |>
  group_by(ENTITY_NAME) |>
  summarize(absent_rate = mean(as.numeric(ABSENT_RATE), na.rm = TRUE), .groups = "drop")

county_metrics <- math_score |>
  full_join(ela_score, by = "ENTITY_NAME") |>
  full_join(pct_ed,    by = "ENTITY_NAME") |>
  full_join(absentee,  by = "ENTITY_NAME") |>
  mutate(subregion = norm_county(ENTITY_NAME))

## ---- base map ----
ny_map <- map_data("county") |> filter(region == "new york")

cat("Map counties:", n_distinct(ny_map$subregion),
    " | data counties:", n_distinct(county_metrics$subregion), "\n")
unmatched <- setdiff(ny_map$subregion, county_metrics$subregion)
cat("Counties on map with NO data match:", paste(unmatched, collapse=", "), "\n")

make_map <- function(metric, title, subtitle, legend, palette = "plasma", dir = 1,
                     labfmt = waiver()) {
  dat <- ny_map |> left_join(county_metrics, by = "subregion")
  ggplot(dat, aes(long, lat, group = group, fill = .data[[metric]])) +
    geom_polygon(color = "white", linewidth = 0.15) +
    coord_map("albers", lat0 = 40, lat1 = 45) +
    scale_fill_viridis_c(option = palette, direction = dir, name = legend,
                         labels = labfmt, na.value = "grey85") +
    labs(title = title, subtitle = subtitle,
         caption = "Source: NY State School Report Card, 2024–25. County-level rollup.") +
    theme_void(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(color = "gray40", size = 10, hjust = 0.5),
      plot.caption  = element_text(color = "gray55", size = 8, hjust = 0.5),
      legend.position = "right"
    )
}

m1 <- make_map("math_score", "Average grade 3–8 math score by county",
               "NY State, 2024–25", "Math\nscale score")
ggsave("maps/01_math_score.png", m1, width = 9, height = 7, dpi = 150, bg = "white")

m2 <- make_map("pct_ed", "Economic disadvantage is concentrated geographically",
               "% of students economically disadvantaged, 2024–25", "% disadv.",
               palette = "magma", dir = -1, labfmt = function(x) paste0(x,"%"))
ggsave("maps/02_pct_ed.png", m2, width = 9, height = 7, dpi = 150, bg = "white")

m3 <- make_map("absent_rate", "Chronic absenteeism by county",
               "Average chronic-absenteeism rate, all students, 2024", "Absent\nrate (%)",
               palette = "inferno", dir = 1, labfmt = function(x) paste0(x,"%"))
ggsave("maps/03_absentee.png", m3, width = 9, height = 7, dpi = 150, bg = "white")

## ---- bivariate-ish: residual of score given poverty, mapped ----
fit <- lm(math_score ~ pct_ed, data = county_metrics)
county_metrics$resid <- NA_real_
ok <- complete.cases(county_metrics$math_score, county_metrics$pct_ed)
county_metrics$resid[ok] <- residuals(fit)
ny_resid <- ny_map |> left_join(county_metrics, by = "subregion")
m4 <- ggplot(ny_resid, aes(long, lat, group = group, fill = resid)) +
  geom_polygon(color = "white", linewidth = 0.15) +
  coord_map("albers", lat0 = 40, lat1 = 45) +
  scale_fill_gradient2(low = "#b2182b", mid = "grey92", high = "#2166ac",
                       midpoint = 0, name = "Over/under-\nperforms",
                       na.value = "grey85") +
  labs(title = "Which counties beat (or miss) what poverty predicts?",
       subtitle = "Math score residual after adjusting for % economically disadvantaged, 2024",
       caption = "Blue = scores higher than poverty predicts; red = lower.") +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face="bold", size=14, hjust=.5),
        plot.subtitle = element_text(color="gray40", size=10, hjust=.5),
        plot.caption = element_text(color="gray55", size=8, hjust=.5))
ggsave("maps/04_residual.png", m4, width = 9, height = 7, dpi = 150, bg = "white")

cat("Done. Wrote 4 PNGs to maps/\n")
