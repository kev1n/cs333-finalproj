## Enrollment-weighted county cartogram: each county's AREA scales to its
## student count, so visual weight matches where students actually are.
suppressMessages({
  library(tidyverse); library(ggplot2); library(maps); library(scales)
  library(sf); library(cartogram)
})
dir.create("maps", showWarnings = FALSE)

norm_county <- function(x) x |> str_remove(" County$") |> str_to_lower() |> str_replace_all("\\.","") |> str_trim()

math <- read_csv("data/Annual_EM_MATH.csv", show_col_types = FALSE)
grades_m <- c("MATH3","MATH4","MATH5","MATH6","MATH7","MATH8")

cty <- math |>
  filter(grepl(" County$", ENTITY_NAME), ASSESSMENT_NAME %in% grades_m, YEAR == 2024,
         SUBGROUP_NAME %in% c("All Students","Economically Disadvantaged")) |>
  group_by(ENTITY_NAME, SUBGROUP_NAME) |>
  summarize(n = sum(as.numeric(TOTAL_COUNT), na.rm = TRUE),
            score = mean(as.numeric(MEAN_SCORE), na.rm = TRUE), .groups = "drop")

metrics <- cty |>
  select(ENTITY_NAME, SUBGROUP_NAME, n) |>
  pivot_wider(names_from = SUBGROUP_NAME, values_from = n) |>
  left_join(cty |> filter(SUBGROUP_NAME=="All Students") |> select(ENTITY_NAME, score),
            by = "ENTITY_NAME") |>
  mutate(enroll = `All Students`,
         pct_ed = 100 * `Economically Disadvantaged` / `All Students`,
         subregion = norm_county(ENTITY_NAME)) |>
  select(subregion, enroll, pct_ed, score)

## county polygons -> sf, projected to NY State Plane-ish (UTM 18N) for cartogram
ny_sf <- st_as_sf(map("county", "new york", fill = TRUE, plot = FALSE)) |>
  mutate(subregion = str_remove(ID, "new york,")) |>
  left_join(metrics, by = "subregion") |>
  filter(!is.na(enroll)) |>
  st_transform(32618)

carto <- cartogram_cont(ny_sf, weight = "enroll", itermax = 12)

p <- ggplot(carto) +
  geom_sf(aes(fill = pct_ed), color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "% disadv.",
                       labels = function(x) paste0(x,"%")) +
  labs(title = "Where NY's students actually are: county size = enrollment",
       subtitle = "County area scaled to student count; color = % economically disadvantaged, 2024–25",
       caption = "Cartogram (continuous). NYC swells; sparse upstate counties shrink.") +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face="bold", size=14, hjust=.5),
        plot.subtitle = element_text(color="gray40", size=10, hjust=.5),
        plot.caption = element_text(color="gray55", size=8, hjust=.5))
ggsave("maps/06_cartogram.png", p, width = 9, height = 7.5, dpi = 150, bg = "white")
cat("Wrote maps/06_cartogram.png\n")
