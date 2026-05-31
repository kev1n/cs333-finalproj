## NYC-inset choropleth: statewide map + zoomed five-borough panel.
suppressMessages({
  library(tidyverse); library(ggplot2); library(maps); library(scales); library(patchwork)
})
dir.create("maps", showWarnings = FALSE)

norm_county <- function(x) x |> str_remove(" County$") |> str_to_lower() |> str_replace_all("\\.","") |> str_trim()

math <- read_csv("data/Annual_EM_MATH.csv", show_col_types = FALSE)
grades_m <- c("MATH3","MATH4","MATH5","MATH6","MATH7","MATH8")

pct_ed <- math |>
  filter(grepl(" County$", ENTITY_NAME), ASSESSMENT_NAME %in% grades_m, YEAR == 2024,
         SUBGROUP_NAME %in% c("All Students","Economically Disadvantaged")) |>
  group_by(ENTITY_NAME, SUBGROUP_NAME) |>
  summarize(n = sum(as.numeric(TOTAL_COUNT), na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = SUBGROUP_NAME, values_from = n) |>
  mutate(pct_ed = 100 * `Economically Disadvantaged` / `All Students`,
         subregion = norm_county(ENTITY_NAME)) |>
  select(subregion, pct_ed)

ny_map  <- map_data("county") |> filter(region == "new york") |> left_join(pct_ed, by = "subregion")
nyc_subs <- c("bronx","kings","new york","queens","richmond")
nyc_map <- ny_map |> filter(subregion %in% nyc_subs)

fill_scale <- scale_fill_viridis_c(option = "magma", direction = -1, name = "% disadv.",
                                    labels = function(x) paste0(x,"%"), na.value = "grey85",
                                    limits = range(ny_map$pct_ed, na.rm = TRUE))

main <- ggplot(ny_map, aes(long, lat, group = group, fill = pct_ed)) +
  geom_polygon(color = "white", linewidth = 0.15) +
  annotate("rect", xmin = -74.30, xmax = -73.68, ymin = 40.48, ymax = 40.94,
           fill = NA, color = "grey20", linewidth = 0.5) +
  coord_map("albers", lat0 = 40, lat1 = 45) + fill_scale +
  labs(title = "Economic disadvantage is geographically concentrated",
       subtitle = "% economically disadvantaged by county, 2024–25  (NYC detail at right)") +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face="bold", size=14),
        plot.subtitle = element_text(color="gray40", size=10), legend.position = "left")

inset <- ggplot(nyc_map, aes(long, lat, group = group, fill = pct_ed)) +
  geom_polygon(color = "white", linewidth = 0.3) +
  coord_map("albers", lat0 = 40, lat1 = 45) + fill_scale +
  labs(title = "New York City") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face="bold", size=11, hjust=.5), legend.position = "none")

combined <- main + inset + plot_layout(widths = c(2.4, 1))
ggsave("maps/05_nyc_inset.png", combined, width = 12, height = 6.5, dpi = 150, bg = "white")
cat("Wrote maps/05_nyc_inset.png\n")
