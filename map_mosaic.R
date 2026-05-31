## Mosaic / tile cartograms (the "compromise"): regular hex & square grids,
## one equal tile per county, topology roughly preserved. Drops area & shape
## accuracy so the color channel is never distorted by region size.
suppressMessages({
  library(tidyverse); library(ggplot2); library(maps); library(sf); library(geogrid)
})
dir.create("maps", showWarnings = FALSE)

norm_county <- function(x) x |> str_remove(" County$") |> str_to_lower() |>
  str_replace_all("\\.","") |> str_trim()

math <- read_csv("data/Annual_EM_MATH.csv", show_col_types = FALSE)
grades_m <- c("MATH3","MATH4","MATH5","MATH6","MATH7","MATH8")

pct_ed <- math |>
  filter(grepl(" County$", ENTITY_NAME), ASSESSMENT_NAME %in% grades_m, YEAR==2024,
         SUBGROUP_NAME %in% c("All Students","Economically Disadvantaged")) |>
  group_by(ENTITY_NAME, SUBGROUP_NAME) |>
  summarize(n=sum(as.numeric(TOTAL_COUNT), na.rm=TRUE), .groups="drop") |>
  pivot_wider(names_from=SUBGROUP_NAME, values_from=n) |>
  mutate(pct_ed=100*`Economically Disadvantaged`/`All Students`,
         subregion=norm_county(ENTITY_NAME)) |>
  select(subregion, pct_ed)

ny_sf <- st_as_sf(map("county","new york", fill=TRUE, plot=FALSE)) |>
  mutate(subregion = str_remove(ID,"new york,")) |>
  left_join(pct_ed, by="subregion") |>
  st_transform(32618)

# abbreviations for tile labels
abbr <- function(s) {
  s |> str_to_title() |>
    str_replace("New York","Manh") |>
    str_replace("Saint","St") |>
    (\(x) ifelse(nchar(x) > 8, paste0(substr(x,1,7),"."), x))()
}

render_tiles <- function(grid_type, file, ptitle) {
  cells <- calculate_grid(shape = ny_sf, grid_type = grid_type, seed = 1)
  res   <- assign_polygons(ny_sf, cells) |>
    mutate(lab = abbr(subregion))
  ctr <- res |> st_centroid() |> st_coordinates() |> as_tibble()
  res$cx <- ctr$X; res$cy <- ctr$Y
  ggplot(res) +
    geom_sf(aes(fill = pct_ed), color = "white", linewidth = 0.4) +
    geom_text(aes(cx, cy, label = lab), size = 2.1, color = "grey15") +
    scale_fill_viridis_c(option="magma", direction=-1, name="% disadv.",
                         labels=function(x) paste0(x,"%")) +
    labs(title = ptitle,
         subtitle = "Every county = one equal tile. Color carries the data; area carries nothing.",
         caption = "Mosaic/tile cartogram (geogrid). Topology preserved; area & shape dropped.") +
    theme_void(base_size=13) +
    theme(plot.title=element_text(face="bold", size=14, hjust=.5),
          plot.subtitle=element_text(color="gray40", size=10, hjust=.5),
          plot.caption=element_text(color="gray55", size=8, hjust=.5))
}

ggsave("maps/10_mosaic_hex.png",
       render_tiles("hexagonal","maps/10_mosaic_hex.png",
                    "Hex-tile cartogram: economic disadvantage by county"),
       width=9, height=8, dpi=150, bg="white")
ggsave("maps/11_mosaic_square.png",
       render_tiles("regular","maps/11_mosaic_square.png",
                    "Square-tile (mosaic) cartogram: economic disadvantage by county"),
       width=9, height=8, dpi=150, bg="white")
cat("Wrote maps/10_mosaic_hex.png and maps/11_mosaic_square.png\n")
