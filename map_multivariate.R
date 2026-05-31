## Non-map multivariate views (SPLOM, parallel coords) + a Dorling cartogram
## that decouples the color channel from region area.
suppressMessages({
  library(tidyverse); library(ggplot2); library(maps); library(scales)
  library(GGally); library(packcircles)
})
dir.create("maps", showWarnings = FALSE)

math   <- read_csv("data/Annual_EM_MATH.csv", show_col_types = FALSE)
exp    <- read_csv("data/Expenditures_per_Pupil.csv", show_col_types = FALSE)
absn   <- read_csv("data/Chronic_Absenteeism.csv", show_col_types = FALSE)
inexp  <- read_csv("data/Inexperienced_Teachers.csv", show_col_types = FALSE)
ooc    <- read_csv("data/Out_of_Cert_Teachers.csv", show_col_types = FALSE)
grades_m <- c("MATH3","MATH4","MATH5","MATH6","MATH7","MATH8")
is_dist <- function(x) grepl("0000$", x) & !grepl("^0{6}", x)

## ---------- DISTRICT-LEVEL multivariate table ----------
d_score <- math |>
  filter(ASSESSMENT_NAME %in% grades_m, SUBGROUP_NAME=="All Students",
         YEAR==2024, is_dist(ENTITY_CD)) |>
  group_by(ENTITY_CD) |>
  summarize(math_score = mean(as.numeric(MEAN_SCORE), na.rm=TRUE), .groups="drop")

d_ed <- math |>
  filter(ASSESSMENT_NAME %in% grades_m,
         SUBGROUP_NAME %in% c("All Students","Economically Disadvantaged"),
         YEAR==2024, is_dist(ENTITY_CD)) |>
  group_by(ENTITY_CD, SUBGROUP_NAME) |>
  summarize(n=sum(as.numeric(TOTAL_COUNT), na.rm=TRUE), .groups="drop") |>
  pivot_wider(names_from=SUBGROUP_NAME, values_from=n) |>
  mutate(pct_ed = 100*`Economically Disadvantaged`/`All Students`) |>
  select(ENTITY_CD, pct_ed)

d_money <- exp |>
  filter(YEAR==2024, is_dist(ENTITY_CD), PUPIL_COUNT_TOT>=100) |>
  transmute(ENTITY_CD, spending = PER_FED_STATE_LOCAL_EXP)

d_abs <- absn |>
  filter(YEAR==2024, is_dist(ENTITY_CD), SUBGROUP_NAME=="All Students") |>
  group_by(ENTITY_CD) |>
  summarize(absent_rate = mean(as.numeric(ABSENT_RATE), na.rm=TRUE), .groups="drop")

d_inexp <- inexp |> filter(YEAR==2024, is_dist(ENTITY_CD)) |>
  transmute(ENTITY_CD, inexp_teach = as.numeric(PER_TEACH_INEXP))
d_ooc   <- ooc |> filter(YEAR==2024, is_dist(ENTITY_CD)) |>
  transmute(ENTITY_CD, out_cert = as.numeric(PER_OUT_CERT))

dist <- d_score |>
  inner_join(d_ed, by="ENTITY_CD") |>
  inner_join(d_money, by="ENTITY_CD") |>
  left_join(d_abs, by="ENTITY_CD") |>
  left_join(d_inexp, by="ENTITY_CD") |>
  left_join(d_ooc, by="ENTITY_CD") |>
  filter(is.finite(math_score), is.finite(pct_ed), is.finite(spending)) |>
  mutate(need = cut(pct_ed, quantile(pct_ed, 0:4/4, na.rm=TRUE),
                    labels=c("Low need","Mid-low","Mid-high","High need"),
                    include.lowest=TRUE))

cat("districts in multivariate table:", nrow(dist), "\n")

splom_vars <- dist |>
  transmute(`Math score`=math_score, `% disadv`=pct_ed,
            `Spend/pupil`=spending/1000, `Absent %`=absent_rate,
            `Inexp tch %`=inexp_teach, need)

## ---------- 1. SPLOM ----------
p_splom <- ggpairs(
  splom_vars, columns=1:5, aes(color=need, alpha=0.5),
  upper=list(continuous=wrap("cor", size=2.8)),
  lower=list(continuous=wrap("points", size=0.5, alpha=0.4)),
  diag =list(continuous=wrap("densityDiag", alpha=0.5))
) +
  scale_color_viridis_d(option="plasma", end=0.9) +
  scale_fill_viridis_d(option="plasma", end=0.9) +
  labs(title="Scatterplot matrix: the correlation map a choropleth can't give you",
       subtitle="NY school districts, 2024-25. Color = poverty quartile.") +
  theme_minimal(base_size=9) +
  theme(plot.title=element_text(face="bold", size=12),
        plot.subtitle=element_text(color="gray40", size=9),
        strip.text=element_text(size=7.5))
ggsave("maps/07_splom.png", p_splom, width=10, height=9, dpi=150, bg="white")

## ---------- 2. Parallel coordinates ----------
pc <- dist |>
  filter(is.finite(absent_rate), is.finite(inexp_teach)) |>
  select(need, `Math\nscore`=math_score, `%\ndisadv`=pct_ed,
         `Spend/\npupil`=spending, `Absent\n%`=absent_rate,
         `Inexp\ntch %`=inexp_teach)
p_par <- ggparcoord(pc, columns=2:6, groupColumn=1, scale="uniminmax",
                    alphaLines=0.18, showPoints=FALSE) +
  scale_color_viridis_d(option="plasma", end=0.9, name="Need\nquartile") +
  labs(title="Parallel coordinates: does disadvantage travel together?",
       subtitle="Each line is a district (min-max scaled). High-need districts in yellow.",
       x=NULL, y="Scaled value (0 = lowest, 1 = highest)") +
  theme_minimal(base_size=11) +
  theme(plot.title=element_text(face="bold", size=13),
        plot.subtitle=element_text(color="gray40", size=10),
        panel.grid.major.x=element_line(color="grey80"))
ggsave("maps/08_parcoord.png", p_par, width=10, height=6, dpi=150, bg="white")

## ---------- 3. Dorling cartogram (circles, area = enrollment) ----------
norm_county <- function(x) x |> str_remove(" County$") |> str_to_lower() |>
  str_replace_all("\\.","") |> str_trim()
cty <- math |>
  filter(grepl(" County$", ENTITY_NAME), ASSESSMENT_NAME %in% grades_m, YEAR==2024,
         SUBGROUP_NAME %in% c("All Students","Economically Disadvantaged")) |>
  group_by(ENTITY_NAME, SUBGROUP_NAME) |>
  summarize(n=sum(as.numeric(TOTAL_COUNT), na.rm=TRUE), .groups="drop") |>
  pivot_wider(names_from=SUBGROUP_NAME, values_from=n) |>
  mutate(enroll=`All Students`, pct_ed=100*`Economically Disadvantaged`/`All Students`,
         subregion=norm_county(ENTITY_NAME)) |>
  select(subregion, enroll, pct_ed)

centroids <- map_data("county") |> filter(region=="new york") |>
  group_by(subregion) |>
  summarize(x=mean(range(long)), y=mean(range(lat)), .groups="drop") |>
  inner_join(cty, by="subregion") |>
  filter(is.finite(enroll), enroll>0)

# initial radius proportional to sqrt(enrollment); repel to remove overlaps
centroids$r <- sqrt(centroids$enroll); centroids$r <- centroids$r/max(centroids$r)*0.55
layout <- circleRepelLayout(centroids[,c("x","y","r")],
                            xysizecols=1:3, sizetype="radius", maxiter=2000)$layout
centroids$x2 <- layout$x; centroids$y2 <- layout$y; centroids$r2 <- layout$radius
verts <- circleLayoutVertices(layout, npoints=60, sizetype="radius") |>
  left_join(centroids |> mutate(id=row_number()) |> select(id, pct_ed, subregion), by="id")

p_dor <- ggplot(verts, aes(x, y, group=id, fill=pct_ed)) +
  geom_polygon(color="white", linewidth=0.2) +
  coord_equal() +
  scale_fill_viridis_c(option="magma", direction=-1, name="% disadv.",
                       labels=function(x) paste0(x,"%")) +
  labs(title="Dorling cartogram: circle area = student enrollment",
       subtitle="Color = % economically disadvantaged, 2024-25. Area no longer distorts color.",
       caption="Non-contiguous; circles repelled from county centroids. Big circle bottom-right = NYC.") +
  theme_void(base_size=13) +
  theme(plot.title=element_text(face="bold", size=14, hjust=.5),
        plot.subtitle=element_text(color="gray40", size=10, hjust=.5),
        plot.caption=element_text(color="gray55", size=8, hjust=.5))
ggsave("maps/09_dorling.png", p_dor, width=9, height=8, dpi=150, bg="white")

cat("Wrote 07_splom.png, 08_parcoord.png, 09_dorling.png\n")
