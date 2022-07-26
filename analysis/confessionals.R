
dev.off(dev.list()["RStudioGD"])

library(ggsankey)
library(brms)
library(ggnewscale)
library(ggchicklet)
library(ggstream)

# confessional showcase ---------------------------------------------------


# 🎨 fonts and palettes ------------------------------------------------------

bg <- "grey20"
txt_col <- "white"

pal <- c(my_pink[3], darken(my_pink[1], 0.5), darken(my_pink[5], 0.5), my_pink[1], my_pink[2], my_pink[5], my_pink[4])
names(pal) <- c("Total", "Male", "Female", "White Male", "POC Male", "White Female", "POC Female")
pal <- colorRampPalette(blue_orange)(7)
pal[4] <- colorRampPalette(pal[c(1,7)])(3)[2]
pal[4] <- bg
names(pal) <- c("Male", "White Male", "POC Male", "Total", "POC Female", "White Female", "Female")

ft_text <- "barlow"

# 💽 data wrangling ----------------------------------------------------------

df_base <- survivoR::confessionals |>
  filter(version == "US") |>
  left_join(
    survivoR::castaway_details |>
      distinct(castaway_id, gender, poc),
    by = "castaway_id"
  ) |>
  mutate(gender = ifelse(gender == "Non-binary", "Female", gender))


df_total <- df_base |>
  group_by(castaway_id, gender, poc) |>
  summarise(n = sum(confessional_count)) |>
  mutate(
    gender_poc = paste(poc, gender),
    gender_poc = factor(gender_poc, levels = c("POC Female", "White Female", "POC Male", "White Male"))
  )

df_index <- make_edit_index()

# 🎈 functions ------------------------------------------------------------

custom_names <- function(x) {
  case_when(
    x == "Rob Mariano" ~ "Boston Rob Mariano",
    x == "Benjamin Wade" ~ "Benjamin 'Coach' Wade",
    x == "Oscar Lusth" ~ "Oscar 'Ozzy' Lusth",
    x == "Jairus Robinson" ~ "Jairus 'J.D.' Robinson",
    TRUE ~ x
  )
}

# 🐍 sankey ------------------------------------------------------------------

g_sankey <- df_base |>
  mutate(
    total = "Total",
    gender_poc = paste(poc, gender)
  ) |>
  make_long(gender_poc, gender, total, value = confessional_count) |>
  group_by(x, node, next_x, next_node) |>
  summarise(value = sum(value)) |>
  ggplot(aes(x = x,
             next_x = next_x,
             node = node,
             next_node = next_node,
             fill = factor(node),
             value = value)
  ) +
  geom_sankey(flow.alpha = 0.75, node.color = 1) +
  scale_fill_manual(
    values = pal,
    breaks = names(pal)
  ) +
  geom_sankey_label(aes(label = node), size = 3.5, color = 1, fill = "white") +
  coord_flip() +
  theme_void() +
  theme(
    text = element_text(family = "jose", colour = "white", size = 48, lineheight = 0.3),
    plot.title = element_text(hjust = 0, lineheight = 0.3),
    plot.background = element_rect(fill = "grey20", colour = "grey20"),
    plot.margin = margin(t = 30, l = 30, r = 30, b = 30),
    legend.position = "none"
  )

# 👗 model ----------------------------------------------------------------

if(FALSE) {
  df_confessionals <- make_edit_index() |>
    left_join(
      survivoR::castaway_details |>
        select(gender, poc, castaway_id),
      by = "castaway_id"
    ) |>
    mutate(gender = ifelse(gender == "Non-binary", "Female", gender))

  new_data <- expand_grid(gender = c("Male", "Female"), poc = c("White", "POC"))
  modb <- brm(edit_index ~ gender + poc, data = df_confessionals)
  pred <- posterior_epred(modb, newdata = new_data)

  cols <- new_data |>
    mutate(names = paste(poc, gender)) |>
    pull(names)
}

g_stream <- df_confessionals |>
  mutate(
    demographic = paste(poc, gender),
    demographic = factor(demographic, levels = c("POC Female", "White Female", "POC Male", "White Male"))
  ) |>
  count(edit_pct, demographic) |>
  ggplot() +
  # geom_histogram(aes(edit_index, fill = demographic)) +
  geom_stream(aes(x = edit_pct, n, fill = demographic), bw = 0.5, type = "proportional") +
  scale_x_continuous(
    breaks = c(-50, 0, 50, 100, 150),
    labels = paste0(c(-50, 0, 50, 100, 150), "%"),
  ) +
  scale_fill_manual(
    values = pal,
    breaks = names(pal),
    na.value = bg
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft_text, colour = txt_col, size = 40, lineheight = 0.3),
    axis.text.x = element_text(lineheight = 0.3, margin = margin(t = 5), size = 40),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(t = 0, l = 0, r = 0, b = 0),
    legend.position = "none"
  )

df_pred <- pred |>
  as_tibble() |>
  head(2000) |>
  set_names(cols) |>
  pivot_longer(everything(), names_to = "demographic", values_to = "draw") |>
  mutate(demographic = as.factor(demographic))

df_med_line <- df_pred |>
  group_by(demographic) |>
  summarise(median = median(draw))

model_text <- "There is a clear difference in the mean edit percentage for men and\n
women where men tend to get a +10% more favourable edit than women.\n
The mean edit percentage is equivalent for race within each gender."

g_pred <- df_pred |>
  ggplot(aes(demographic, draw, colour = demographic, fill = demographic)) +
  geom_hline(yintercept = 1, colour = txt_col, linetype = 3) +
  geom_jitter(width = 0.2,  alpha = 0.05) +
  geom_segment(aes(y = 1, yend = median, x = demographic, xend = demographic), df_med_line, colour = txt_col) +
  geom_point(aes(x = demographic, y = median), df_med_line, size = 10, colour = txt_col) +
  geom_point(aes(x = demographic, y = median, colour = demographic), df_med_line, size = 9) +
  scale_y_continuous(
    breaks = seq(0.8, 1.2, 0.1),
    labels = c("-20%", "-10%", "0%", "+10%", "+20%"),
    position = "right"
  ) +
  scale_x_discrete(
    breaks = c("White Male", "White Female", "POC Male", "POC Female"),
    labels = c("Male\n(White)", "Female\n(White)", "Male\n(POC)", "Female\n(POC)"),
    position = "top"
  ) +
  coord_flip() +
  labs(
    # subtitle = model_text
    y = "Edit (%)"
  ) +
  scale_fill_manual(
    values = pal,
    breaks = names(pal),
    na.value = bg
  ) +
  scale_colour_manual(
    values = pal,
    breaks = names(pal)
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft_text, colour = "white", size = 40, lineheight = 0.3),
    axis.title.x = element_text(lineheight = 0.3, margin = margin(t = 5)),
    axis.text = element_text(lineheight = 0.3, margin = margin(t = 5)),
    axis.ticks = element_line(colour = "grey20"),
    plot.title = element_text(hjust = 0, lineheight = 0.3),
    plot.subtitle = element_text(hjust = 0, lineheight = 0.16, size = 48, margin = margin(b = 20)),
    plot.background = element_rect(fill = "grey20", colour = bg),
    plot.margin = margin(t = 30, l = 30, r = 30, b = 30),
    legend.position = "none"
  )


# ⚾ make base -------------------------------------------------------------

title <- "SURVIVOR: CONFESSIONALS"
subtitle <- str_wrap(
  "Confessional counts loosely represent a players screen time where they talk strategy
and replay events. It is an imperfect measure, but can indicate success in the game.
This is a detailed summary of confessionals<br>and an analysis of the edit for key demographics", 180)
g_base <- ggbase(title, subtitle, size = 92, x = 0, x_sub = 0, y_sub = 23.2, size_sub = 20,
                 ft_title = "survivor", ft_sub = ft_text, title_col = lighten(pal["POC Female"], 0.5))


# 📦 total and edit boxes --------------------------------------------------

df_total_30 <- df_total |>
  left_join(df_index, by = "castaway_id") |>
  left_join(select(survivoR::castaway_details, castaway_id, full_name), by = "castaway_id") |>
  # group_by(gender) |>
  ungroup() |>
  slice_max(total, n = 40, with_ties = FALSE) |>
  arrange(total) |>
  mutate(
    x = 0,
    y = 1:40
    # y = ifelse(gender == "Female", y + 20, y),
  ) |>
  ungroup() |>
  mutate(
    total_scaled = min_max(total, 0.2, 1),
    full_name = custom_names(full_name)
  )

df_edit_30 <- df_total |>
  left_join(df_index, by = "castaway_id") |>
  left_join(select(survivoR::castaway_details, castaway_id, full_name), by = "castaway_id") |>
  ungroup() |>
  slice_max(edit_pct, n = 40, with_ties = FALSE) |>
  arrange(edit_pct) |>
  mutate(
    x = 0,
    y = 1:40
  ) |>
  ungroup() |>
  mutate(
    edit_pct_scaled = min_max(edit_pct, 0.2, 1),
    edit_pct = paste0(ifelse(edit_pct > 0, "+", ""), edit_pct, "%"),
    full_name = custom_names(full_name)
  )

df_bump <- df_edit_30 |>
  select(castaway_id, x, y, gender_poc) |>
  mutate(x = 2) |>
  bind_rows(
    df_total_30 |>
      select(castaway_id, x, y, gender_poc) |>
      mutate(x = 1.15)
  ) |>
  semi_join(
    df_edit_30 |>
      semi_join(df_total_30, by = "castaway_id"),
    by = "castaway_id"
  )

a <- 2
b <- 1.15
c <- 0.1
g_top_30 <-df_total_30 |>
  ggplot(aes(fill = gender_poc)) +
  # ggplot() +

  geom_rrect(aes(xmin = 0, xmax = total_scaled, ymin = y-0.45, ymax = y+0.45)) +
  geom_text(aes(x = 0.02, y = y, label = full_name), family = ft_text, colour = txt_col, size = 12, hjust = 0) +

  geom_rrect(aes(xmin = a, xmax = edit_pct_scaled+a, ymin = y-0.45, ymax = y+0.45), df_edit_30) +
  geom_text(aes(x = 0.02+a, y = y, label = full_name), df_edit_30, family = ft_text, size = 12, hjust = 0, colour = txt_col) +

  geom_bump(aes(x, y, colour = gender_poc, group = castaway_id), df_bump) +

  scale_fill_manual(
    values = pal,
    breaks = names(pal),
    na.value = bg
  ) +
  scale_colour_manual(
    values = pal,
    breaks = names(pal)
  ) +

  new_scale_fill() +
  new_scale_colour() +

  # edit
  geom_rrect(aes(xmin = b-c, xmax = b + c, ymin = y-0.45, ymax = y+0.45, fill = total)) +
  geom_text(aes(x = b, y = y, label = total, colour = gender_poc), size = 12, family = ft_text, colour = bg) +

  scale_fill_gradientn(colours = prgr_div[8:12]) +
  scale_colour_gradientn(colours = prgr_div[8:12]) +

  new_scale_fill() +
  new_scale_colour() +

  geom_rrect(aes(xmin = b+a-c, xmax = b+a + c, ymin = y-0.45, ymax = y+0.45, fill = edit_index), df_edit_30) +
  geom_text(aes(x = b+a, y = y, label = edit_pct), df_edit_30, colour = bg, size = 12, family = ft_text) +

  annotate("text", x = 0, y = 41, label = "Total confessional count", family = ft_text, size = 16, colour = txt_col, hjust = 0) +
  annotate("text", x = a, y = 41, label = "Edit (%)", family = ft_text, size = 16, colour = txt_col, hjust = 0) +

  scale_fill_gradientn(colours = prgr_div[8:12]) +
  scale_colour_gradientn(colours = prgr_div[8:12]) +

  scale_y_continuous(
    breaks = 1:40,
    labels = 40:1
  ) +
  labs(
    subtitle = "Top 40 castaways ranked by their total number of confessionals and how favorable\n
their edits were over their seasons. The edit is measured as a percentage e.g. +50%\n
means they recieved 50% more confessionals than expected (over-edit) standardised for\n
the number of episodes. Early boots are adjusted for low data"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft_text, colour = "white", size = 48, lineheight = 0.3),
    axis.text.y = element_text(lineheight = 0.3, margin = margin(t = 5)),
    axis.ticks = element_line(colour = "grey20"),
    # plot.title = element_text(hjust = 0, lineheight = 0.3),
    plot.subtitle = element_text(hjust = 0, lineheight = 0.16),
    plot.background = element_rect(fill = "grey20", colour = bg),
    plot.margin = margin(t = 30, l = 30, r = 30, b = 30),
    legend.position = "none"
  )

# 👉 points --------------------------------------------------------------

df_points <- df_total |>
  left_join(df_index, by = "castaway_id") |>
  mutate(n = edit_pct)

g_points <- df_points |>
  ggplot(aes(total, edit_index, colour = gender_poc)) +
  geom_point(size = 5, alpha = 0.3) +
  scale_fill_manual(
    values = pal,
    breaks = names(pal),
    na.value = bg
  ) +
  scale_colour_manual(
    values = pal,
    breaks = names(pal)
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft_text, colour = "white", size = 48, lineheight = 0.3),
    axis.text = element_text(lineheight = 0.3, margin = margin(t = 5)),
    axis.ticks = element_line(colour = "grey20"),
    plot.title = element_text(hjust = 0, lineheight = 0.3),
    plot.background = element_rect(fill = "grey20", colour = bg),
    plot.margin = margin(t = 30, l = 30, r = 30, b = 30),
    legend.position = "none"
  )


# 💭 bubble chart ---------------------------------------------------------

title_text_bubble <- str_rich_wrap(glue(
  "Over 42 seasons of US Survivor there has been a balance of male and
  female castaways, however men have had 2.7k more
  confessionals than women.
  There have been 26 more {cg('women of colour', pal[5])} than {cg('men of colour', pal[3])} but
  each cohort has the same number of confessionals"),
  60
)

df_bubble <- list(
  gender_poc = df_base |>
    group_by(gender, poc) |>
    summarise(n = sum(confessional_count)) |>
    mutate(
      r = sqrt(n/pi),
      gender_poc = paste(poc, gender),
      gender_poc = factor(gender_poc, levels = c("POC Female", "White Female", "POC Male", "White Male"))
    ),

  gender = df_base |>
    group_by(gender_poc = gender) |>
    summarise(n = sum(confessional_count)) |>
    mutate(r = sqrt(n/pi)),

  all = df_base |>
    summarise(n = sum(confessional_count)) |>
    mutate(r = sqrt(n/pi))
)

df_bump1 <- tribble(
  ~id, ~x, ~y,
  1, 5, 9276.5,
  1, 2.5, 7920/2,
  2, 5, 9276.5,
  2, 2.5, 7920 + 10633/2,
  3, 2.5, 7920/2,
  3, 0, 2347/2,
  4, 2.5, 7920/2,
  4, 0, 2347 + 5573/2,
  5, 2.5, 7920 + 10633/2,
  5, 0, 2347 + 5573 + 2344/2,
  6, 2.5, 7920 + 10633/2,
  6, 0, 2347 + 5573 + 2344 + 8289/2
)

df_bump2 <- tribble(
  ~id, ~x, ~y,
  1, 5, 7920/2,
  1, 2.5, 7920/2,
  2, 5, 7920 + 10633/2,
  2, 2.5, 7920 + 10633/2,
  3, 2.5, 2347/2,
  3, 0, 2347/2,
  4, 2.5, 2347 + 5573/2,
  4, 0, 2347 + 5573/2,
  5, 2.5, 2347 + 5573 + 2344/2,
  5, 0, 2347 + 5573 + 2344/2,
  6, 2.5, 2347 + 5573 + 2344 + 8289/2,
  6, 0, 2347 + 5573 + 2344 + 8289/2
)

df_tree_text <- tribble(
  ~id, ~x, ~y, ~label,
  1, 5, 9276.5, "Total\n18.5k confessionals\n626 castaways",
  1, 2.5, 7920/2, "Female\n7.9k confessionals\n313 castaways",
  2, 2.5, 7920 + 10633/2, "Male\n10.6k confessionals\n313 castaways",
  3, -1, 2347/2, "Female (POC)\n2.3k confessionals\n106 castaways",
  4, 0, 2347 + 5573/2, "Female (White)\n5.6k confessionals\n207 castaways",
  5, -1, 2347 + 5573 + 2344/2, "Male (POC)\n2.3k confessionals\n80 castaways",
  6, 0, 2347 + 5573 + 2344 + 8289/2, "Male (White)\n8.3k confessionals\n233 castaways"
)


d <- 0.5
g_tree <- ggplot() +
  geom_line(aes(x, y, group = id), df_bump2, linetype = 3, colour = txt_col) +
  geom_chicklet(aes(x = 5, y = n), df_bubble[["all"]], fill = "grey30", colour = txt_col, radius = grid::unit(8, "pt")) +
  geom_chicklet(aes(x = 2.5, y = n), df_bubble[["gender"]], radius = grid::unit(8, "pt"), fill = "grey40") +
  geom_chicklet(aes(x = 0, y = n, fill = gender_poc), df_bubble[["gender_poc"]], radius = grid::unit(8, "pt")) +
  geom_text(aes(x, y, label = label), df_tree_text, family = ft_text, size = 14, lineheight = 0.3, colour = txt_col) +
  annotate("point", x = -0.45, y = 2347/2, colour = txt_col, size = 4) +
  annotate("point", x = -0.45, y = 2347 + 5573 + 2344/2, colour = txt_col, size = 4) +
  annotate("richtext", x = 6.6, y = 0, label = title_text_bubble, family = ft_text, size = 16,
           colour = txt_col, label.color = NA, fill = NA, lineheight = 0.35, hjust = 0) +
  scale_fill_manual(
    values = pal,
    breaks = names(pal),
    na.value = bg
  ) +
  scale_colour_manual(
    values = pal,
    breaks = names(pal)
  ) +
  coord_flip(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft_text, colour = "white", size = 48, lineheight = 0.3),
    axis.ticks = element_line(colour = "grey20"),
    plot.title = element_text(hjust = 0, lineheight = 0.3),
    plot.background = element_rect(fill = "grey20", colour = bg),
    plot.margin = margin(t = 30, l = 30, r = 30, b = 30),
    legend.position = "none"
  )


# 📊 histogram ------------------------------------------------------------

hist_text <-
  "Distribution of confessionals for each castaway.\n
  • 82% have less than 50 confessionals\n
  • The majority of castaways with more than 50\n
    confessionals played multiple seasons (70%)\n\n
Who has the most confessionals?
"

g_hist <- df_total |>
  ggplot(aes(n, fill = gender_poc)) +
  geom_histogram(bins = 45, colour = txt_col, size = 0.1) +
  annotate("text", x = 70, y = 130, label = hist_text, family = ft_text, colour = txt_col, lineheight = 0.16,
           size = 16, hjust = 0, vjust = 1) +
  scale_fill_manual(
    values = pal,
    breaks = names(pal),
    na.value = bg
  ) +
  scale_colour_manual(
    values = pal,
    breaks = names(pal)
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft_text, colour = "white", size = 48, lineheight = 0.3),
    axis.text.x = element_text(lineheight = 0.3, margin = margin(t = 5)),
    # axis.title.y = element_text(lineheight = 0.4, angle = 90),
    axis.ticks = element_line(colour = "grey20"),
    plot.title = element_text(hjust = 0, lineheight = 0.3),
    plot.background = element_rect(fill = "grey20", colour = bg),
    plot.margin = margin(t = 30, l = 30, r = 30, b = 30),
    legend.position = "none"
  )


# 🅰️ text boxes ----------------------------------------------------------

top_right <-
  "Takeaways:\n
    • Boston Rob has the most confessionals\n
    • Devens has the most favourable edit\n
    • Hantz has the second most confessionals but also the second most\n
      favourable edit\n
    • Only 7 people appear in both the top 40 total confessionals and top\n
      40 edits\n\n
Accounting for every castaway that has played the game, is there a measurable\n
difference in the edit for different demograhics?\n"

top_right_text <- ggplot() +
  annotate("text", x = 0, y = 1, label = top_right, family = ft_text, size = 17, colour = txt_col,
           hjust = 0, lineheight = 0.16, vjust = 1) +
  xlim(0, 1) +
  ylim(0.5, 1) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = bg)
  )

bottom_right <- "
Results:\n
  • Men tend to get a +8% more favourable edit\n
  • Women tend to be under-edited by -10%\n
  • The mean edit percentage is equivalent for race within each gender\n\n
Estimates are from a Bayesian model using {brms}. Bands represent the\n
posterior distribution of the means"

bottom_right_text <- ggplot() +
  annotate("text", x = 0, y = 1, label = bottom_right, family = ft_text, size = 17, colour = txt_col,
           hjust = 0, lineheight = 0.16, vjust = 1) +
  xlim(0, 1) +
  ylim(0.5, 1) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = bg)
  )

# 📐 triangle ----------------------------------------------------------------

g_triangle <- tribble(
  ~x, ~y,
  0, 0,
  0, 1,
  0.83, 0.5,
  0, 0
) |>
  ggplot(aes(x, y)) +
  geom_polygon(fill = "grey30") +
  theme_void() +
  theme(plot.background = element_rect(fill = bg, colour = bg))

# 🦴 join -----------------------------------------------------------------

g_base +
  inset_element(g_tree, bottom = 0.24, top = 0.875, left = 0.03, right = 0.28) +
  inset_element(g_top_30, bottom = -0.07, top = 0.9, left = 0.3, right = 0.66) +
  inset_element(g_pred, bottom = 0.3, top = 0.685, left = 0.695, right = 0.95) +
  inset_element(g_stream, bottom = 0.2, top = 0.28, left = 0.695, right = 0.95) +
  inset_element(top_right_text, bottom = 0.7, top = 0.87, left = 0.7, right = 0.95) +
  inset_element(g_triangle, bottom = 0.78, top = 0.87, left = 0.66, right = 0.69) +
  inset_element(g_hist, bottom = -0.03, top = 0.24, left = 0.03, right = 0.28) +
  inset_element(g_triangle, bottom = 0.12, top = 0.21, left = 0.27, right = 0.30) +
  inset_element(bottom_right_text, bottom = -0.02, top = 0.18, left = 0.695, right = 0.95) +
  plot_annotation(
    caption = glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Data: available from the survivoR package on Github {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/survivoR #rstats"),
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg),
      plot.caption = element_markdown(size = 42, colour = txt_col, family = ft_text)
    )
  )

ggsave("images/random/confessionals/complete.png", height = 13, width = 24)

# 💾 save charts ----------------------------------------------------------

ggsave("images/random/confessionals/tree.png", g_tree, height = 8, width = 6)
ggsave("images/random/confessionals/hist.png", g_hist, height = 6, width = 8)
ggsave("images/random/confessionals/top_30.png", g_top_30, height = 12, width = 8)
ggsave("images/random/confessionals/pred.png",
       g_pred/bottom_right_text +
         plot_layout(height = c(3, 1)) +
         plot_annotation(
           theme = theme(
             plot.background = element_rect(fill = bg, colour = bg),
             plot.margin = margin(b = 30)
           )
         ),
       height = 8, width = 6)



# random stats ------------------------------------------------------------

survivoR::castaways |>
  distinct(version_season, castaway_id) |>
  count(castaway_id) |>
  transmute(
    castaway_id,
    multiple_seasons = n > 1
  ) |>
  left_join(df_total) |>
  filter(n >= 50) |>
  count(multiple_seasons)

survivoR::confessionals |>
  filter(version == "US") |>
  # group_by(castaway_id) |>
  summarise(mean = mean(confessional_count)) |>
  ggplot(aes(mean)) +
  geom_histogram(bins = 60)

survivoR::confessionals |>
  filter(version == "US") |>
  group_by(season, castaway_id) |>
  summarise(total = sum(confessional_count)) |>
  ungroup() |>
  summarise(mean = mean(total))
