
library(tidyverse)
library(ggchicklet)
library(ggbump)
library(ggtext)

source("tools/startup.R")

txt_col <- "white"
bg <- "grey20"
ft_text <- "oswald"
ft_title <- "oswald"
pal1 <- my_pink

# advantages plot ---------------------------------------------------------

df_id <- survivoR::advantage_movement |>
  distinct(advantage_id, season) |>
  arrange(season, advantage_id) |>
  group_by(season) |>
  mutate(id = 1:n()) |>
  ungroup() |>
  mutate(global_id = 1:n())

adv_type_levels <- survivoR::advantage_details |>
  distinct(advantage_type) |>
  pull(advantage_type)

df_base <- survivoR::advantage_movement |>
  left_join(df_id, by = c("season", "advantage_id")) |>
  left_join(distinct(survivoR::advantage_details, advantage_id, advantage_type), by = "advantage_id") |>
  group_by(advantage_id) |>
  mutate(
    voted_out_with = event == "Voted out with advantage",
    changed_hands = length(unique(castaway)) > 1,
    detail = case_when(
      voted_out_with ~ "Voted out with advantage",
      event %in% c("Expired", "Absorbed", "Destroyed") ~ "Expired",
      TRUE ~ success
    ),
    advantage_type = factor(advantage_type, levels = adv_type_levels)
    ) |>
  ungroup()

# summary stats -----------------------------------------------------------

summary_stats <- df_base |>
  filter(!str_detect(advantage_id, "b")) |>
  ungroup() |>
  count(detail) |>
  drop_na() |>
  mutate(
    detail = ifelse(detail == "Voted out with advantage", "Voted out with advantage", detail),
    detail = factor(detail, levels = c("Yes", "Not needed", "No", "Expired", "Voted out with advantage")),
  )

total_played <- survivoR::advantage_movement |>
  filter(event == "Played") |>
  nrow()

n_found <- survivoR::advantage_movement |>
  filter(
    !str_detect(advantage_id, "b"),
    event == "Found"
    ) |>
  nrow()

n_played <- survivoR::advantage_movement |>
  filter(
    !str_detect(advantage_id, "b"),
    event == "Played"
  ) |>
  nrow()

n_successful <- survivoR::advantage_movement |>
  filter(
    !str_detect(advantage_id, "b"),
    event == "Played",
    success == "Yes"
  ) |>
  nrow()


# overtime ---------------------------------------------------------------

text1 <- str_wrap(glue("There have been {n_found} advantages in the game since season 11, Guatemala. Since Season 33 Millennials vs. Gen X advantages have been more prevalent in the game",50))

g_time <- survivoR::advantage_details |>
  filter(!str_detect(advantage_id, "b")) |>
  mutate(advantage_type = factor(advantage_type, levels = adv_type_levels)) |>
  count(season, season_name, advantage_type) |>
  ggplot(aes(season, n, fill = advantage_type)) +
  geom_chicklet(colour = "grey20", size = 0.25) +
  geom_text(aes(season, 0.5, label = season), family = "oswald", size = 12, colour = txt_col) +
  annotate("text", x = 11, y = 11, label = text1, hjust = 0, vjust = 1, family = "oswald", size = 18, colour = txt_col, lineheight = 0.3) +
  scale_fill_manual(values = colorRampPalette(d10)(10)) +
  scale_y_continuous(position = "right") +
  labs(y = "Number of\nadvantages") +
  theme_void() +
  theme(
    text = element_text(family = "oswald", colour = "white", size = 36, lineheight = 0.3),
    axis.text.y = element_text(lineheight = 0.4, margin = margin(l = 5)),
    axis.title.y = element_text(lineheight = 0.4, margin = margin(l = -40), size = 48, vjust = 0.9),
    axis.ticks = element_line(colour = "grey20"),
    plot.subtitle = element_text(hjust = 0.5, lineheight = 0.3, size = 32),
    plot.background = element_rect(fill = "grey20", colour = "grey20"),
    plot.margin = margin(t = 30, l = 30, r = 30, b = 30),
    legend.position = "none"
  )

g_time
ggsave("time.png", height = 8, width = 8)


# legend for above --------------------------------------------------------

survivoR::advantage_details |>
  filter(!str_detect(advantage_id, "b")) |>
  mutate(advantage_type = factor(advantage_type, levels = adv_type_levels)) |>
  group_by(advantage_type) |>
  mutate(first_appeared = min(season))

g_leg <- survivoR::advantage_details |>
  filter(!str_detect(advantage_id, "b"),) |>
  mutate(advantage_type = factor(advantage_type, levels = adv_type_levels)) |>
  group_by(advantage_type) |>
  mutate(first_appeared = min(season)) |>
  count(advantage_type, first_appeared) |>
  ungroup() |>
  mutate(
    y = n():1,
    x = 0,
    xn = 0.5,
    n_lab = n,
    n = pmin(n, 38),
    xmax = n/sum(n),
    ymax = y+0.4
  ) |>
  ggplot(aes(x, y, fill = advantage_type)) +
  geom_rrect(aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax)) +
  geom_text(aes(x+0.06, y+0.3, label = advantage_type), family = "oswald", size = 19, colour = txt_col, hjust = 0, vjust = 0) +
  geom_text(aes(xn-0.07, y+0.3, label = n_lab), family = "oswald", size = 19, colour = txt_col, hjust = 1, vjust = 0) +
  geom_text(aes(xn, y+0.3, label = first_appeared), family = "oswald", size = 19, colour = txt_col, hjust = 1, vjust = 0) +
  annotate("segment", x = 0.30, xend = 0.34, y = 10.8, yend = 9.8, size = 3, colour = "grey20") +
  annotate("text", 0.5, 11.7, label = "First\nappeared", family = "oswald", size = 19, colour = txt_col, hjust = 0.5, vjust = 0, lineheight = 0.3) +
  annotate("text", 0, 11.7, label = "Hidden immunity idols are the most prevalent\nadvantage and a staple of new-school survivor", family = "oswald", size = 19, colour = txt_col, hjust = 0, vjust = 0, lineheight = 0.3) +
  scale_fill_manual(values = colorRampPalette(d10)(10)) +
  coord_cartesian(clip = "off") +
  ylim(c(-0.5, 11.7)) +
  theme_void() +
  theme(
    text = element_text(family = "jose", colour = "white", size = 48, lineheight = 0.3),
    plot.background = element_rect(fill = "grey20", colour = "grey20"),
    plot.margin = margin(t = 50, l = 50, r = 50, b = 0),
    legend.position = "none"
  )

g_leg
ggsave("legend.png", height = 7, width = 7)

# success ---------------------------------------------------------------

labels <- c(
  "The advantage was played successfully and changed the result of the vote",
  "The advantage was played successfully but wasn't needed to achieve the desired result",
  "Didn't achieve desired outcome",
  "The advantage expired",
  "Castaway was voted out with the advantage"
)

g_success <- df_base |>
  filter(!str_detect(advantage_id, "b")) |>
  ungroup() |>
  count(detail) |>
  drop_na() |>
  mutate(
    detail = ifelse(detail == "Voted out with advantage", "Voted out with\nadvantage", detail),
    detail = factor(detail, levels = c("Yes", "Not needed", "No", "Expired", "Voted out with\nadvantage")),
    ) |>
  arrange(detail) |>
  mutate(labels = str_wrap(labels, 15)) |>
  ggplot() +
  geom_chicklet(aes(detail, n, fill = detail)) +
  annotate("rect", xmin = 0.5, xmax = 3.5, ymin = 0, ymax = 83, fill = "grey30", colour = "grey30") +
  annotate("text", x = 2, y = 78, label = "Advantages Played", family = "oswald", size = 24, colour = "white") +
  annotate("text", x = 4.5, y = 78, label = "Not played", family = "oswald", size = 24, colour = "white") +
  geom_chicklet(aes(detail, n, fill = detail, colour = detail)) +
  geom_text(
    aes(detail, n-1, label = labels), vjust = 1,
    family = "oswald", colour = dark, size = 12, lineheight = 0.35
  ) +
  geom_text(
    aes(detail, n+7, label = n, colour = detail), vjust = 1,
    family = "oswald", size = 32, lineheight = 0.4, fontface = "bold"
  ) +
  scale_fill_manual(
    values = pal1,
    breaks = c("Yes", "Not needed", "No", "Expired", "Voted out with\nadvantage"),
    labels = c("Successful", "Not needed", "Unsuccessful", "Expired", "Voted out with\nadvantage")
    ) +
  scale_colour_manual(
    values = pal1,
    breaks = c("Yes", "Not needed", "No", "Expired", "Voted out with\nadvantage"),
    labels = c("Successful", "Not needed", "Unsuccessful", "Expired", "Voted out with advantage"),
    guide = "none"
  ) +
  scale_x_discrete(
    labels = c("Yes" = "Successful", "Not needed", "No" = "Unsuccessful", "Expired", "Voted out with advantage")
  ) +
  labs(
    title = str_wrap(glue("There have been {n_found} advantages found, {n_played} played and {n_successful} played successfully that changed the course of the game"),60),
    y = "Number of advantages"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = "oswald", colour = "white", size = 48),
    plot.title = element_text(hjust = 0.5, lineheight = 0.3, size = 52),
    axis.text.x = element_text(lineheight = 0.25, margin = margin(t = 0)),
    axis.ticks.x = element_line(colour = "grey20"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "grey20", colour = "grey20"),
    legend.position = "none",
    plot.margin = margin(t = 40, l = 40, r = 40, b = 30)
  )

g_success
ggsave("success.png", height = 8, width = 8)

# base function -----------------------------------------------------------

ggbase <- function(
    title,
    subtitle,
    ft_title,
    ft_sub,
    x = 0,
    y = 24,
    x_sub = 12,
    y_sub = 24,
    size = 80,
    size_sub = 16
    ) {
  ggplot() +
    annotate(
      "text", x = x, y = y,
      label = title,
      family = ft_title,
      size = size,
      # colour = lighten(d10[3], 0.5),
      colour = pal1[2],
      hjust = 0, vjust = 0.5,
      fontface = "bold",
      lineheight = 0.2
    ) +
      annotate(
        "richtext", x = x_sub, y = y_sub,
        label = subtitle,
        family = ft_sub,
        size = size_sub, colour = txt_col,
        hjust = 0, vjust = 1, label.colour = NA, fill = NA,
        lineheight = 0.35
      ) +
      xlim(c(0, 24)) +
      ylim(c(0, 24)) +
      coord_cartesian(clip = "off") +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "grey20", colour = "grey20"),
        plot.title = element_text()
      )
}

title <- toupper("Survivor: The History of Advantages")
subtitle <- str_rich_wrap(
  "Over the 42 seasons of Survivor advantages have been introduced to the
  game to give players an edge and to shake up the strategy. A successful play
  can help advance the player further in the game but can also make the player
  a target if others know about it. Advantages build uncertainty into the game and
  prompt players to adapt. Advantages, particularly hidden immunity idols are now
  integral to the game of Survivor. Here is the history.",
  200
)

g_base <- ggbase(title, subtitle, size = 92, x = 0, x_sub = 0, y_sub = 23, size_sub = 20,
                 ft_title = "survivor", ft_sub = "oswald")

ggsave("base.png", height = 12, width = 24)

# changed hands charts ----------------------------------------------------------

df_details <- df_base |>
  ungroup() |>
  mutate(
    detail = ifelse(detail == "Voted out with advantage", "Voted out with advantage", detail),
    detail = factor(detail, levels = c("Yes", "Not needed", "No", "Expired", "Voted out with advantage")),
  ) |>
  count(detail) |>
  drop_na() |>
  mutate(y1 = n():1) |>
  select(-n)

df_bump <- df_base |>
  filter(!str_detect(advantage_id, "b")) |>
  mutate(
    detail = ifelse(detail == "Voted out with advantage", "Voted out with advantage", detail),
    detail = factor(detail, levels = c("Yes", "Not needed", "No", "Expired", "Voted out with advantage")),
  ) |>
  group_by(advantage_id) |>
  slice_max(sequence_id) |>
  ungroup() |>
  count(changed_hands, detail) |>
  group_by(changed_hands) |>
  mutate(
    p = n/sum(n),
    x0 = ifelse(changed_hands, 0, 0),
    x1 = x0+3
    ) |>
  ungroup() |>
  mutate(id = 1:n()) |>
  pivot_longer(cols = c(x0, x1), names_to = "x_name", values_to = "x") |>
  left_join(df_details, by = "detail") |>
  mutate(
    y = ifelse(!changed_hands & x == 0, 2, 4),
    y = ifelse(x == 1 | x == 3, y1, y),
    y = y+0.2
    )

df_changed_bar <- df_bump |>
  distinct(changed_hands, detail, p) |>
  left_join(df_details, by = "detail") |>
  mutate(
    y1 = 2*y1,
    xmin = 0 + 3,
    xmax = 4*p + 3,
    ymin = y1-0.3,
    ymax = y1+0.3,
    ymin = ymin + 0.7*changed_hands,
    ymax = ymax + 0.7*changed_hands,
    colour = c(pal1, pal1[c(1,2,4,5)]),
    fill = c(pal1, rep(NA, 4))
  )

df_chg_hands <- df_base |>
  filter(!str_detect(advantage_id, "b")) |>
  distinct(changed_hands, advantage_id) |>
  count(changed_hands)

a <- 0.4
g_changed <- df_changed_bar |>
  ggplot() +
  geom_rrect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = df_changed_bar$fill, colour = df_changed_bar$colour) +
  geom_bump(aes(x, 2*y, group = id, colour = detail), df_bump, size = 0.5) +
  geom_text(aes(xmax + 0.4, ymax-0.3, label = as_pct(p)), colour = df_changed_bar$colour, family = "oswald", size = 18) +
  annotate("rrect", xmin = -2, xmax = 0, ymin = 2*1.4+a, ymax = 2*2.6+a, fill = "white", colour = "white", size = 1) +
  annotate("rrect", xmin = -2, xmax = 0, ymin = 2*3.4+a, ymax = 2*4.6+a, fill = NA, colour = "white", size = 1) +
  annotate("text", x = -1, y = 4+a, label = "The advantage\nremained with the\noriginal finder", family = "oswald", size = 16, colour = "grey20", lineheight = 0.3) +
  annotate("text", x = -1, y = 8+a, label = "The advantage\nwas given to\nanother player", family = "oswald", size = 16, colour = "white", lineheight = 0.3) +
  annotate("text", x = 0.3, y = 4+a+0.6, label = df_chg_hands$n[1], family = "oswald", size = 16, colour = "white", lineheight = 0.3) +
  annotate("text", x = 0.3, y = 8+a+0.6, label = df_chg_hands$n[2], family = "oswald", size = 16, colour = "white", lineheight = 0.3) +
  scale_colour_manual(
    values = pal1,
    breaks = c("Yes", "Not needed", "No", "Expired", "Voted out with advantage"),
    labels = c("Successful", "Not needed", "Unsuccessful", "Expired", "Voted out with advantage"),
    guide = "none"
  ) +
  labs(title = str_wrap("There are 26 instances where an advantage was given to another player but no obivious difference in result.",40)) +
  theme_void() +
  theme(
    text = element_text(family = "oswald", colour = "white", size = 48),
    plot.title = element_text(hjust = 0.25, lineheight = 0.3, size = 48, margin = margin(b = -20)),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "grey20", colour = "grey20"),
    legend.position = "none",
    plot.margin = margin(t = 40, l = 40, r = 40, b = 30)
  )

g_changed

ggsave("changed-hands.png", height = 8, width = 8)

# location ----------------------------------------------------------------

df_location <- survivoR::advantage_details |>
  filter(!str_detect(advantage_id, "b")) |>
  mutate(
    location_found = case_when(
      location_found == "Found around camp" ~ "Around camp",
      str_detect(location_found, "Edge|Exile|Ghost|Idols") ~ "On Exile, Ghost Island, Edge of Extinction",
      str_detect(location_found, "reward|challenge") ~ "At reward or during challenge",
      TRUE ~ "Elsewhere"
    )
  ) |>
  count(location_found) |>
  arrange(n) |>
  mutate(
    x = c(3.55, 4.45, 3.55, 4),
    yn = cumsum(n),
    y = ifelse(row_number() == 1, 0, lag(yn)) + n/2,
    x_text = c(3.25, 4.75, 3.25, 4),
    y_text = c(17, 36, 61, 112),
    y_num_text = c(y[1:3], 70),
    hjust = c(0, 0, 0, 0.5),
    point_col = c(rep(txt_col, 3), "grey20"),
    alpha = c(0.75, 0.5, 0.25, 0),
    location_found = paste(location_found)
  )


g_location <- df_location |>
  ggplot(aes(4, n)) +
  geom_chicklet(alpha = df_location$alpha, colour = "white") +
  geom_point(aes(x, y), size = 5, colour = df_location$point_col, alpha = 1) +
  geom_segment(aes(x = x, xend = x_text, y = y, yend = y_text-1), colour = df_location$point_col) +
  geom_text(aes(x_text, y_text, label = location_found), family = "oswald", size = 18,
           colour = txt_col, lineheight = 0.4, hjust = df_location$hjust, alpha = 1) +
  geom_richtext(aes(3.7, y, label = n), family = "oswald", size = 12,
            colour = txt_col, lineheight = 0.4, hjust = 0.5, alpha = 1, label.color = NA,
            fill = NA) +
  annotate("text", 5.5, 82, label = "Advantages were found ...", family = "oswald", size = 18,
           colour = txt_col, lineheight = 0.4, hjust = 0.5) +
  coord_flip(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = "oswald", colour = "white", size = 48),
    plot.title = element_text(hjust = 0.5, lineheight = 0.3, size = 48),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "grey20", colour = "grey20"),
    legend.position = "none",
    plot.margin = margin(t = 40, l = 40, r = 40, b = 30)
  )
g_location



# clue --------------------------------------------------------------------


df_clue <- survivoR::advantage_details |>
  filter(!str_detect(advantage_id, "b")) |>
  mutate(
    clue_details = ifelse(
      clue_details %in% c("Found without clue", "No clue existed"),
      "Without clue",
      "With clue"
      )
  ) |>
  count(clue_details) |>
  arrange(n) |>
  mutate(
    x = 1,
    yn = cumsum(n),
    y = ifelse(row_number() == 1, 0, lag(yn)) + n/2,
    x_text = x,
    y_text = y,
    hjust = 0.5,
    alpha = c(0.5, 0),
    y_num_text = y - c(25, 40)
  )


g_clue <- df_clue |>
  ggplot(aes(1, n)) +
  geom_chicklet(alpha = df_clue$alpha, colour = "white") +
  geom_text(aes(x_text, y_text, label = clue_details), family = "oswald", size = 18,
            colour = txt_col, lineheight = 0.4, hjust = df_clue$hjust, alpha = 1) +
  geom_richtext(aes(0.7, y_text, label = n), family = "oswald", size = 12,
                colour = txt_col, lineheight = 0.4, hjust = 0.5, alpha = 1, label.color = NA,
                fill = NA) +
  coord_flip(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = "oswald", colour = "white", size = 48),
    plot.title = element_text(hjust = 0.5, lineheight = 0.3, size = 48),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "grey20", colour = "grey20"),
    legend.position = "none",
    plot.margin = margin(t = 40, l = 40, r = 40, b = 30)
  )
g_clue

g_location /
  g_clue +
  plot_layout(heights = c(5, 2)) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(colour = "grey20", fill = "grey20")
    )
  )

ggsave("location.png", height = 6, width = 6)



# when played -------------------------------------------------------------

g_when_played <- df_base |>
  arrange(advantage_id, sequence_id) |>
  group_by(advantage_id) |>
  slice_max(sequence_id) |>
  ungroup() |>
  count(episode, detail) |>
  mutate(
    detail = ifelse(detail == "Voted out with advantage", "Voted out with advantage", detail),
    detail = factor(detail, levels = c("Yes", "Not needed", "No", "Expired", "Voted out with advantage")),
  ) |>
  ggplot() +
  geom_chicklet(aes(episode, n, fill = detail), colour = "grey20") +
  annotate("text", 4, 12, label = "Most blindsides occur\nin episode 11", family = ft_text, size = 19, colour = txt_col, hjust = 0, lineheight = 0.3) +
  annotate("curve", x = 5, xend = 11, y = 20, yend = 18, curvature = 0.3, arrow = arrow(length = unit(0.02, "npc"), type = "closed"), colour = txt_col) +
  scale_fill_manual(
    values = pal1,
    breaks = c("Yes", "Not needed", "No", "Expired", "Voted out with advantage"),
    labels = c("Successful", "Not needed", "Unsuccessful", "Expired", "Voted out with advantage")
  ) +
  scale_x_continuous(breaks = 1:15, labels = 1:15) +
  coord_flip(clip = "off") +
  labs(
    title = "Episode the advantage was played"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "oswald", colour = "white", size = 48),
    plot.title = element_text(hjust = 0, lineheight = 0.3, size = 52),
    axis.text = element_text(lineheight = 0.25, margin = margin(t = 0)),
    axis.ticks.x = element_line(colour = "grey20"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "grey20", colour = "grey20"),
    legend.position = "none",
    plot.margin = margin(t = 40, l = 40, r = 40, b = 30)
  )

g_when_played
ggsave("when-played.png", height = 8, width = 8)


# join --------------------------------------------------------------------

g_base +
  inset_element(g_success, left = 0.03, right = 0.37, bottom = -0.03, top = 0.5) +
  inset_element(g_leg, left = 0.03, right = 0.33, bottom = 0.45, top = 0.84) +
  inset_element(g_when_played, left = 0.375, right = 0.615, bottom = -0.03, top = 0.45) +
  inset_element(g_time, left = 0.33, right = 0.72, bottom = 0.42, top = 0.865) +
  inset_element(g_clue, left = 0.72, right = 1, bottom = 0.45, top = 0.62) +
  inset_element(g_location, left = 0.72, right = 1, bottom = 0.58, top = 0.87) +
  inset_element(g_changed, left = 0.6, right = 1, bottom = 0, top = 0.45) +
  plot_annotation(
    caption = glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Data: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/survivoR #rstats"),
    theme = theme(
      plot.background = element_rect(fill = "grey20", colour = "grey20"),
      plot.caption = element_markdown(size = 32, colour = txt_col, family = ft_text)
    )
  )

ggsave("complete.png", height = 13, width = 24)
