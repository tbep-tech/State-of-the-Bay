library(ggplot2)
library(dplyr)
library(tidyr)
library(googlesheets4)
library(googledrive)
library(showtext)
library(here)
library(patchwork)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

# showtext_auto()
# showtext_opts(dpi = 500)

# auth google drive
drive_auth(email = 'mbeck@tbep.org')
gs4_auth(token = drive_token())

rstdat <- read_sheet('1IkGidfU4SQJ_ZtOEqfn8licNCuWGlq_MtGuWWuIL1Js', sheet = 'FullDatabase')

# data prep
rstsum <- rstdat %>% 
  select(
    Year = `Year Reported`, 
    HMPU_TARGETS = `HMPU Habitat Type for Restoration`, 
    Acres, 
    Activity = `Basic Activity (Enhance/Rest)`, 
    `Linear Miles` = `Linear Miles`,
    `Linear Ft` = `Linear Feet`
  ) %>% 
  rowwise() %>% 
  mutate(
    Miles = sum(`Linear Miles`,  `Linear Ft` / 5280, na.rm = T), 
    HMPU_TARGETS = case_when(
      HMPU_TARGETS == 'Uplands (Non-coastal)' ~ 'Native Uplands',
      HMPU_TARGETS == 'Non-forested Freshwater Wetlands' ~ 'Non-Forested Freshwater Wetlands', 
      # HMPU_TARGETS %in% c('Intertidal Estuarine (Other)', 'Mangrove Forests', 'Salt Barrens', 'Salt marshes') ~ 'Total Intertidal', 
      T ~ HMPU_TARGETS)
  ) %>% 
  ungroup() %>% 
  filter(Year <= 2021 & Year >= 2019) %>% 
  group_by(HMPU_TARGETS, Activity) %>% 
  summarise(
    tot= n(),
    Acres = sum(Acres, na.rm = T), 
    Miles = sum(Miles, na.rm = T),
    .groups = 'drop'
  ) %>% 
  group_by(HMPU_TARGETS) %>% 
  mutate(
    tot = sum(tot)
  ) %>% 
  pivot_longer(c('Acres', 'Miles'), names_to = 'var', values_to = 'val') %>% 
  unite('var', Activity, var, sep = ', ') %>% 
  pivot_wider(names_from = 'var', values_from = 'val') %>% 
  select(HMPU_TARGETS, tot, `Restoration, Acres`, `Restoration, Miles`, `Enhancement, Acres`, `Enhancement, Miles`)

# add total intertidal from GPRA
totint <- rstsum %>% 
  filter(HMPU_TARGETS %in% c('Intertidal Estuarine (Other)', 'Mangrove Forests', 'Salt Barrens', 'Salt marshes')) %>% 
  mutate(HMPU_TARGETS = 'Total Intertidal') %>% 
  group_by(HMPU_TARGETS) %>% 
  summarise_if(is.numeric, function(x) sum(x, na.rm = T))

# add totint to rstsum
rstsum <- rstsum %>% 
  bind_rows(totint) %>% 
  filter(HMPU_TARGETS != 'Intertidal Estuarine (Other)')

# from https://github.com/tbep-tech/hmpu-workflow/blob/b58f01e167002aea81817acc7a4ec55de41a598c/R/funcs.R#L925
# 2020 seagrass est added manually
cursum <- structure(list(
  Category = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c("Subtidal", "Intertidal", "Supratidal"), class = "factor"), 
  HMPU_TARGETS = c("Artificial Reefs",  "Hard Bottom", "Oyster Bars", "Seagrasses", "Tidal Flats", "Living Shorelines", 
                   "Total Intertidal", "Mangrove Forests", "Salt Barrens", "Salt Marshes", "Tidal Tributaries", 
                   "Coastal Uplands", "Forested Freshwater Wetlands", "Native Uplands", 
                   "Non-Forested Freshwater Wetlands"), 
  unis = c("ac", "ac", "ac", "ac", "ac", "mi", "ac", "ac", "ac", "ac", "mi", "ac", "ac", "ac", "ac"), 
  `Current Extent` = c(166, 423, 171, 33945.0856405873, 16220, 11.3, 20353, 15300, 496, 4557, 387, 3619, 152132, 140600, 67587)), 
  class = "data.frame", row.names = c(NA, -15L))

# restoresum
restoresum <- structure(list(
  HMPU_TARGETS = c("Coastal Uplands", "Forested Freshwater Wetlands", 
                   "Mangrove Forests", "Native Uplands", "Non-Forested Freshwater Wetlands", 
                   "Salt Barrens", "Salt Marshes", "Total Intertidal"), 
  `total restorable` = c(1272, 159836, 2757, 43928, 159836, 2757, 1092, 3849)), 
  row.names = c(NA, -8L), class = c("tbl_df", "tbl", "data.frame"))

trgs <- data.frame(
  Category  = c("Subtidal", "Subtidal", "Subtidal", "Subtidal", "Subtidal",
                "Intertidal", "Intertidal", "Intertidal", "Intertidal", "Intertidal", "Intertidal", 
                "Supratidal", "Supratidal", "Supratidal", "Supratidal"),
  HMPU_TARGETS = c("Hard Bottom", "Artificial Reefs", "Tidal Flats", "Seagrasses", "Oyster Bars",
                   "Living Shorelines", "Total Intertidal", "Mangrove Forests", "Salt Barrens", "Salt Marshes", "Tidal Tributaries", 
                   "Coastal Uplands", "Non-Forested Freshwater Wetlands", "Forested Freshwater Wetlands", "Native Uplands"),
  `Target2030` = c(423, 166, 16220, 40000, 221,
                   21.3, 21353, 15300, 546, 4807, 391,
                   3769, 68937, 152282, 141050),
  `Target2050` = c(423, 166, 16220, 40000, 471,
                   56.3, 23803, 15300, 796, 5457, 405,
                   4219, 71787, 152732, 142100)
) %>% 
  mutate(
    Category = factor(Category, levels = c("Subtidal", "Intertidal", "Supratidal")), 
    HMPU_TARGETS = factor(HMPU_TARGETS, levels = HMPU_TARGETS)
  )

prg <- rstsum %>% 
  full_join(cursum, by = 'HMPU_TARGETS') %>% 
  full_join(trgs, by = c('HMPU_TARGETS', 'Category')) %>% 
  # filter(!HMPU_TARGETS %in% c('Mangrove Forests', 'Salt Barrens', 'Salt Marshes')) %>% 
  mutate(
    prgtot = case_when(
      unis == 'ac' ~ `Restoration, Acres`,
      unis == 'mi' ~ `Restoration, Miles`
    ),
    prgtot = ifelse(is.na(prgtot), 0, prgtot), 
    tot = ifelse(is.na(tot), 0, tot)
  ) %>% 
  select(HMPU_TARGETS, tot, Category, unis, `Current Extent`, `Target2030`, `Target2050`, prgtot) %>% 
  pivot_longer(cols = c('Current Extent', 'prgtot'), names_to = 'cat', values_to = 'extent') %>% 
  mutate(
    cat = factor(cat, 
      levels = rev(c('Current Extent', 'prgtot')), 
      labels = rev(c('2017 Extent', '2019-2021 progress'))
      )
  ) %>% 
  group_by(HMPU_TARGETS) %>% 
  mutate(
    prg2030 = 100 * (extent / Target2030),
    prg2050 = 100 * (extent / Target2050)
  ) %>% 
  ungroup() %>% 
  mutate(
    HMPU_TARGETS = factor(HMPU_TARGETS, levels = rev(sort(unique(HMPU_TARGETS)))), 
    Category = factor(Category, 
                      levels = c("Subtidal", "Intertidal", "Supratidal"),
                      labels = c("In-bay", "Coastal", "Uplands")
    ), 
    typ = case_when(
      HMPU_TARGETS %in% c('Artificial Reefs', 'Hard Bottom', 'Tidal Flats', 'Mangrove Forests', 'Native Uplands', 'Forested Freshwater Wetlands') ~ 'hold', 
      T ~ 'prog'
    )
  )

labs <- prg %>% 
  group_by(HMPU_TARGETS, Category, typ) %>% 
  summarise(
    gain2030 = case_when(
      cat == '2019-2021 progress' ~ prg2030
      ),
    gain2050 = case_when(
      cat == '2019-2021 progress' ~ prg2050
    ),
    prg2030 = sum(prg2030),
    prg2050 = sum(prg2050), 
    .groups = 'drop'
  ) %>% 
  filter(!is.na(gain2030)) %>% 
  mutate(
    gain2030 = case_when(
      gain2030 == 0 ~ '',
      T ~ paste0('+', round(gain2030, 1), '%')
    ),
    gain2050 = case_when(
      gain2050 == 0 ~ '',
      T ~ paste0('+', round(gain2050, 1), '%')
    ), 
    prg2030 = case_when(
      !HMPU_TARGETS %in% c('Oyster Bars', 'Living Shorelines', 'Coastal Uplands') ~ 100, 
      T ~ prg2030
    ), 
    prg2050 = case_when(
      !HMPU_TARGETS %in% c('Oyster Bars', 'Living Shorelines', 'Coastal Uplands') ~ 100, 
      T ~ prg2050
    )
  )

cols <-  c('#E4A41A', '#004F7E')

thm <- theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    strip.placement = 'outside', 
    legend.position = 'top',
    strip.background = element_blank(), 
    strip.text.y = element_text(angle = 90, size = 10), 
    strip.text.x = element_blank(),
    axis.title.x = element_text(size = 11)
  )

prg1 <- prg %>% 
  filter(typ == 'prog')
labs1 <- labs %>% 
  filter(typ == 'prog')

p1 <- ggplot(prg1, aes(y = HMPU_TARGETS, x = prg2030)) + 
  geom_bar(aes(fill = cat), stat = 'identity') + 
  facet_grid(Category ~ ., space = 'free', scales = 'free_y', switch = 'y') + 
  geom_vline(xintercept = 100, linetype = 'dashed', size = 1, col = '#00806E') + 
  geom_text(data = labs1, aes(label = gain2030), hjust = 0, nudge_x = 1, col = cols[1]) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100), limits = c(0, 120)) +
  scale_fill_manual(values = cols) +
  guides(fill = guide_legend(reverse = T)) +
  thm +
  labs(
    x = '% of 2030 target',
    y = NULL, 
    fill = NULL,
    title = 'Progress needed'
  )

prg2 <- prg %>% 
  filter(typ == 'hold')
labs2 <- labs %>% 
  filter(typ == 'hold')

p2 <- ggplot(prg2, aes(y = HMPU_TARGETS, x = prg2030)) + 
  geom_bar(aes(fill = cat), stat = 'identity') + 
  facet_grid(Category ~ ., space = 'free', scales = 'free_y', switch = 'y') + 
  geom_vline(xintercept = 100, linetype = 'dashed', size = 1, col = '#00806E') + 
  geom_text(data = labs2, aes(label = gain2030), hjust = 0, nudge_x = 1, col = cols[1]) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100), limits = c(0, 120)) +
  scale_fill_manual(values = cols) +
  guides(fill = guide_legend(reverse = T)) +
  thm +
  labs(
    x = '% of 2030 target',
    y = NULL, 
    fill = NULL,
    title = 'Hold the line'
  )

svg(here('figures/restoreprg.svg'), family = fml, height = 5, width = 9)
print(p1)
dev.off()

svg(here('figures/restorehold.svg'), family = fml, height = 3, width = 8)
print(p2)
dev.off()

