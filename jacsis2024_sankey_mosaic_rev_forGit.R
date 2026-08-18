library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)
library(ggplot2)
# install.packages("ggalluvial")
library(ggalluvial)
library(dplyr)
library(tibble)
library(ggnewscale)
library(scales)

# install.packages("devtools")
# devtools::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)

# load data ### This is just an example. Not real data!!!
load("jacsis_git_example2024.RData")


# Filter data for valid IDs
data <- data %>% filter(Monitor_ID %in% valid_ids$V1)

data <- data %>% left_join(ipw2, by = c("Monitor_ID" = "Monitor_ID..."))

######### Next vaccine dev when #########

### raw count
# data_sankey <- data %>%
#   select(Q41.1, Q41.2) %>%
#   filter(!is.na(Q41.1) & !is.na(Q41.2)) %>%
#   group_by(Q41.1, Q41.2) %>%
#   summarise(count = n(), .groups = 'drop')

### ipw age
data_sankey <- data %>%
  select(Q41.1, Q41.2, ipw2corr) %>%
  filter(!is.na(Q41.1) & !is.na(Q41.2)) %>%
  group_by(Q41.1, Q41.2) %>%
  summarise(count = sum(ipw2corr, na.rm = TRUE), .groups = "drop")

data_sankey <- data_sankey %>%
  mutate(Q41.1 = as.character(Q41.1),
         Q41.2 = as.character(Q41.2))

# Substitute 1 as AA, 2 as BB, etc.
data_sankey <- data_sankey %>%
  mutate(Q41.1 = recode(Q41.1,
                        `1` = "Several months",
                        `2` = "A half year",
                        `3` = "One year",
                        `4` = "2-3 years",
                        `5` = ">4 years",
                        `6` = "Never"),
         Q41.2 = recode(Q41.2,
                        `1` = "Several months",
                        `2` = "A half year",
                        `3` = "One year",
                        `4` = "2-3 years",
                        `5` = ">4 years",
                        `6` = "Never")) %>%
  ungroup()

# Level
data_sankey$Q41.1 <- factor(data_sankey$Q41.1, levels = c("Never", ">4 years", "2-3 years", "One year", "A half year", "Several months"))
data_sankey$Q41.2 <- factor(data_sankey$Q41.2, levels = c("Never", ">4 years", "2-3 years", "One year", "A half year", "Several months"))

# # Create a Sankey diagram using ggplot2
total_n <- sum(data_sankey$count)  # Total number of valid responses

data_sankey <- data_sankey %>%
  mutate(per = count/total_n*100,
         count_per = paste0(round(count/total_n*100,1),"%\n(", count, ")"))

### raw count
# count_temp <- as.numeric(table(data$Q41.1))
# per_temp <- round(count_temp / nrow(data) * 100, 1)

### ipw age
count_temp <- as.numeric(
  tapply(data$ipw2corr[!is.na(data$Q41.1)],
         data$Q41.1[!is.na(data$Q41.1)],
         sum,
         na.rm = TRUE)
)
per_temp <- round(count_temp / nrow(data) * 100, 1)
count_temp <- round(count_temp, 0)

lab_map_Q41_1 <- c(
  "Never" = paste0(per_temp[6], "%\n(", count_temp[6], ")"),
  ">4 years" = paste0(per_temp[5], "%\n(", count_temp[5], ")"),
  "2-3 years" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "One year" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "A half year" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Several months" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

### raw count
# count_temp <- as.numeric(table(data$Q41.2))
# per_temp <- round(count_temp / nrow(data) * 100, 1)

### ipw age
count_temp <- as.numeric(
  tapply(data$ipw2corr[!is.na(data$Q41.2)],
         data$Q41.2[!is.na(data$Q41.2)],
         sum,
         na.rm = TRUE)
)
per_temp <- round(count_temp / nrow(data) * 100, 1)
count_temp <- round(count_temp, 0)

lab_map_Q41_2 <- c(
  "Never" = paste0(per_temp[6], "%\n(", count_temp[6], ")"),
  ">4 years" = paste0(per_temp[5], "%\n(", count_temp[5], ")"),
  "2-3 years" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "One year" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "A half year" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Several months" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

# Q41.1 用と Q41.2 用に、それぞれ「カテゴリ → ラベル」の対応を作成
lab1 <- data_sankey %>% distinct(Q41.1, count_per) %>% 
  { setNames(.$count_per, .$Q41.1) }

lab2 <- data_sankey %>% distinct(Q41.2, count_per) %>% 
  { setNames(.$count_per, .$Q41.2) }

lvl1 <- levels(factor(data_sankey$Q41.1))
lvl2 <- levels(factor(data_sankey$Q41.2))

color_pal <- c(
  # "#FFB347", # pastel red-orange
  # "#FF9999"
  "#CBAACB",  # pastel purple
  "#FF6961",
  "#FFD1DC", # pastel pink
  "#77DD77", # pastel green
  "#89CFF0", # pastel light blue
  "#66B2FF"
)

pal1 <- setNames(color_pal, lvl1)  # Q41.1用
pal2 <- setNames(color_pal, lvl2)  # Q41.2用

ggplot(
  data_sankey,
  aes(axis1 = Q41.1, axis2 = Q41.2, y = per)
) +
  # フローは Q41.1 の色
  geom_alluvium(aes(fill = Q41.1), width = 1/12, alpha = 0.6) +
  
  # 左ストラタ（Q41.1）：凡例あり
  geom_stratum(
    aes(x = 1, stratum = Q41.1, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 1, stratum = Q41.1,
      label = after_stat( # 見つかれば置換、なければ元の文字
        ifelse(!is.na(lab_map_Q41_1[as.character(stratum)]),
               lab_map_Q41_1[as.character(stratum)],
               as.character(stratum))
      )
    ),
    size = 5, color = "gray25"
  ) +
  scale_fill_manual(values = pal1, name = "") +
  
  # ★ ここで fill スケールをリセット（以降は別スケール）
  new_scale_fill() +
  
  # 右ストラタ（Q41.2）：色は付けるが凡例は出さない
  geom_stratum(
    aes(x = 2, stratum = Q41.2, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 2, stratum = Q41.2,
      label = after_stat(
        ifelse(!is.na(lab_map_Q41_2[as.character(stratum)]),
               lab_map_Q41_2[as.character(stratum)],
               as.character(stratum))
      )
    ),
    size = 5, color = "gray25"
  ) +
  scale_fill_manual(values = pal2, guide = "none") +  # ← 凡例を消す
  
  scale_x_discrete(limits = c("Development","Implementation"), expand = c(.1, .1)) +
  labs(x = NULL, y = "Percentage", title = "") +
  theme_minimal() +
  # Change font size of x and y titles and labels, and legend font
  theme(
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 22)
  ) +
  # Font color
  theme(
    text = element_text(color = "gray25")
    # axis.text = element_text(color = "black"),
    # axis.title = element_text(color = "black"),
    # legend.text = element_text(color = "black"),
    # legend.title = element_text(color = "black")
  )

#1000x750

######### Fatality increase #########

### ALL / Vac / Unvac

# data_temp <- data 
data_temp <- data %>% filter(Q39 <= 2)
# data_temp <- data %>% filter(Q39 >= 4)

data_sankey <- data_temp %>%
  filter(Q39 != 3 & !is.na(Q42.1) & !is.na(Q42.2) & !is.na(Q42.3)) %>%
  select(Q42.1, Q42.2, Q42.3) %>%
  group_by(Q42.1, Q42.2, Q42.3) %>%
  summarise(count = n(), .groups = 'drop')
data_sankey <- data_sankey %>%
  mutate(Q42.1 = as.character(Q42.1),
         Q42.2 = as.character(Q42.2))

# Substitute 1 as AA, 2 as BB, etc.
data_sankey <- data_sankey %>%
  mutate(Q42.1 = recode(Q42.1,
                        `1` = "Definitely will",
                        `2` = "Probably will",
                        `3` = "Probably won't",
                        `4` = "Definitely won't"),
         Q42.2 = recode(Q42.2,
                        `1` = "Definitely will",
                        `2` = "Probably will",
                        `3` = "Probably won't",
                        `4` = "Definitely won't"),
         Q42.3 = recode(Q42.3,
                        `1` = "Definitely will",
                        `2` = "Probably will",
                        `3` = "Probably won't",
                        `4` = "Definitely won't")) %>%
  ungroup()

# Level
data_sankey$Q42.1 <- factor(data_sankey$Q42.1, levels = c("Definitely won't", "Probably won't", "Probably will", "Definitely will"))
data_sankey$Q42.2 <- factor(data_sankey$Q42.2, levels = c("Definitely won't", "Probably won't", "Probably will", "Definitely will"))
data_sankey$Q42.3 <- factor(data_sankey$Q42.3, levels = c("Definitely won't", "Probably won't", "Probably will", "Definitely will"))

# # Create a Sankey diagram using ggplot2
total_n <- sum(data_sankey$count)  # Total number of valid responses

data_sankey <- data_sankey %>%
  mutate(per = count/total_n*100,
         count_per = paste0(round(count/total_n*100,1),"%\n(", count, ")"))

count_temp <- as.numeric(table(data_temp$Q42.1))
per_temp <- round(count_temp / nrow(data_temp) * 100, 1)

lab_map_Q42_1 <- c(
  "Definitely won't" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "Probably won't" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "Probably will" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Definitely will" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

count_temp <- as.numeric(table(data_temp$Q42.2))
per_temp <- round(count_temp / nrow(data_temp) * 100, 1)

lab_map_Q42_2 <- c(
  "Definitely won't" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "Probably won't" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "Probably will" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Definitely will" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

count_temp <- as.numeric(table(data_temp$Q42.3))
per_temp <- round(count_temp / nrow(data_temp) * 100, 1)

lab_map_Q42_3 <- c(
  "Definitely won't" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "Probably won't" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "Probably will" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Definitely will" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

# Q41.1 用と Q41.2 用に、それぞれ「カテゴリ → ラベル」の対応を作成
lab1 <- data_sankey %>% distinct(Q42.1, count_per) %>% 
  { setNames(.$count_per, .$Q42.1) }

lab2 <- data_sankey %>% distinct(Q42.2, count_per) %>% 
  { setNames(.$count_per, .$Q42.2) }

lab3 <- data_sankey %>% distinct(Q42.3, count_per) %>% 
  { setNames(.$count_per, .$Q42.3) }

lvl1 <- levels(factor(data_sankey$Q42.1))
lvl2 <- levels(factor(data_sankey$Q42.2))
lvl3 <- levels(factor(data_sankey$Q42.3))

color_pal <- c(
  # "#FFB347", # pastel red-orange
  # "#FF9999"
  # "#CBAACB",  # pastel purple
  "#FF6961",
  "#FFD1DC", # pastel pink
  # "#77DD77", # pastel green
  "#89CFF0", # pastel light blue
  "#66B2FF"
)

pal1 <- setNames(color_pal, lvl1)
pal2 <- setNames(color_pal, lvl2)
pal3 <- setNames(color_pal, lvl3)

ggplot(
  data_sankey,
  aes(axis1 = Q42.1, axis2 = Q42.2, axis3 = Q42.3, y = per)
) +
  # フロー：左(Q42.1)の色 → このスケールに凡例を出す
  geom_alluvium(aes(fill = Q42.1), width = 1/12, alpha = 0.6, discern = TRUE, show.legend = TRUE) +
  
  # 左ストラタ（Q42.1）：凡例あり
  geom_stratum(
    aes(x = 1, stratum = Q42.1, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 1, stratum = Q42.1,
      label = after_stat(ifelse(!is.na(lab_map_Q42_1[as.character(stratum)]),
                                lab_map_Q42_1[as.character(stratum)],
                                as.character(stratum))))
    , size = 7, coro = "gray25", check_overlap = TRUE
  ) +
  # ← この scale_fill_manual が「左(=フロー含む)用」。凡例タイトルは空に。
  scale_fill_manual(values = pal1, name = "Vaccination in future pandemic") +
  
  # ===== ここで中央用にスケールをリセット（重要！） =====
new_scale_fill() +
  
  # 中央ストラタ（Q42.2）：色は付けるが凡例は出さない
  geom_stratum(
    aes(x = 2, stratum = Q42.2, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 2, stratum = Q42.2,
      label = after_stat(ifelse(!is.na(lab_map_Q42_2[as.character(stratum)]),
                                lab_map_Q42_2[as.character(stratum)],
                                as.character(stratum))))
    , size = 7, coro = "gray25", check_overlap = TRUE
  ) +
  scale_fill_manual(values = pal2, guide = "none") +
  
  # ===== 右用に再度スケールをリセット =====
new_scale_fill() +
  
  # 右ストラタ（Q42.3）：色は付けるが凡例は出さない
  geom_stratum(
    aes(x = 3, stratum = Q42.3, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 3, stratum = Q42.3,
      label = after_stat(ifelse(!is.na(lab_map_Q42_3[as.character(stratum)]),
                                lab_map_Q42_3[as.character(stratum)],
                                as.character(stratum))))
    , size = 7, coro = "gray25", check_overlap = TRUE
  ) +
  scale_fill_manual(values = pal3, guide = "none") +
  
  scale_x_discrete(
    limits = c("0.2-1%\n(COVID-19)","2-15%\n(Spanish flu)",">20%\n(H5N1)"),   # 実データに合わせて
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  labs(x = NULL, y = "Percentage", title = "") +
  theme_minimal() +
  # Change font size of x and y titles and labels, and legend font
  theme(
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24)
  ) +
  # Font color
  theme(
    text = element_text(color = "gray25")
    # axis.text = element_text(color = "black"),
    # axis.title = element_text(color = "black"),
    # legend.text = element_text(color = "black"),
    # legend.title = element_text(color = "black")
  )

#1200x1200

###### Durability ######
### All / Vac / Unvac
# No subtract data
# data_temp <- data
data_temp <- data %>% filter(Q39 <= 2)
# data_temp <- data %>% filter(Q39 >= 4)

data_sankey <- data_temp %>%
  filter(Q39 != 3 & !is.na(Q42.4) & !is.na(Q42.5) & !is.na(Q42.6) & !is.na(Q42.7) & !is.na(Q42.8) ) %>%
  select(Q42.4, Q42.5, Q42.6,  Q42.7, Q42.8) %>%
  group_by(Q42.4, Q42.5, Q42.6,  Q42.7, Q42.8) %>%
  summarise(count = n(), .groups = 'drop')
data_sankey <- data_sankey %>%
  mutate(Q42.4 = as.character(Q42.4),
         Q42.5 = as.character(Q42.5),
         Q42.6 = as.character(Q42.6),
         Q42.7 = as.character(Q42.7),
         Q42.8 = as.character(Q42.8))

# Substitute 1 as AA, 2 as BB, etc.
data_sankey <- data_sankey %>%
  mutate(Q42.4 = recode(Q42.4,
                        `1` = "Definitely will",
                        `2` = "Probably will",
                        `3` = "Probably won't",
                        `4` = "Definitely won't"),
         Q42.5 = recode(Q42.5,
                        `1` = "Definitely will",
                        `2` = "Probably will",
                        `3` = "Probably won't",
                        `4` = "Definitely won't"),
         Q42.6 = recode(Q42.6,
                        `1` = "Definitely will",
                        `2` = "Probably will",
                        `3` = "Probably won't",
                        `4` = "Definitely won't"),
         Q42.7 = recode(Q42.7,
                        `1` = "Definitely will",
                        `2` = "Probably will",
                        `3` = "Probably won't",
                        `4` = "Definitely won't"),
         Q42.8 = recode(Q42.8,
                        `1` = "Definitely will",
                        `2` = "Probably will",
                        `3` = "Probably won't",
                        `4` = "Definitely won't")) %>%
  ungroup()

# Level
data_sankey$Q42.4 <- factor(data_sankey$Q42.4, levels = c("Definitely won't", "Probably won't", "Probably will", "Definitely will"))
data_sankey$Q42.5 <- factor(data_sankey$Q42.5, levels = c("Definitely won't", "Probably won't", "Probably will", "Definitely will"))
data_sankey$Q42.6 <- factor(data_sankey$Q42.6, levels = c("Definitely won't", "Probably won't", "Probably will", "Definitely will"))
data_sankey$Q42.7 <- factor(data_sankey$Q42.7, levels = c("Definitely won't", "Probably won't", "Probably will", "Definitely will"))
data_sankey$Q42.8 <- factor(data_sankey$Q42.8, levels = c("Definitely won't", "Probably won't", "Probably will", "Definitely will"))

# # Create a Sankey diagram using ggplot2
total_n <- sum(data_sankey$count)  # Total number of valid responses

data_sankey <- data_sankey %>%
  mutate(per = count/total_n*100,
         count_per = paste0(round(count/total_n*100,1),"%\n(", count, ")"))

count_temp <- as.numeric(table(data_temp$Q42.4))
per_temp <- round(count_temp / nrow(data_temp) * 100, 1)

lab_map_Q42_4 <- c(
  "Definitely won't" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "Probably won't" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "Probably will" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Definitely will" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

count_temp <- as.numeric(table(data_temp$Q42.5))
per_temp <- round(count_temp / nrow(data_temp) * 100, 1)

lab_map_Q42_5 <- c(
  "Definitely won't" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "Probably won't" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "Probably will" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Definitely will" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

count_temp <- as.numeric(table(data_temp$Q42.6))
per_temp <- round(count_temp / nrow(data_temp) * 100, 1)

lab_map_Q42_6 <- c(
  "Definitely won't" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "Probably won't" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "Probably will" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Definitely will" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

count_temp <- as.numeric(table(data_temp$Q42.7))
per_temp <- round(count_temp / nrow(data_temp) * 100, 1)

lab_map_Q42_7 <- c(
  "Definitely won't" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "Probably won't" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "Probably will" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Definitely will" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

count_temp <- as.numeric(table(data_temp$Q42.8))
per_temp <- round(count_temp / nrow(data_temp) * 100, 1)

lab_map_Q42_8 <- c(
  "Definitely won't" = paste0(per_temp[4], "%\n(", count_temp[4], ")"),
  "Probably won't" = paste0(per_temp[3], "%\n(", count_temp[3], ")"),
  "Probably will" = paste0(per_temp[2], "%\n(", count_temp[2], ")"),
  "Definitely will" = paste0(per_temp[1], "%\n(", count_temp[1], ")")
)

# それぞれ「カテゴリ → ラベル」の対応を作成
lab4 <- data_sankey %>% distinct(Q42.4, count_per) %>% 
  { setNames(.$count_per, .$Q42.4) }
lab5 <- data_sankey %>% distinct(Q42.5, count_per) %>% 
  { setNames(.$count_per, .$Q42.5) }
lab6 <- data_sankey %>% distinct(Q42.6, count_per) %>% 
  { setNames(.$count_per, .$Q42.6) }
lab7 <- data_sankey %>% distinct(Q42.7, count_per) %>% 
  { setNames(.$count_per, .$Q42.7) }
lab8 <- data_sankey %>% distinct(Q42.8, count_per) %>% 
  { setNames(.$count_per, .$Q42.8) }

lvl4 <- levels(factor(data_sankey$Q42.4))
lvl5 <- levels(factor(data_sankey$Q42.5))
lvl6 <- levels(factor(data_sankey$Q42.6))
lvl7 <- levels(factor(data_sankey$Q42.7))
lvl8 <- levels(factor(data_sankey$Q42.8))

color_pal <- c(
  # "#FFB347", # pastel red-orange
  # "#FF9999"
  # "#CBAACB",  # pastel purple
  "#FF6961",
  "#FFD1DC", # pastel pink
  # "#77DD77", # pastel green
  "#89CFF0", # pastel light blue
  "#66B2FF"
)

pal4 <- setNames(color_pal, lvl4)
pal5 <- setNames(color_pal, lvl5)
pal6 <- setNames(color_pal, lvl6)
pal7 <- setNames(color_pal, lvl7)
pal8 <- setNames(color_pal, lvl8)

ggplot(
  data_sankey,
  aes(axis1 = Q42.4, axis2 = Q42.6, axis3 = Q42.6, axis4 = Q42.7, axis5 = Q42.8, y = per)
) +
  # フロー：左(Q42.4)の色 → このスケールに凡例を出す
  geom_alluvium(aes(fill = Q42.4), width = 1/12, alpha = 0.6, discern = TRUE, show.legend = TRUE) +
  
  # 左ストラタ（Q42.4）：凡例あり
  geom_stratum(
    aes(x = 1, stratum = Q42.4, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 1, stratum = Q42.4,
      label = after_stat(ifelse(!is.na(lab_map_Q42_4[as.character(stratum)]),
                                lab_map_Q42_4[as.character(stratum)],
                                as.character(stratum))))
    , size = 7, color = "grey25", check_overlap = TRUE
  ) +
  # ← この scale_fill_manual が「左(=フロー含む)用」。凡例タイトルは空に。
  scale_fill_manual(values = pal1, name = "Vaccination in future pandemic") +
  
  
  
  
  # ===== ここで中央用にスケールをリセット（重要！） =====
new_scale_fill() +
  
  # 中央ストラタ（Q42.5）：色は付けるが凡例は出さない
  geom_stratum(
    aes(x = 2, stratum = Q42.5, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 2, stratum = Q42.5,
      label = after_stat(ifelse(!is.na(lab_map_Q42_5[as.character(stratum)]),
                                lab_map_Q42_5[as.character(stratum)],
                                as.character(stratum))))
    , size = 7, coro = "gray25", check_overlap = TRUE
  ) +
  scale_fill_manual(values = pal2, guide = "none") +
  
  
  
  # ===== ここで中央用にスケールをリセット（重要！） =====
new_scale_fill() +
  
  # 中央ストラタ（Q42.6）：色は付けるが凡例は出さない
  geom_stratum(
    aes(x = 3, stratum = Q42.6, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 3, stratum = Q42.6,
      label = after_stat(ifelse(!is.na(lab_map_Q42_6[as.character(stratum)]),
                                lab_map_Q42_6[as.character(stratum)],
                                as.character(stratum))))
    , size = 7, coro = "gray25", check_overlap = TRUE
  ) +
  scale_fill_manual(values = pal3, guide = "none") +
  
  
  
  # ===== ここで中央用にスケールをリセット（重要！） =====
new_scale_fill() +
  
  # 中央ストラタ（Q42.7）：色は付けるが凡例は出さない
  geom_stratum(
    aes(x = 4, stratum = Q42.7, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 4, stratum = Q42.7,
      label = after_stat(ifelse(!is.na(lab_map_Q42_7[as.character(stratum)]),
                                lab_map_Q42_7[as.character(stratum)],
                                as.character(stratum))))
    , size = 7, coro = "gray25", check_overlap = TRUE
  ) +
  scale_fill_manual(values = pal4, guide = "none") +
  
  
  
  
  
  
  
  # ===== 右用に再度スケールをリセット =====
new_scale_fill() +
  
  # 右ストラタ（Q42.8）：色は付けるが凡例は出さない
  geom_stratum(
    aes(x = 5, stratum = Q42.8, fill = after_stat(stratum)),
    width = 1/8, color = "grey60"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      x = 5, stratum = Q42.8,
      label = after_stat(ifelse(!is.na(lab_map_Q42_8[as.character(stratum)]),
                                lab_map_Q42_8[as.character(stratum)],
                                as.character(stratum))))
    , , size = 7, coro = "gray25", check_overlap = TRUE
  ) +
  scale_fill_manual(values = pal5, guide = "none") +
  
  scale_x_discrete(
    limits = c("Several\nmonths","A half\nyear", "One\nyear", "Several\nyears", "Lifelong"),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  labs(x = NULL, y = "Percentage", title = "") +
  theme_minimal() +
  # Change font size of x and y titles and labels, and legend font
  theme(
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24)
  ) +
  # Font color
  theme(
    text = element_text(color = "gray25")
    # axis.text = element_text(color = "black"),
    # axis.title = element_text(color = "black"),
    # legend.text = element_text(color = "black"),
    # legend.title = element_text(color = "black")
  )

#1400x1200

######### Mosaic ABCD #########

### Mosaic plot
# x = Q39. y = Q42.1 (proportion)
# Draw graph

df_plot <- data %>%
  select(Q39, Q42.1)

df_plot$Q39 <- as.factor(df_plot$Q39)
df_plot$Q42.1 <- as.factor(df_plot$Q42.1)

df_plot <- df_plot %>%
  mutate(name39 = case_when(
    Q39 == 1 ~ "Vaccinated\nin 2021",
    Q39 == 2 ~ "Vaccinated\nafter 2021",
    Q39 == 3 ~ "Wanted\nbut unable",
    Q39 == 4 | Q39 == 5 ~ "Unvaccinated",
    TRUE ~ NA))
df_plot$name39 <- factor(df_plot$name39, levels = c("Vaccinated\nin 2021", "Vaccinated\nafter 2021", "Wanted\nbut unable", "Unvaccinated"))

df_plot <- df_plot %>%
  mutate(name42 = case_when(
    Q42.1 == 1 ~ "Definitely will",
    Q42.1 == 2 ~ "Probably will",
    Q42.1 == 3 ~ "Probably won't",
    Q42.1 == 4 ~ "Definitely won't",
    TRUE ~ NA))
df_plot$name42 <- factor(df_plot$name42, levels = c("Definitely will", "Probably will", "Probably won't", "Definitely won't"))

# tab <- table(df_plot$name39, df_plot$name42)
# 
# mosaicplot(tab,
#            color = c("salmon", "pink", , "skyblue", "blue",),
#            main = "",
#            xlab = "COVID-19 vaccine", ylab = "Vaccination for next pandemic")

# df_summary <- df_plot %>%
#   count(name39, name42) %>%
#   group_by(name39) %>%
#   mutate(perc = n / sum(n) * 100,
#          ypos = cumsum(n) - n/2)  # 縦位置（モザイク用）

color_pal <- c(
  "#66B2FF",
  "#89CFF0", # pastel light blue
  "#FFD1DC", # pastel pink
  # "#77DD77", # pastel green
  "#FF6961"
)

p <- ggplot(df_plot) +
  geom_mosaic(aes(x = product(name39), fill = name42, weight = 1)) +
  scale_fill_manual(
    values = color_pal,
    name = "次のパンデミックでの\nワクチン接種意向",
    # guide = guide_legend(reverse = TRUE)
    guide = "none"
  ) +
  labs(x = "COVID-19 vaccination", y = "Vaccination in future pandemic") +
  theme_bw()

p

# クロス集計（name39×name42）と、name39内の割合 prop、name39の総数 n_x
tab <- df_plot %>%
  count(name39, name42, name = "n") %>%
  group_by(name39) %>%
  mutate(n_x = sum(n), prop = n / n_x) %>%
  ungroup()

# 横幅（各 name39 の幅）と x 位置（累積）
xpos <- tab %>%
  distinct(name39, n_x) %>%
  arrange(name39) %>%  # factor順のまま
  mutate(
    width = n_x / sum(n_x),
    x_min = lag(cumsum(width), default = 0),
    x_max = x_min + width,
    x_mid = (x_min + x_max) / 2
  ) %>%
  select(name39, x_min, x_max, x_mid)

# 各タイルの (xmin, xmax, ymin, ymax) とラベル位置 (x_mid, y_mid)
rects <- tab %>%
  left_join(xpos, by = "name39") %>%
  group_by(name39) %>%
  arrange(name42, .by_group = TRUE) %>%  # 縦積みの順序＝name42のfactor順
  mutate(
    y_min = lag(cumsum(prop), default = 0),
    y_max = y_min + prop,
    y_mid = (y_min + y_max) / 2
  ) %>%
  ungroup() %>%
  mutate(
    label = paste0(round(prop * 100, 1), "%\n(", n, ")"),
    # label = if_else(prop < 0.04, "", label)  # 4%未満はラベル非表示（任意）
  )

rects$y_mid[9] <- 0.035
rects$y_mid[13] <- 0.035
rects$y_mid[14] <- 0.115

# 軸ラベル用（x 軸の目盛り位置と表示名）
xbreaks <- xpos$x_mid
xlabels <- as.character(xpos$name39)

# プロット
ggplot(rects) +
  geom_rect(
    aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = name42),
    color = "white", linewidth = 0.3
  ) +
  scale_fill_manual(
    values = color_pal,
    name = "Vaccination in future pandemic",
    guide = guide_legend(reverse = TRUE)
    # guide = "none"
  ) +
  geom_text(
    aes(x = x_mid, y = y_mid, label = label),
    size = 7, color = "black"
  ) +
  scale_x_continuous(breaks = xbreaks, labels = xlabels, expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  labs(x = "COVID-19 vaccination", y = "Proportion") +
  theme_bw() +
  # Change font size of axis label, title, legend
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )

###### Excluding Q39 = 3 ######

### Mosaic plot
# x = Q39. y = Q42.1 (proportion)
# Draw graph

data_temp <- data %>% filter(Q39 != 3)

df_plot <- data_temp %>%
  select(Q39, Q42.1)

df_plot$Q39 <- as.factor(df_plot$Q39)
df_plot$Q42.1 <- as.factor(df_plot$Q42.1)

df_plot <- df_plot %>%
  mutate(name39 = case_when(
    Q39 == 1 ~ "Vaccinated\nin 2021",
    Q39 == 2 ~ "Vaccinated\nafter 2021",
    # Q39 == 3 ~ "Wanted\nbut unable",
    Q39 == 4 | Q39 == 5 ~ "Unvaccinated",
    TRUE ~ NA))
df_plot$name39 <- factor(df_plot$name39, levels = c("Vaccinated\nin 2021", "Vaccinated\nafter 2021", "Unvaccinated"))

df_plot <- df_plot %>%
  mutate(name42 = case_when(
    Q42.1 == 1 ~ "Definitely will",
    Q42.1 == 2 ~ "Probably will",
    Q42.1 == 3 ~ "Probably won't",
    Q42.1 == 4 ~ "Definitely won't",
    TRUE ~ NA))
df_plot$name42 <- factor(df_plot$name42, levels = c("Definitely will", "Probably will", "Probably won't", "Definitely won't"))

# tab <- table(df_plot$name39, df_plot$name42)
# 
# mosaicplot(tab,
#            color = c("salmon", "pink", , "skyblue", "blue",),
#            main = "",
#            xlab = "COVID-19 vaccine", ylab = "Vaccination for next pandemic")

# df_summary <- df_plot %>%
#   count(name39, name42) %>%
#   group_by(name39) %>%
#   mutate(perc = n / sum(n) * 100,
#          ypos = cumsum(n) - n/2)  # 縦位置（モザイク用）

color_pal <- c(
  "#66B2FF",
  "#89CFF0", # pastel light blue
  "#FFD1DC", # pastel pink
  # "#77DD77", # pastel green
  "#FF6961"
)

p <- ggplot(df_plot) +
  geom_mosaic(aes(x = product(name39), fill = name42, weight = 1)) +
  scale_fill_manual(
    values = color_pal,
    name = "次のパンデミックでの\nワクチン接種意向",
    # guide = guide_legend(reverse = TRUE)
    guide = "none"
  ) +
  labs(x = "COVID-19 vaccination", y = "Vaccination in next pandemic") +
  theme_bw()

p

# クロス集計（name39×name42）と、name39内の割合 prop、name39の総数 n_x
tab <- df_plot %>%
  count(name39, name42, name = "n") %>%
  group_by(name39) %>%
  mutate(n_x = sum(n), prop = n / n_x) %>%
  ungroup()

# 横幅（各 name39 の幅）と x 位置（累積）
xpos <- tab %>%
  distinct(name39, n_x) %>%
  arrange(name39) %>%  # factor順のまま
  mutate(
    width = n_x / sum(n_x),
    x_min = lag(cumsum(width), default = 0),
    x_max = x_min + width,
    x_mid = (x_min + x_max) / 2
  ) %>%
  select(name39, x_min, x_max, x_mid)

# 各タイルの (xmin, xmax, ymin, ymax) とラベル位置 (x_mid, y_mid)
rects <- tab %>%
  left_join(xpos, by = "name39") %>%
  group_by(name39) %>%
  arrange(name42, .by_group = TRUE) %>%  # 縦積みの順序＝name42のfactor順
  mutate(
    y_min = lag(cumsum(prop), default = 0),
    y_max = y_min + prop,
    y_mid = (y_min + y_max) / 2
  ) %>%
  ungroup() %>%
  mutate(
    label = paste0(round(prop * 100, 1), "%\n(", n, ")"),
    # label = if_else(prop < 0.04, "", label)  # 4%未満はラベル非表示（任意）
  )

rects$y_mid[9] <- 0.05
rects$y_mid[10] <- 0.155
rects$y_mid[11] <- 0.32

# 軸ラベル用（x 軸の目盛り位置と表示名）
xbreaks <- xpos$x_mid
xlabels <- as.character(xpos$name39)

# プロット
ggplot(rects) +
  geom_rect(
    aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = name42),
    color = "white", linewidth = 0.3
  ) +
  scale_fill_manual(
    values = color_pal,
    name = "Vaccination in future pandemic",
    guide = guide_legend(reverse = TRUE)
    # guide = "none"
  ) +
  geom_text(
    aes(x = x_mid, y = y_mid, label = label),
    size = 8, color = "gray25"
  ) +
  scale_x_continuous(breaks = xbreaks, labels = xlabels, expand = c(0, 0)) +
  # scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  # expand_limits(y = 1.05)  +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 1)) +

  labs(x = "COVID-19 vaccination", y = "Proportion") +
  theme_bw() +
  # Change font size of axis label, title, legend
  theme(
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 24),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24)
  ) +
  # Font color
  theme(
    text = element_text(color = "gray25")
    # axis.text = element_text(color = "black"),
    # axis.title = element_text(color = "black"),
    # legend.text = element_text(color = "black"),
    # legend.title = element_text(color = "black")
  ) +
  theme(panel.border = element_blank())

#1600x1000

