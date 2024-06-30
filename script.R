# Import library ----
# install.packages("stringr")
library(stringr)

# install.packages("lubridate")
library(lubridate)

# install.packages("dplyr")
library(dplyr)

# install.packages("ggplot2")
library(ggplot2)

# install.packages("grid")
library(grid)

# install.packages("gridExtra")
library(gridExtra)

# install.packages("ggridges")
library(ggridges)

# install.packages("ggpubr")
library(ggpubr)

# install.packages("stats")
library(stats)

# install.packages("glmtoolbox")
library(glmtoolbox)


# Variables ----
dis_marathon <- 42195
age_group <- NA


# Fonction Utiles ----

# Fonction pour convertir un temps au format "h:mm:ss.ms" en secondes
convert_to_seconds <- function(time_string) {
  parts <- strsplit(time_string, "[:,.]")[[1]]
  
  hours <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  milliseconds <- as.numeric(parts[4])
  
  total_seconds <- hours * 3600 + minutes * 60 + seconds + milliseconds / 1000
  
  return(total_seconds)
}

# Import dataset & function ----
resultats_femmes <- read.csv("~/Desktop/BUT SD/SAE/DomainesAppplication/DATA/resultats_femmes.csv", sep=";")
resultats_hommes <- read.csv("~/Desktop/BUT SD/SAE/DomainesAppplication/DATA/resultats_hommes.csv", sep=";")
resultats_echecs <- read.csv("~/Desktop/BUT SD/SAE/DomainesAppplication/DATA/resultats_joueurs_echecs.csv", sep=";", fileEncoding = "ISO-8859-13")
source("~/Desktop/BUT SD/SAE/DomainesAppplication/fonction moindre carrés.R")
source("~/Desktop/BUT SD/SAE/DomainesAppplication/fonctionsGraphs.R")

# Nettoyage ----
# Nettoyage Femmes ----

resultats_femmes <- resultats_femmes[-7]

# Nettoyage Femmes : NA ----
table(is.na(resultats_femmes))
resultats_femmes <- na.omit(resultats_femmes)
table(is.na(resultats_femmes))


# Nettoyage Femmes : distance ----
resultats_femmes$epreuve <- resultats_femmes$dis

resultats_femmes$dis <- case_when(
  resultats_femmes$dis == "middle-long/10000-metres" ~ 10000,
  resultats_femmes$dis == "middle-long/1500-metres" ~ 1500,
  resultats_femmes$dis == "middle-long/3000-metres" ~ 3000,
  resultats_femmes$dis == "middle-long/3000-metres-steeplechase" ~ 3000,
  resultats_femmes$dis == "middle-long/5000-metres" ~ 5000,
  resultats_femmes$dis == "middle-long/800-metres" ~ 800,
  resultats_femmes$dis == "road-running/10-kilometres" ~ 10000,
  resultats_femmes$dis == "road-running/half-marathon" ~ 21097.5,
  resultats_femmes$dis == "road-running/marathon" ~ 42195,
  .default = NA,
  TRUE ~ as.numeric(resultats_femmes$dis)
)

# table(resultats_femmes$dis, exclude = NULL)
# table(resultats_femmes$epreuve, exclude = NULL)



# Nettoyage Femmes : DOB ----
resultats_femmes <- resultats_femmes[!is.na(resultats_femmes$DOB)
                                            & resultats_femmes$DOB != "", ]

resultats_femmes$DOB <- case_when(
  nchar(resultats_femmes$DOB) == 8 ~ paste("01", resultats_femmes$DOB, sep = " "),
  nchar(resultats_femmes$DOB) == 4 ~ paste("01","JAN", resultats_femmes$DOB, sep = " "),
  .default = NA,
  TRUE ~ as.character(resultats_femmes$DOB)
)

resultats_femmes$DOB <- case_when(
  nchar(resultats_femmes$DOB) == 11 ~ format(as.Date(resultats_femmes$DOB, format = "%d %b %Y"), "%Y-%m-%d"),
  nchar(resultats_femmes$DOB) == 8 ~ format(as.Date(resultats_femmes$DOB, format = "%d %b %Y"), "%Y-%m-%d"),
  nchar(resultats_femmes$DOB) == 4 ~ format(as.Date(resultats_femmes$DOB, format = "%d %b %Y"), "%Y-%m-%d"),
  .default = NA,
)


resultats_femmes$DOB <- as.Date(resultats_femmes$DOB)

# class(resultats_femmes$DOB)
# resultats_femmes[which(is.na(resultats_femmes$DOB)), ]
# table(resultats_femmes$DOB)
# resultats_femmes$DOB[47]



# Nettoyage Femmes : Date ----
resultats_femmes <- resultats_femmes[!is.na(resultats_femmes$Date)
                                     & resultats_femmes$Date != "", ]

resultats_femmes$Date <- case_when(
  nchar(resultats_femmes$Date) == 8 ~ paste("01", resultats_femmes$Date, sep = " "),
  nchar(resultats_femmes$Date) == 4 ~ paste("01","JAN", resultats_femmes$Date, sep = " "),
  .default = NA,
  TRUE ~ as.character(resultats_femmes$Date)
)

resultats_femmes$Date <- case_when(
  nchar(resultats_femmes$Date) == 11 ~ format(as.Date(resultats_femmes$Date, format = "%d %b %Y"), "%Y-%m-%d"),
  nchar(resultats_femmes$Date) == 8 ~ format(as.Date(resultats_femmes$Date, format = "%d %b %Y"), "%Y-%m-%d"),
  nchar(resultats_femmes$Date) == 4 ~ format(as.Date(resultats_femmes$Date, format = "%d %b %Y"), "%Y-%m-%d"),
  .default = NA,
)


resultats_femmes$Date <- as.Date(resultats_femmes$Date)

# class(resultats_femmes$Date)
# resultats_femmes[which(is.na(resultats_femmes$Date)), ]
# table(resultats_femmes$Date)
# resultats_femmes$Date[47]

# Nettoyage Femmes : Time ----
resultats_femmes$Mark <- as.character(resultats_femmes$Mark)

resultats_femmes$Mark[which(resultats_femmes$epreuve == "road-running/10-kilometres")] <- substr(resultats_femmes$Mark[which(resultats_femmes$epreuve == "road-running/10-kilometres")], 0, 5)


resultats_femmes$Mark <- case_when(
  str_count(resultats_femmes$Mark, ":") == 2 ~ paste(resultats_femmes$Mark, "00", sep = "."),
  str_count(resultats_femmes$Mark, ":") == 1 ~ paste("0", resultats_femmes$Mark, sep = ":"),
  .default = NA
)

resultats_femmes$Mark <- case_when(
  substr(resultats_femmes$Mark, nchar(resultats_femmes$Mark), nchar(resultats_femmes$Mark)) == "h" ~ {
    modified_mark <- paste(substr(resultats_femmes$Mark, 1, nchar(resultats_femmes$Mark) - 1), "0", sep = "")
    modified_mark
  },
  TRUE ~ resultats_femmes$Mark
)

resultats_femmes$Mark <- case_when(
  str_count(resultats_femmes$Mark, ":") == 2 ~ paste(resultats_femmes$Mark, "00", sep = "."),
  str_count(resultats_femmes$Mark, ":") == 1 ~ paste("0", resultats_femmes$Mark, sep = ":"),
  .default = NA
)

class(resultats_femmes$Mark)
resultats_femmes[which(is.na(resultats_femmes$Mark)), ]
# table(resultats_femmes$Mark)


# ----
# Nettoyage Hommes ----

resultats_hommes <- resultats_hommes[-7]

# Nettoyage Hommes : NA ----
table(is.na(resultats_hommes))
resultats_hommes <- na.omit(resultats_hommes)
table(is.na(resultats_hommes))


# Nettoyage Hommes : distance ----
resultats_hommes$epreuve <- resultats_hommes$dis

resultats_hommes$dis <- case_when(
  resultats_hommes$dis == "middle-long/10000-metres" ~ 10000,
  resultats_hommes$dis == "middle-long/1500-metres" ~ 1500,
  resultats_hommes$dis == "middle-long/3000-metres" ~ 3000,
  resultats_hommes$dis == "middle-long/3000-metres-steeplechase" ~ 3000,
  resultats_hommes$dis == "middle-long/5000-metres" ~ 5000,
  resultats_hommes$dis == "middle-long/800-metres" ~ 800,
  resultats_hommes$dis == "road-running/10-kilometres" ~ 10000,
  resultats_hommes$dis == "road-running/half-marathon" ~ 21097.5,
  resultats_hommes$dis == "road-running/marathon" ~ 42195,
  .default = NA,
  TRUE ~ as.numeric(resultats_hommes$dis)
)

# table(resultats_hommes$dis, exclude = NULL)
# table(resultats_hommes$epreuve, exclude = NULL)



# Nettoyage Hommes : DOB ----
resultats_hommes <- resultats_hommes[!is.na(resultats_hommes$DOB)
                                     & resultats_hommes$DOB != "", ]

resultats_hommes$DOB <- case_when(
  nchar(resultats_hommes$DOB) == 8 ~ paste("01", resultats_hommes$DOB, sep = " "),
  nchar(resultats_hommes$DOB) == 4 ~ paste("01","JAN", resultats_hommes$DOB, sep = " "),
  .default = NA,
  TRUE ~ as.character(resultats_hommes$DOB)
)

resultats_hommes$DOB <- case_when(
  nchar(resultats_hommes$DOB) == 11 ~ format(as.Date(resultats_hommes$DOB, format = "%d %b %Y"), "%Y-%m-%d"),
  nchar(resultats_hommes$DOB) == 8 ~ format(as.Date(resultats_hommes$DOB, format = "%d %b %Y"), "%Y-%m-%d"),
  nchar(resultats_hommes$DOB) == 4 ~ format(as.Date(resultats_hommes$DOB, format = "%d %b %Y"), "%Y-%m-%d"),
  .default = NA,
)


resultats_hommes$DOB <- as.Date(resultats_hommes$DOB)

# class(resultats_hommes$DOB)
# resultats_hommes[which(is.na(resultats_hommes$DOB)), ]
# table(resultats_hommes$DOB)
# resultats_hommes$DOB[47]



# Nettoyage Hommes : Date ----
resultats_hommes <- resultats_hommes[!is.na(resultats_hommes$Date)
                                     & resultats_hommes$Date != "", ]

resultats_hommes$Date <- case_when(
  nchar(resultats_hommes$Date) == 8 ~ paste("01", resultats_hommes$Date, sep = " "),
  nchar(resultats_hommes$Date) == 4 ~ paste("01","JAN", resultats_hommes$Date, sep = " "),
  .default = NA,
  TRUE ~ as.character(resultats_hommes$Date)
)

resultats_hommes$Date <- case_when(
  nchar(resultats_hommes$Date) == 11 ~ format(as.Date(resultats_hommes$Date, format = "%d %b %Y"), "%Y-%m-%d"),
  nchar(resultats_hommes$Date) == 8 ~ format(as.Date(resultats_hommes$Date, format = "%d %b %Y"), "%Y-%m-%d"),
  nchar(resultats_hommes$Date) == 4 ~ format(as.Date(resultats_hommes$Date, format = "%d %b %Y"), "%Y-%m-%d"),
  .default = NA,
)


resultats_hommes$Date <- as.Date(resultats_hommes$Date)

# class(resultats_hommes$Date)
# resultats_hommes[which(is.na(resultats_hommes$Date)), ]
# table(resultats_hommes$Date)
# resultats_hommes$Date[47]


# Nettoyage Hommes : Time ----
resultats_hommes$Mark <- as.character(resultats_hommes$Mark)

resultats_hommes$Mark <- case_when(
  str_count(resultats_hommes$Mark, ":") == 2 ~ paste(resultats_hommes$Mark, "00", sep = "."),
  str_count(resultats_hommes$Mark, ":") == 1 ~ paste("0", resultats_hommes$Mark, sep = ":"),
  .default = NA
)

resultats_hommes$Mark <- case_when(
  substr(resultats_hommes$Mark, nchar(resultats_hommes$Mark), nchar(resultats_hommes$Mark)) == "h" ~ {
    modified_mark <- paste(substr(resultats_hommes$Mark, 1, nchar(resultats_hommes$Mark) - 1), "0", sep = "")
    modified_mark
  },
  TRUE ~ resultats_hommes$Mark
)

resultats_hommes$Mark <- case_when(
  str_count(resultats_hommes$Mark, ":") == 2 ~ paste(resultats_hommes$Mark, "00", sep = "."),
  str_count(resultats_hommes$Mark, ":") == 1 ~ paste("0", resultats_hommes$Mark, sep = ":"),
  .default = NA
)

class(resultats_hommes$Mark)
resultats_hommes[which(is.na(resultats_hommes$Mark)), ]
table(resultats_hommes$Mark)


# ----
# Nettoyage Echecs ----
names(resultats_echecs) <- c("Annee", "Mois", "Numero", "Nom", "Points", "DOB", "DM")
resultats_echecs$Points <- as.numeric(resultats_echecs$Points)

# Nettoyage Echecs : NA ----
resultats_echecs = na.omit(resultats_echecs[resultats_echecs$DOB != "", ])

# Nettoyage Echecs : Donnees abérrantes ----
resultats_echecs <- resultats_echecs[which(resultats_echecs$Nom != "Vladislav Shianovsky"), ]

resultats_echecs$DOB <- str_trim(resultats_echecs$DOB)
# Nettoyage Echecs : DOB ----
resultats_echecs$DOB <- case_when(
  nchar(resultats_echecs$DOB) == 8 ~ paste(resultats_echecs$DOB, "01", sep = "-"),
  nchar(resultats_echecs$DOB) == 4 ~ paste(resultats_echecs$DOB, "Jan", "01", sep = "-"),
  .default = NA,
  TRUE ~ as.character(resultats_echecs$DOB)
)


resultats_echecs$DOB <- format(as.Date(resultats_echecs$DOB, format = "%Y-%b-%d"), "%Y-%m-%d")
resultats_echecs$DOB <- as.Date(resultats_echecs$DOB)
class(resultats_echecs$DOB)


# Nettoyage Echecs : Year ----
resultats_echecs$Annee <- paste(resultats_echecs$Annee, "01", "01", sep="-")


# ----
# Ajout Var. ----
# Ajout Var. : Age ----
resultats_femmes$age <- round(as.numeric(difftime(Sys.Date(), resultats_femmes$DOB, units = "days") / 365.25))
resultats_hommes$age <- round(as.numeric(difftime(Sys.Date(), resultats_hommes$DOB, units = "days") / 365.25))
resultats_echecs$age <- round(as.numeric(difftime(Sys.Date(), resultats_echecs$DOB, units = "days") / 365.25))


# Ajout Var. : Age at epreuve ----
resultats_femmes$age_at_ep <- round(as.numeric(resultats_femmes$Date - resultats_femmes$DOB) / 365.25)

resultats_hommes$age_at_ep <- round(as.numeric(resultats_hommes$Date - resultats_hommes$DOB) / 365.25)

resultats_echecs$age_at_ep <- as.numeric(year(as.Date(resultats_echecs$Annee)) - year(resultats_echecs$DOB))



resultats_femmes <- resultats_femmes[which(resultats_femmes$age_at_ep <= 100 ), ]
resultats_hommes <- resultats_hommes[which(resultats_hommes$age_at_ep <= 100 ), ]
resultats_echecs <- resultats_echecs[which(resultats_echecs$age_at_ep <= 100 ), ]

# Ajout Var. : Groupe Age ----
resultats_femmes <- resultats_femmes %>%
  mutate(age_grp = case_when(
    age %in% 0:10 ~ "0-10",
    between(age, 10, 20) ~ "10-20",
    between(age, 20, 30) ~ "20-30",
    between(age, 30, 40) ~ "30-40",
    between(age, 40, 50) ~ "40-50",
    between(age, 50, 60) ~ "50-60",
    age > 60 ~ "60+",
    TRUE ~ NA
    )
  )

resultats_hommes <- resultats_hommes %>%
  mutate(age_grp = case_when(
    age %in% 0:10 ~ "0-10",
    between(age, 10, 20) ~ "10-20",
    between(age, 20, 30) ~ "20-30",
    between(age, 30, 40) ~ "30-40",
    between(age, 40, 50) ~ "40-50",
    between(age, 50, 60) ~ "50-60",
    age > 60 ~ "60+",
    TRUE ~ NA
  )
  )

resultats_echecs <- resultats_echecs %>%
  mutate(age_grp = case_when(
    age %in% 0:10 ~ "0-10",
    between(age, 10, 20) ~ "10-20",
    between(age, 20, 30) ~ "20-30",
    between(age, 30, 40) ~ "30-40",
    between(age, 40, 50) ~ "40-50",
    between(age, 50, 60) ~ "50-60",
    age > 60 ~ "60+",
    TRUE ~ NA
  )
  )

# Ajout Var. : Seconds ----

resultats_femmes$seconds <- sapply(resultats_femmes$Mark, convert_to_seconds)
resultats_hommes$seconds <- sapply(resultats_hommes$Mark, convert_to_seconds)

table(is.na(resultats_femmes$seconds))
resultats_femmes[which(is.na(resultats_femmes$seconds)), ]

# Ajout Var. : Speed ----
resultats_femmes$speed <- resultats_femmes$dis / resultats_femmes$seconds
resultats_hommes$speed <- resultats_hommes$dis / resultats_hommes$seconds




# Nouveau Dataset : BestSpeed ----
best_femmes <- resultats_femmes %>%
  group_by(age_at_ep, epreuve) %>%
  filter(speed == max(speed))

best_hommes <- resultats_hommes %>%
  group_by(age_at_ep, epreuve) %>%
  filter(speed == max(speed))

best_echecs <- resultats_echecs %>%
  group_by(age_at_ep) %>%
  filter(Points == max(Points))

best_echecs$epreuve <- "echecs"

# Graphs ----
# Graphs : Best by year and ep.
best_femmes %>%
  ggplot() +
  aes(x = age_at_ep, y = speed) +
  # geom_smooth(method = "loess", se = FALSE, color = "purple4", size = 1) +
  geom_point(shape = "circle", size = 1.6, colour = "#E683F6") +
  labs(
    x = "Age",
    y = "Speed",
    title = "Meilleurs performance par âge - Femmes",
    subtitle = "et par épreuve",
    caption = "vitesse en m.s-1"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 22L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic",
                                 hjust = 0.5),
    axis.title.y = element_text(size = 13L,
                                face = "bold"),
    axis.title.x = element_text(size = 13L,
                                face = "bold")
  ) +
  facet_wrap(vars(epreuve), scales = "free_y") +
  xlim(5, 70)

best_hommes %>%
  ggplot() +
  aes(x = age_at_ep, y = speed) +
  # geom_smooth(method = "loess", se = FALSE, color = "purple4", size = 1) +
  geom_point(shape = "circle", size = 1.6, colour = "#E683F6") +
  labs(
    x = "Age",
    y = "Speed",
    title = "Meilleurs performance par âge - Hommes",
    subtitle = "et par épreuve",
    caption = "vitesse en m.s-1"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 22L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic",
                                 hjust = 0.5),
    axis.title.y = element_text(size = 13L,
                                face = "bold"),
    axis.title.x = element_text(size = 13L,
                                face = "bold")
  ) +
  facet_wrap(vars(epreuve), scales = "free_y") +
  xlim(5, 70)

tmp1 <- best_femmes |> mutate(sexe = "F")
tmp2 <- best_hommes |> mutate(sexe = "M")

best_all <- bind_rows(tmp1, tmp2)
best_all$sexe <- as.factor(best_all$sexe)
rm(tmp1, tmp2)

best_all %>%
  ggplot() +
  aes(x = age_at_ep, y = speed, colour = sexe) +
  geom_point(shape = "circle", size = 1.6) +
  labs(
    x = "Âge",
    y = "Vitesse",
    title = "Meilleurs performance par âge",
    subtitle = "et par épreuve",
    caption = "vitesse en m.s-1"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 22L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic",
                                 hjust = 0.5),
    axis.title.y = element_text(size = 13L,
                                face = "bold"),
    axis.title.x = element_text(size = 13L,
                                face = "bold")
  ) +
  facet_wrap(vars(epreuve), scales = "free_y") +
  xlim(5, 70)


best_echecs %>%
  ggplot() +
  aes(x = age_at_ep, y = Points) +
  # geom_smooth(method = "loess", se = FALSE, color = "purple4", size = 1.5) +
  geom_point(shape = "circle", size = 3.6, colour = "#E683F6") +
  labs(
    x = "Age",
    y = "Points",
    title = "Meilleurs performance par âge - Échecs",
    subtitle = "",
    caption = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 22L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic",
                                 hjust = 0.5),
    axis.title.y = element_text(size = 13L,
                                face = "bold"),
    axis.title.x = element_text(size = 13L,
                                face = "bold")
  ) +
  xlim(10, 80)



# Modèles ----
list_epreuve <- unique(best_femmes$epreuve)
# Modèles : linéaires ----
# Définir la fonction modèle linéaire

graph_lineaire(dataset = best_femmes, x.axis = "age_at_ep", y.axis = "speed", epreuve = list_epreuve, x.label="Age", printTable=FALSE, ncol=3, point.size=3.3, point.col="#FFAEFE", line.size=1.5, line.col="#AEDFFF")

graph_lineaire(dataset = best_hommes, x.axis = "age_at_ep", y.axis = "speed", epreuve = list_epreuve, x.label="Age", printTable=FALSE, ncol=3, point.size=3.3, point.col="#FFADAD", line.size=1.5, line.col="#AEDFFF")

graph_lineaire(dataset = best_echecs, x.axis = "age_at_ep", y.axis = "Points", epreuve = "echecs", x.label="Age", printTable=FALSE, ncol=1, point.size=3.3, point.col="#E683F6", line.size=1.5, line.col="#AEDFFF")


# Modèles : quadratique ----

graph_quadratique(dataset = best_femmes,
                  x.axis = "age_at_ep",
                  y.axis = "speed",
                  epreuve = list_epreuve,
                  x.label="Age",
                  point.size=3.3,
                  point.col="#FFAEFE",
                  point.shape = 16,
                  line.size=1.5,
                  line.col="#b2aeff",
                  ncol=3,
                  printTable=FALSE,
                  valueTable = TRUE,
                  table_name="result_quad_F")


graph_quadratique(dataset = best_hommes,
                  x.axis = "age_at_ep",
                  y.axis = "speed",
                  epreuve = list_epreuve,
                  x.label="Age",
                  point.size=3.3,
                  point.col="#FFADAD",
                  point.shape = 16,
                  line.size=1.5,
                  line.col="#b2aeff",
                  ncol=3,
                  printTable=FALSE,
                  valueTable = TRUE,
                  table_name="result_quad_H")

graph_quadratique(dataset = best_echecs,
                  x.axis = "age_at_ep",
                  y.axis = "Points",
                  epreuve = "echecs",
                  x.label="Age",
                  point.size=3.3,
                  point.col="#E683F6",
                  point.shape = 16,
                  line.size=1.5,
                  line.col="#b2aeff",
                  ncol=1,
                  printTable=FALSE,
                  valueTable = TRUE,
                  table_name="result_quad_echecs")



# Modèles : Moore ----
graph_moore(dataset = best_femmes,
            x.axis = "age_at_ep",
            y.axis = "speed",
            epreuve = list_epreuve,
            x.label="Age",
            printTable=FALSE,
            ncol=3,
            point.size=3.3,
            point.col="#FFAEFE",
            point.shape = 16,
            line.size=1.5,
            line.col="#8e71e3",
            valueTable = TRUE,
            table_name="result_moore_F")

graph_moore(dataset = best_hommes,
            x.axis = "age_at_ep",
            y.axis = "speed",
            epreuve = list_epreuve,
            x.label="Age",
            printTable=FALSE,
            ncol=3,
            point.size=3.3,
            point.col="#FFADAD",
            point.shape = 16,
            line.size=1.5,
            line.col="#8e71e3",
            valueTable = TRUE,
            table_name="result_moore_H")

graph_moore(dataset = best_echecs,
            x.axis = "age_at_ep",
            y.axis = "Points",
            epreuve = "echecs",
            x.label="Age",
            printTable=FALSE,
            ncol=1,
            point.size=3.3,
            point.col="#E683F6",
            point.shape = 16,
            line.size=1.5,
            line.col="#8e71e3",
            valueTable = TRUE,
            table_name="result_moore_ECHECS")

# tmp <- best_femmes |>
#  filter(epreuve == "middle-long/3000-metres")

best_femmes <- best_femmes[which(best_femmes$age_at_ep > 6
                                 & best_femmes$age_at_ep <= 50),]

graph_moore(dataset = best_femmes,
            x.axis = "age_at_ep",
            y.axis = "speed",
            epreuve = list_epreuve,
            x.label="Age",
            printTable=FALSE,
            ncol=3,
            point.size=3.3,
            point.col="#FFAEFE",
            line.size=1.5,
            line.col="#8e71e3",
            valueTable = TRUE,
            table_name="result_moore_F")

# d = pente







library(formattable)
tmp <- result_quad_F
tmp[, 2:7] <- round(tmp[, 2:7], 2)
tmp <- tmp %>% mutate(across(residus.sum, ~ formatC(., format = "e", digits = 1)))
tmp[, 9:11] <- round(tmp[, 9:11], 2)

formattable(tmp,
            align = c("l", rep("c", 10)),
            list(),
            width = c(10, rep(0.3, ncol(tmp)-1)),
            table.attr = 'style="font-size: 15px;", class=\"table table-striped\", style="border: 1px solid black;";\"')
mean(tmp$R2_ajuste)


tmp <- result_quad_H
tmp[, 2:7] <- round(tmp[, 2:7], 2)
tmp <- tmp %>% mutate(across(residus.sum, ~ formatC(., format = "e", digits = 1)))
tmp[, 9:11] <- round(tmp[, 9:11], 2)

formattable(tmp,
            align = c("l", rep("c", 10)),
            list(),
            width = c(10, rep(0.3, ncol(tmp)-1)),
            table.attr = 'style="font-size: 15px;", class=\"table table-striped\", style="border: 1px solid black;";\"')
mean(tmp$R2_ajuste)







tmp <- result_moore_F
tmp[, 2:10] <- round(tmp[, 2:10], 2)
tmp <- tmp %>% mutate(across(residus.mean, ~ formatC(., format = "e", digits = 1)))
tmp[, 12:14] <- round(tmp[, 12:14], 2)

formattable(tmp,
            align = c("l", rep("c", 10)),
            list(),
            width = c(10, rep(0.3, ncol(tmp)-1)),
            table.attr = 'style="font-size: 15px;", class=\"table table-striped\", style="border: 1px solid black;";\"')
mean(tmp$R2_ajuste)


tmp <- result_moore_H
tmp[, 2:10] <- round(tmp[, 2:10], 2)
tmp <- tmp %>% mutate(across(residus.mean, ~ formatC(., format = "e", digits = 1)))
tmp[, 12:14] <- round(tmp[, 12:14], 2)

formattable(tmp,
            align = c("l", rep("c", 10)),
            list(),
            width = c(10, rep(0.3, ncol(tmp)-1)),
            table.attr = 'style="font-size: 15px;", class=\"table table-striped\", style="border: 1px solid black;";\"')
mean(tmp$R2_ajuste)






tmp <- result_quad_echecs
tmp[, 2:7] <- round(tmp[, 2:7], 2)
tmp <- tmp %>% mutate(across(residus.sum, ~ formatC(., format = "e", digits = 1)))
tmp[, 9:11] <- round(tmp[, 9:11], 2)

formattable(tmp,
            align = c("l", rep("c", 10)),
            list(),
            width = c(10, rep(0.3, ncol(tmp)-1)),
            table.attr = 'style="font-size: 15px;", class=\"table table-striped\", style="border: 1px solid black;";\"')
mean(tmp$R2_ajuste)

tmp <- result_moore_ECHECS
tmp[, 2:10] <- round(tmp[, 2:10], 2)
tmp <- tmp %>% mutate(across(residus.mean, ~ formatC(., format = "e", digits = 1)))
tmp[, 12:14] <- round(tmp[, 12:14], 2)

formattable(tmp,
            align = c("l", rep("c", 10)),
            list(),
            width = c(10, rep(0.3, ncol(tmp)-1)),
            table.attr = 'style="font-size: 15px;", class=\"table table-striped\", style="border: 1px solid black;";\"')
mean(tmp$R2_ajuste)
