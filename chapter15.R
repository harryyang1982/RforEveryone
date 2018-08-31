# 15.1 Binding Rows and Columns

library(tidyverse)

sportLeague <- tibble(sport = c("Hockey", "Baseball", "Football"),
                      league = c("NHL", "MLB", "NFL"))

trophy <- tibble(trophy = c("Stanley Cup", "Commissioner's Trophy",
                            "Vince Lombardi Trophy"))

trophies1 <- bind_cols(sportLeague, trophy)

trophies2 <- tribble(
  ~sport, ~league, ~trophy,
  "Basketball", "NBA", "Larry O'Brien Championship Trophy",
  "Golf", "PGA", "Wanamaker Trophy"
)

trophies <- bind_rows(trophies1, trophies2)

trophies

# 15.2 Joins with dplyr

library(readr)
colorsURL <- "http://www.jaredlander.com/data/DiamondColors.csv"
diamondColors <- read_csv(colorsURL)

diamondColors

data(diamonds)

unique(diamonds$color)

diamonds %>% 
  left_join(diamondColors, by = c("color" = "Color")) %>% 
  select(carat, color, price, Description, Details)

diamonds %>% 
  left_join(diamondColors, by = c("color" = "Color")) %>% 
  distinct(color, Description)

diamondColors %>% 
  distinct(Color, Desciption)

diamonds %>% 
  right_join(diamondColors, by = c("color" = "Color")) %>% 
  nrow

diamonds %>% 
  nrow

all.equal(
  diamonds %>% left_join(diamondColors, by = c("color" = "Color")),
  diamonds %>% inner_join(diamondColors, by = c("color" = "Color"))
)

all.equal(
  diamonds %>% 
    right_join(diamondColors, by = c("color" = "Color")),
  diamonds %>% 
    full_join(diamondColors, by = c("color" = "Color"))
)

diamondColors %>% semi_join(diamonds, by = c("Color" = "color"))

diamondColors %>% 
  anti_join(diamonds, by = c("Color" = "color"))

diamondColors %>% 
  filter(Color %in% unique(diamonds$color))

diamondColors %>% 
  filter(!Color %in% unique(diamonds$color))

# 15.3 Converting Data Formats

library(tidyverse)

emotion <- read_tsv("http://www.jaredlander.com/data/reaction.txt")
emotion

emotion %>% 
  gather(Type, Measurement, Age, BMI, React, Regulate)

emotionLong <- emotion %>% 
  gather(Type, Measurement, Age, BMI, React, Regulate) %>% 
  arrange(ID)

head(emotionLong, 20)

emotion %>% 
  gather(Type, Measurement, -ID, -Test, -Gender)

identical(emotion %>% 
            gather(Type, Measurement, Age, BMI, React, Regulate),
          emotion %>% 
            gather(Type, Measurement, -ID, -Test, -Gender)
)

emotionLong %>% 
  spread(Type, Measurement)

