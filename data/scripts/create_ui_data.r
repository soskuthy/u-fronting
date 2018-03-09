library(tidyverse)
library(here)

becker.vowels <- read_csv(here("data", "final_data", "becker_vowels.csv"))
glottolog <- read_csv(here("data", "raw_data", "glottolog.csv"))
english.as.primary <- read_csv(here("data", "raw_data", "english_as_primary.csv"))
glottolog <- glottolog %>% 
  dplyr::rename(ISO=iso639P3code) %>%
  dplyr::select(ISO, country_ids, latitude, longitude)

becker.uni.long <- becker.vowels %>% filter(Quantity %in% c("Uniform", "Long"))
unique(becker.uni.long$vowel)
front.high.rounded <- c("y", "ʉ", "u\u0308", "ü(y)")
y <- unlist(as.list(by(becker.uni.long$vowel, becker.uni.long$Language, function (x) {sum(front.high.rounded %in% x) > 0})))


becker.iyu <- becker.uni.long %>%
  filter(vowel %in% c("u", "i", front.high.rounded)) %>%
  mutate(y=y[Language], vowel=recode(vowel, `ʉ`="y", `ü`="y", `ü(y)`="y"))
  

becker.iyu <- merge(becker.iyu, glottolog, by="ISO")

eng.prim.countries <- subset(english.as.primary, Primary=="Yes")$Country_code

becker.iyu$eng.prim <- unlist(lapply(strsplit(becker.iyu$country_ids, " "), function (x) {length(intersect(x, eng.prim.countries)) >= (length(x) / 2)}))

write_csv(becker.iyu,here("data", "final_data", "becker_ui.csv"))
