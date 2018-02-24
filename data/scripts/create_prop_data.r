library(here)
library(tidyverse)
library(stringi)
library(stringr)

ids_regexps <- read_csv(here("data", "raw_data", "ids_regexps.csv"))
dat <- ids_regexps
dat$u.count <- 0
dat$y.count <- 0
dat$coronal.pre.u.count <- 0
dat$palatal.pre.u.count <- 0
dat$other.pre.u.count <- 0
dat$coronal.pre.y.count <- 0
dat$palatal.pre.y.count <- 0
dat$other.pre.y.count <- 0

for (lang in ids_regexps$ling) {
  
  print(lang)
  
  regexps <- ids_regexps[ids_regexps$ling==lang,]
  
  ldat <- read_csv(here("data", "raw_data", "ids_dataset", paste0(lang, ".csv")))
  eval(parse(text=regexps$cleaning))
  strs <- clean(c(ldat[,regexps$text.col])[[1]])
  
  
  coronal.pre <- stri_unescape_unicode(regexps$coronal.pre)
  palatal.pre <- stri_unescape_unicode(regexps$palatal.pre)
  u <- stri_unescape_unicode(regexps$u.regexp)
  y <- stri_unescape_unicode(regexps$y.regexp)
  
  coronal.pre.u <- paste0("(",coronal.pre,")", u)
  coronal.pre.y <- paste0("(",coronal.pre,")", y)
  palatal.pre.u <- paste0("(",palatal.pre,")", u)
  palatal.pre.y <- paste0("(",palatal.pre,")", y)
  other.pre.u <- paste0("(?<!", coronal.pre, "|", palatal.pre, ")", u)
  other.pre.y <- paste0("(?<!", coronal.pre, "|", palatal.pre, ")", y)
  
  dat$u.count[dat$ling==lang] <- sum(str_count(strs, u))
  dat$coronal.pre.u.count[dat$ling==lang] <- sum(str_count(strs, coronal.pre.u))
  dat$palatal.pre.u.count[dat$ling==lang] <- sum(str_count(strs, palatal.pre.u))
  dat$other.pre.u.count[dat$ling==lang] <- sum(str_count(strs, other.pre.u))
  
  dat$y.count[dat$ling==lang] <- sum(str_count(strs, y))
  dat$coronal.pre.y.count[dat$ling==lang] <- sum(str_count(strs, coronal.pre.y))
  dat$palatal.pre.y.count[dat$ling==lang] <- sum(str_count(strs, palatal.pre.y))
  dat$other.pre.y.count[dat$ling==lang] <- sum(str_count(strs, other.pre.y))
}



# generating proportions

dat$favouring.pre.u.prop <- (dat$coronal.pre.u.count + dat$palatal.pre.u.count) / dat$u.count
dat$favouring.pre.y.prop <- (dat$coronal.pre.y.count + dat$palatal.pre.y.count) / dat$y.count
dat$favouring.pre.u.count <- dat$coronal.pre.u.count + dat$palatal.pre.u.count
dat$favouring.pre.y.count <- dat$coronal.pre.y.count + dat$palatal.pre.y.count

# language has y?

dat$y <- !is.na(dat$y.regexp)

# merging with u data

becker.vowels <- read_csv(here("data", "final_data", "becker_vowels.csv"))

ids.props <- dplyr::rename(dat, language=ling)
ids.props <- dplyr::select(ids.props, 
                           -u.regexp, 
                           -y.regexp, 
                           -coronal.pre, 
                           -palatal.pre,
                           -coronal.post,
                           -palatal.post,
                           -text.col,
                           -cleaning,
                           -notes)
becker.u <- becker.vowels %>%
  filter(vowel=="u" & Quantity %in% c("Long", "Uniform")) %>%
  dplyr::rename(u.f1=f1, u.f2=f2, language=Language)

ids.props <- dplyr::left_join(ids.props, select(becker.u, language, u.f2), by="language")

# save

write_csv(ids.props, here("data", "final_data", "ids_props.csv"))

### smaller data set: proportion of favouring environments for /u/ in 
### languages where fronting has occurred vs. languages where it hasn't

# languages where wholesale fronting has occurred
fronting.ls <- c("Ancient_Greek", "English", "Yiddish", "Swedish", "French", "Breton", "Albanian")
# languages where the IDS transcription of the fronted vowel is u (not y)
fronting.ls.u.trs <- c("Ancient_Greek", "English", "Yiddish")
# has wholesale fronting occurred in this language?
ids.props$fronting.l <- ids.props$language %in% fronting.ls
# the proportion of preceding favouring environments for fronted vowels (transcribed as u or y) and other vowels
ids.props$key.vowel.favouring.pre.count <- ifelse(ids.props$language %in% fronting.ls.u.trs | !ids.props$y, dat$favouring.pre.u.count, dat$favouring.pre.y.count)
ids.props$key.vowel.count <- ifelse(ids.props$language %in% fronting.ls.u.trs | !ids.props$y, ids.props$u.count, ids.props$y.count)

ids.wholesale <- select(ids.props, 
                        language,
                        fronting.l,
                        favouring.count=key.vowel.favouring.pre.count,
                        all.count=key.vowel.count)
write_csv(ids.wholesale, here("data", "final_data", "ids_wholesale.csv"))
