library(dplyr)
library(here)

vs <- read.table(here("data", "raw_data", "beckervowelcorpus.txt"), fileEncoding="UTF-8", header=T, 
                 quote='"', sep="\t",stringsAsFactors=F)
# for now: manually removing erroneous Hungarian entry...
vs <- vs[!(vs$ISO=="hun" & vs$Method=="Wordlist"),]

# keeping only one dialect / language
# - if there is a "Standard"/"standard"/"stadard"/"Stadard" keep that - not implemented yet!
# - otherwise:
#   + if there are multiple recording styles, preferentially choose
#     o isolation > wordlist > carrier sentence > meaningful sentence > running speech

dialects <- unique(vs[,c("ISO","Dialect","Method")])
dialects$Method <- factor(dialects$Method, 
                          levels=c("Isolation","Wordlist","Carrier sentence",
                                   "Meaningful sentence","Running speech")
)
dialects <- dialects[order(dialects$ISO,dialects$Dialect,dialects$Method),]
dialects <- dialects[!duplicated(dialects$ISO),]
dialects.to.keep <- paste(dialects$ISO, dialects$Dialect, dialects$Method)

vs$combined.label <- paste(vs$ISO, vs$Dialect, vs$Method)
vs <- vs[vs$combined.label %in% dialects.to.keep,]
vs <- vs[!duplicated(paste(vs$Language,vs$Quantity)),]

# fixing subsystems info
unique(vs$Quantity)
vs$Quantity <- recode(vs$Quantity, 
                      `0`="Uniform",
                      Conbined="Combined",
                      long="Long")

# moving to long format
vs.long <- reshape(vs, varying=list(vowel=paste0("V",1:14,"OS"),
                                    ps=paste0("V",1:14,"PS"),
                                    f1=paste0("V",1:14,"F1"),
                                    f2=paste0("V",1:14,"F2"),
                                    f3=paste0("V",1:14,"F3"),
                                    f4=paste0("V",1:14,"F4")),
                   v.names=c("vowel","ps","f1","f2","f3","f4"),
                   timevar="vnum",direction="long")

# reorder according to language / dialect / subsystem (called quantity here)
vs.long <- vs.long[order(vs.long$ISO, vs.long$Dialect, vs.long$Quantity),]

# filtering out lines with no relevant info
vs.long <- vs.long[!is.na(vs.long$f1) & !vs.long$f1=="" & !vs.long$f2=="" & !vs.long$f1=="XXX",]

# converting to numeric cols

vs.long$f1 <- as.numeric(vs.long$f1)
vs.long$f2 <- as.numeric(vs.long$f2)

# keeping long / short / uniform subsystems

vs.long <- filter(vs.long, Quantity %in% c("Short", "Long", "Uniform"))

becker.vowels <- dplyr::select(vs.long, ISO, Language, Dialect, Genetics, Speakers, Method, Quantity, vowel, f1, f2, f3)

write_csv(becker.vowels, here("data", "final_data", "becker_vowels.csv"))
