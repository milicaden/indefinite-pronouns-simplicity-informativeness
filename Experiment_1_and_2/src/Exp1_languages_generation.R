library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(minpack.lm)

source("./Indefinites_functions.R")
####################################
####################################
# DATA
####################################
#Natural language data
####################################
# Import Haspelmath's data file: 1 = yes, 6=no, 9=no info. 
# Columns DET and PERSON added by Milica based on the data from Haspelmath's appendix A with 1 meaning that the IP has respectively a DET and PERSON version, and 0 that it doesn't.
# Column neg.frag added by Milica based on data collected from literature or from competent speakers: 1 corresponds to the IP being used in constructions in which it is interpreted as a negated existential (such as negative fragment answers), 6 to it not being used. These data are made available in an online Appendix.
Folder = "../data/"
langdata = read.csv(paste0(Folder, "languages_real_40_updated.csv"), header = TRUE)
langdata[langdata == 6] <-0 

# Subset to indefinite pronouns for persons.
langdata <- subset(langdata, PERSON == 1)

# Extract flavors for different items from Haspelmath's data based on our working assumptions
langdata = extract_flavors(langdata)

# Subset the data to relevant columns
relevant <- c("LANG", "ITEM", "skflavor", "suflavor", "nsflavor", "npiflavor", "fciflavor", "nqflavor")
df <- langdata[relevant]
df$type <- "natural"

# If generate = FALSE, we are importing already generated artificial languages from a csv file, if generate = TRUE, we are generating them from scratch
generate <- FALSE



####################################
#Artificial language data
####################################

# Generate a data frame with all logically possible items (in terms of which flavors they can take).
allpossibilities <- expand.grid(rep(list(0:1), 6))
colnames(allpossibilities) <- c("skflavor", "suflavor", "nsflavor", "npiflavor", "fciflavor", "nqflavor")

# Remove the one item that cannot take any of the flavors
allpossibilities = subset(allpossibilities, !(skflavor== 0 & suflavor ==0 & nsflavor ==0 & npiflavor == 0 & fciflavor == 0 & nqflavor ==  0))

# Generate fake languages and their items
fakefilename = paste0(Folder, "artificial_languages_exp1.csv")

if(generate) {
  fakelangdf = generate_languages(1, 10000, allpossibilities)
  fakelangdf$type <- "artificial"
  write.csv(fakelangdf, fakefilename, row.names=FALSE)
} else {
  fakelangdf <- read.csv(fakefilename)
}

# Bind natural and artificial languages data
df <-rbind(df,fakelangdf)
exp1languagesname = paste0(Folder, "languages_exp1.csv")
write.csv(df, exp1languagesname, row.names=FALSE)





