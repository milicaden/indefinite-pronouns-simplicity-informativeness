
# Extract flavors from functions
extract_flavors = function(langdata){
  langdata$skflavor = ifelse(langdata$spec.know == 1,1,0)
  langdata$suflavor = ifelse(langdata$spec.unkn == 1,1,0)
  langdata$nsflavor = ifelse(langdata$irr.nonsp == 1,1,0)
  langdata$npiflavor = ifelse(((langdata$compar == 1 & (langdata$question == 1|langdata$indir.neg == 1|langdata$free.ch == 0))|(langdata$irr.nonsp == 0 & (langdata$question == 1|langdata$indir.neg == 1))|(langdata$dir.neg == 1 & langdata$neg.frag == 0 & langdata$irr.nonsp == 0)|(langdata$condit == 1 & langdata$irr.nonsp == 0 & langdata$free.ch == 0)),1,0)
  langdata$fciflavor = ifelse(langdata$free.ch == 1,1,0)
  langdata$nqflavor = ifelse(langdata$neg.frag == 1,1,0)
  return(langdata)
}

# Generate a number artificial languages selecting randomly items from a data frame

generate_languages = function(initial, number_lang, allitemsdf){
  ITEM = c()
  LANG = c()
  for(i in initial:number_lang) #generate # artificial languages
  {k<-sample(1:7,1) #for each language, determine how many items it will have (between 1 and 7)
  for(j in 1:k){
    currentitem <-paste0("fl", i,"-",j) #give a unique name to each item
    currentlang <-paste0("fl", i) #give a unique name to each language
    ITEM <- c(ITEM, currentitem)
    LANG <-c(LANG, currentlang)
  }
  }
  
  temporary = allitemsdf[FALSE,]
  
  for(i in 1:length(ITEM)){
    k<-sample(1:nrow(allitemsdf), 1)
    subdf <- allitemsdf[k,]
    temporary <- rbind(temporary, subdf)
  }
  fakelangdf <- data.frame(LANG, ITEM)
  fakelangdf <- cbind(fakelangdf, temporary)
  return(fakelangdf)
}

# Prep a data frame
prep = function(df){
  # Determine for each item which combination of flavors it can express
  df$meaning <- ifelse(df$skflavor == 1,"sk-","X-")
  df$meaning <- ifelse(df$suflavor == 1,paste0(df$meaning,"su-"),paste0(df$meaning,"X-"))
  df$meaning <- ifelse(df$nsflavor == 1,paste0(df$meaning,"ns-"),paste0(df$meaning,"X-"))
  df$meaning <- ifelse(df$npiflavor == 1,paste0(df$meaning,"npi-"),paste0(df$meaning,"X-"))
  df$meaning <- ifelse(df$fciflavor == 1,paste0(df$meaning,"fci-"),paste0(df$meaning,"X-"))
  df$meaning <- ifelse(df$nqflavor == 1,paste0(df$meaning,"nq"),paste0(df$meaning,"X"))
  df$meaning <- as.factor(df$meaning)

  # Determine for each item how many flavors it can express
  df$numoffl <- df$skflavor + df$suflavor + df$nsflavor + df$npiflavor + df$fciflavor + df$nqflavor

  # Determine for each language how many items can express each of the flavors
  for(flavor in c("sk", "su", "ns", "npi", "fci", "nq")){
    assign(paste0("numof", flavor), setNames(aggregate(eval(parse(text = paste0(flavor, "flavor")))~LANG, data = df, sum), c("LANG", paste0("numof", flavor))))
  }
  df<- Reduce(function(x, y) merge(x, y, by = c("LANG"), all=TRUE), list(df, numofsk, numofsu, numofns, numofnpi, numoffci, numofnq))
  return(df)
}

####################################
# COMMUNICATIVE COST AND COMPLEXITY OF A LANGUAGE: DEFINING FUNCTIONS
####################################

# Prior probability of a flavor based on Beekhuizen's corpus
priordf = read.csv("../data/Beekhuizen_priors.csv", header = TRUE)
probaf <- function(state) {
  return(priordf[1,state])
}

# Conditional probability of a flavor given an item: 1 over number of flavors the item can convey if an item can convey the flavor in question, 0 otherwise.
probafgi <- function(state, word, df) {
  temp <- subset(df, ITEM == word)
  return(temp[,paste0(state, "flavor")]/temp$numoffl)
}

# Conditional probability of an item given a flavor: 1 over number of items that can convey the flavor in question if an item can convey the flavor in question, 0 otherwise.
probaigf <- function(state, word, df){
  temp <- subset(df, ITEM == word)
  if (temp[,paste0("numof", state)] == 0)
  {return(0)} #this step to assign probability of each item given some flavor x to be 0 if no item can express that flavor in the language
  else
  {return(temp[,paste0(state, "flavor")]/temp[,paste0("numof", state)])}
}

# Communicative cost of a language based on utility of a language (asumming u(s,s')=1 if s=s', 0 otherwise).
utility <- function(language, df){
  temp <- subset(df, LANG == language)
  u <- 0
  for (i in temp$ITEM){
    for (k in c("sk", "su", "ns", "npi", "fci", "nq")){
      u <- u + (probaf(k)*probafgi(k,i,df)*probaigf(k,i,df))
    }
  }
  return(u)
}

cost <- function(language, df){
  1/utility(language, df)
}

# Complexity of a language in terms of the number of items
complexity <- function(language, df){
  temp <- subset(df, LANG == language)
  length(temp$ITEM)
}


featurecomplexity <- function(language, df){
  temp <- subset(df, LANG == language)
  u <- 0
  for (i in temp$ITEM){
    currentitemcomplexity <- subset(temp, ITEM == i)$`complexity`
    u <- u + currentitemcomplexity
  }
  return(u)
}

# Synonimy index
synindex = function(language, df){
  temp <- subset(df, LANG == language)
  syn_index = 0
  for(flavor in c("skflavor", "suflavor", "nsflavor", "npiflavor", "fciflavor", "nqflavor")){
    items = c() #collect all items in the language that can express a certain flavor
    for(item in temp$ITEM){
      if(subset(temp, ITEM == item)[[flavor]] == 1){
        items = c(items, item)
      }
    }
    if(length(items)>1){
      syn_index = syn_index + length(items) - 1
    }
  }
  return(syn_index)
}

# Coverage index
covindex = function(language, df){
  temp <- subset(df, LANG == language)
  cov_index = 0
  for(flavor in c("skflavor", "suflavor", "nsflavor", "npiflavor", "fciflavor", "nqflavor")){
    items = c() #collect all items in the language that can express a certain flavor
    for(item in temp$ITEM){
      if(subset(temp, ITEM == item)[[flavor]] == 1){
        items = c(items, item)
      }
    }
    if(length(items)>0){
      cov_index = cov_index + 1
    }
  }
  return(cov_index)
}

# Compute syn-indices and cov-indices of all languages from a data file
syncovindices = function(df){
  LANG <-unique(df$LANG)
  
  #synonimy index
  syn_index = c()
  for(i in LANG){
    k <- synindex(i, df)
    syn_index <-c(syn_index, k)
  }
  cov_index = c()
  for(i in LANG){
    k <- covindex(i, df)
    cov_index <-c(cov_index, k)
  }
  #make a df
  all <- data.frame(LANG, syn_index, cov_index)
  allfinal <- na.omit(all) 
  allfinal <-merge(allfinal, unique(df[,c("LANG", "type")]), by=c("LANG"))
  
  return(allfinal)
}

# Compute complexity, cost and syn-indices of all languages from a data file
compcostsyn = function(df){
  LANG <-unique(df$LANG)
  #complexity: features
  complexityoflanguages <- c()
  for(i in LANG){
    k <- featurecomplexity(i, df)
    complexityoflanguages <-c(complexityoflanguages, k)
  }
  #communicative cost
  costoflanguages <- c()
  for(i in LANG){
    k <- cost(i, df)
    costoflanguages <-c(costoflanguages, k)
  }
  
  #RSA communicative cost
  RSAcostoflanguages <- c()
  for(i in LANG){
    k <- RSAcost(i, df)
    RSAcostoflanguages <-c(RSAcostoflanguages, k)
  }
  #make a df
  all <- data.frame(LANG, complexityoflanguages, costoflanguages, RSAcostoflanguages)
  allfinal <- na.omit(all) 
  allfinal <-merge(allfinal, unique(df[,c("LANG", "type")]), by=c("LANG"))
  
  return(allfinal)
}


#Compute euc.dist between 2 points
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

# Compute the minimal euc. dist between a language and pareto frontier
min.euc.dist = function(df, pareto) {#df is a data frame with languages of interest; pareto is a df with estimated points of the pareto frontier
for(i in 1:nrow(df)){
  distances = c()
  for(j in 1:nrow(pareto)){
    eucdist = euc.dist(c(df$costoflanguages[i], df$complexityoflanguages[i]), c(pareto$costoflanguages[j], pareto$complexityoflanguages[j]))
    distances = c(distances, eucdist)
  }
  df$minimal[i] = min(distances)
  }
return(df)
}

#########################################
###RSA-style informativeness of languages
#########################################

# Literal listener (assuming they use uniform - i.e. uninformative - priors over flavors)
l0 <- function(state, word, df) {
  temp <- subset(df, ITEM == word)
  return(temp[,paste0(state, "flavor")]/temp$numoffl)
}

#Log-score (if L0(state|word) = 0, log score is -Inf, otherwise log of it)
log_score = function(state, word, df){
  k = l0(state, word, df)
  if(k==0){
    return(-Inf)
  }
  if(k>0){
    return(log(k))
  }
}

# Pragmatic speaker
s1 <- function(state, word, df){
  temp <- subset(df, ITEM == word)
  language = subset(df, LANG == temp$LANG)
  denominator = 0
  for(i in language$ITEM){
    k = exp(log_score(state, i, df))
    denominator = denominator + k}
  numerator = exp(log_score(state, word, df))
  if (denominator == 0)
  {return(0)} #this step to assign probability of each item given some flavor x to be 0 if no item can express that flavor in the language
  else
  {return(numerator/denominator)}
}

# Pragmatic listener
l1 = function(state, word, df){
  numerator = s1(state, word, df)*probaf(state)
  denominator = 0
  for (i in c("sk", "su", "ns", "npi", "fci", "nq")){
    k = s1(i, word, df)*probaf(i)
    denominator = denominator + k
  }
  if (denominator == 0) #denominator can be 0 only if a word has no meaning
  {return(0)}
  else
  {return(numerator/denominator)}
}

# Communicative cost of a language based on RSA utility of a language (asumming u(s,s')=1 if s=s', 0 otherwise).
RSAutility <- function(language, df){
  temp <- subset(df, LANG == language)
  u <- 0
  for (i in temp$ITEM){
    for (k in c("sk", "su", "ns", "npi", "fci", "nq")){
      u <- u + (probaf(k)*l1(k,i,df)*s1(k,i,df))
    }
  }
  return(u)
}

RSAcost <- function(language, df){
  1/RSAutility(language, df)
}


