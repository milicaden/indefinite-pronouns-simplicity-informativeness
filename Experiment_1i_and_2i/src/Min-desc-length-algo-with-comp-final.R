#install.packages("rlist", repos = "http://cran.us.r-project.org")
library(rlist)
library(stringr)
library(dplyr)
library(readr)


##############################
# Define useful functions
##############################

# Combine each element of a set with each element of a different set via union and intersetion
combine = function(component1, component2){
  set <- list()
  for(a in 1:length(component1)){
    for (b in 1:length(component2)){
      if(identical(component1,component2) & a>=b){ #filtering one of the orders and unions/intersections/complements of a set with itself
        next
      }
      i = unlist(component1[a], use.names=FALSE)
      j = unlist(component2[b], use.names=FALSE)
      if(setequal(i, j)){#don't do combinations of things that are equal in terms of flavors
        next
      }
      oldnames <- names(set)
      flav.comb1 = union(i,j)
      flav.comb2 = intersect(i,j)
      flav.comb3 = setdiff(i,j)
      flav.comb4 = setdiff(j,i)
      set = list.append(set, flav.comb1, flav.comb2, flav.comb3, flav.comb4)
      names(set) <- c(oldnames, paste("union", names(component1[a]), names(component2[b])), paste("intersect", names(component1[a]), names(component2[b])), paste("setdiff", names(component1[a]), names(component2[b])), paste("setdiff", names(component2[b]), names(component1[a])))
    }
  }
  return(set)
}

# Filter function: to ease further computations, from each new build-block we discarded feature combinations that result in the same set of flavor as a previously constructed simpler feature combination
#we further remove (i) feature combinations which result in an empty set; (ii) from all possible combinations of two features which result in the same list of descriptions, we preserve only one
# We care not to remove combinations containing seplus.
filter = function(set){
  if(length(set) == 0){
    return(set)}
  else{
    #set = set[set != "character(0)"]
    unwanted = c()
    #make a list of all things from build_blocks from before
    set_name = deparse(substitute(set))
    name_number = parse_number(set_name)#get the number from the name
    less_complex = c()#make a list of elements from simpler build_blocks
    for(i in 1:(name_number-1)){
      if(name_number == 1){
        next
      }
      less_complex = c(less_complex, eval(parse(text = paste0("build_blocks", i))))
    }
    for(a in 1:length(set)){
      for(b in 1:length(less_complex)){
        # if((grepl("seplus", names(set[a]))|grepl("intersect", names(set[a])))&!grepl("seplus", names(less_complex[b]))&!grepl("intersect", names(less_complex[b]))){ #do not eliminate feature combinations that contain seplus
        #  next
        #}
        if(grepl("seplus", names(set[a]))){ #do not eliminate feature combinations that contain seplus
          next
        }
        if(setequal(unlist(set[a], use.names = FALSE), unlist(less_complex[b], use.names = FALSE)) &!(grepl("rplus", names(set[b]))& !grepl("seplus", names(set[b])) & !grepl("intersect", names(set[b])))&!(grepl("rminus", names(set[b]))& !grepl("seplus", names(set[b]))& !grepl("intersect", names(set[b])))){#eliminate more complex things unless less complex has rplus/rminus and not seplus&intersect
          unwanted = c(unwanted, a)
        }
      }
    }
    if(length(unwanted) >0){
      set = set[-unwanted]}
    newunwanted = c()
    #add here e.g. for(i) if in build_blocks 1,2,3 the same, unwanted = c(unwanted, j)
    for(i in 1:length(set)){
      for(j in 1:length(set)){
        # if(!setequal(unlist(set[i], use.names = FALSE), unlist(set[j], use.names = FALSE))){
        #  next
        # }
        if(setequal(unlist(set[i], use.names = FALSE), unlist(set[j], use.names = FALSE))){
          
          if((grepl("rplus", names(set[i]))|grepl("rminus", names(set[i])))&!grepl("rplus", names(set[j]))&!grepl("rminus", names(set[j]))){ #if 2 have equal flavors but one contains rplus/rminus while the other doesn't, eliminate the former
            newunwanted = c(newunwanted, i)
            next
          }
          if((grepl("rplus", names(set[j]))|grepl("rminus", names(set[j])))&!grepl("rplus", names(set[i]))&!grepl("rminus", names(set[i]))){ #if 2 have equal flavors but one contains rplus/rminus while the other doesn't, eliminate the former
            newunwanted = c(newunwanted, j)
            next
          }
          if((grepl("seplus", names(set[j])))&!grepl("seplus", names(set[i]))){ #do not eliminate feature combinations that contain seplus or intersection
            newunwanted = c(newunwanted, i)
            next
          }
          if((grepl("seplus", names(set[i])))&!grepl("seplus", names(set[j]))){ #do not eliminate feature combinations that contain seplus or intersection
            newunwanted = c(newunwanted, j)
            next
          }
          if((grepl("intersect", names(set[i])))&!grepl("intersect", names(set[j]))){ #do not eliminate feature combinations that contain seplus or intersection
            newunwanted = c(newunwanted, j)
            next
          }
          if((grepl("intersect", names(set[j])))&!grepl("intersect", names(set[i]))){ #do not eliminate feature combinations that contain seplus or intersection
            newunwanted = c(newunwanted, i)
            next
          }
          if(i>j){#of the two with the same flavor sets, keep the one with the last index
            newunwanted = c(newunwanted, j)
            next
          }
        }
        #if(grepl("seplus", names(set[j]))&!grepl("seplus", names(set[i]))){ #do not eliminate feature combinations that contain seplus
        #  next
        #}
      }
    }
    if(length(newunwanted) >0){
      set = set[-newunwanted]}
    set = set[set != "character(0)"]
    return(set)
  }
}

# Generate all descriptions up to a certain depth (depth = number of features in the descriptions) starting with build_blocks whose depth is 1
generate = function(depth, build_blocks){
  start = 2
  while(start < depth + 1){
    nam <- paste0("build_blocks", start)
    assign(nam, c(), envir = .GlobalEnv)
    for(x in 1:start){
      for(y in 1:start){
        if(x+y == start & x <= y){
          current = combine(eval(parse(text = paste0("build_blocks", x))), eval(parse(text = paste0("build_blocks", y))))
          assign(paste0("build_blocks", start), c(eval(parse(text = paste0("build_blocks", start))), current), envir = .GlobalEnv)#add current to what's already been created for that level of build_blocks
        }
      }
    }
    #assign(paste0("build_blocks", start), filter(eval(parse(text = paste0("build_blocks", start)))))#this won't work for filter because we are recovering the number from the name of the variable
    #if(exists("build_blocks1", envir = .GlobalEnv)){
    #    assign("build_blocks1", filter(build_blocks1), envir = .GlobalEnv)
    #}
    if(exists("build_blocks2", envir = .GlobalEnv)){
      assign("build_blocks2", filter(build_blocks2), envir = .GlobalEnv)
    }
    if(exists("build_blocks3", envir = .GlobalEnv)){
      assign("build_blocks3", filter(build_blocks3), envir = .GlobalEnv)
    }
    if(exists("build_blocks4", envir = .GlobalEnv) ){
      assign("build_blocks4", filter(build_blocks4), envir = .GlobalEnv)
    }
    if(exists("build_blocks5", envir = .GlobalEnv)){
      assign("build_blocks5", filter(build_blocks5), envir = .GlobalEnv)
    }
    if(exists("build_blocks6", envir = .GlobalEnv )){
      assign("build_blocks6", filter(build_blocks6), envir = .GlobalEnv)
    } 
    start = start+1
    
  }
  all = c()
  for(i in 1:depth){
    all = c(all, eval(parse(text = paste0("build_blocks", i))))
  }
  return(all)
}



##############################
# Features, ie. build_blocks1
##############################
kplus = c("sk")
kminus = c("su", "ns", "npi", "fci", "nq")
specplus = c("sk", "su")
specminus = c("ns", "npi", "fci", "nq")
seplus = c("npi", "fci", "nq")
seminus = c("sk", "su", "ns")
rplus = c("npi", "nq")
rminus = c("fci")
negplus = c("nq")
negminus = c("sk", "su", "ns", "npi", "fci")

build_blocks1 = list(kplus, kminus, specplus, specminus, seplus, seminus, rplus, rminus, negplus, negminus)
names(build_blocks1) <- c("kplus", "kminus", "specplus", "specminus", "seplus", "seminus", "rplus", "rminus", "negplus", "negminus")

##############################
# Generate all descriptions
##############################


descriptions = generate(5, build_blocks1)



#With up to 6 features, we capture all 63 logically possible flavor combinations. 

##############################
# Selecting the descriptions with minimun complexity
##############################
#Compute complexity
df <- data.frame(names(descriptions), I(descriptions))
df$complexity = str_count(df$names.descriptions., "kplus") + str_count(df$names.descriptions., "kminus") + str_count(df$names.descriptions., "specplus") + str_count(df$names.descriptions., "specminus") + str_count(df$names.descriptions., "seplus") + str_count(df$names.descriptions., "seminus") + str_count(df$names.descriptions., "rplus") + str_count(df$names.descriptions., "rminus") + str_count(df$names.descriptions., "negplus") + str_count(df$names.descriptions., "negminus") 

#write.csv2(df,'../data/depth-6.csv')
#According to Haspelmath, you can only have rplus or rminus feature in the intersection with se+ feature. So we filter those that don't satisfy this.
#Note that this command removes most but not all the cases which are not allowed, but we verify later that none of those is selected as the best description.

df <- subset(df, !((grepl("rplus",df$names.descriptions.)|grepl("rminus",df$names.descriptions.))&!(grepl("seplus",df$names.descriptions.)&grepl("intersect",df$names.descriptions.))))

#Arranging nicely the meaning column
df$meaning <- ifelse(grepl("sk", df$descriptions),"sk-","X-")
df$meaning <- ifelse(grepl("su", df$descriptions),paste0(df$meaning,"su-"),paste0(df$meaning,"X-"))
df$meaning <- ifelse(grepl("ns", df$descriptions),paste0(df$meaning,"ns-"),paste0(df$meaning,"X-"))
df$meaning <- ifelse(grepl("npi", df$descriptions),paste0(df$meaning,"npi-"),paste0(df$meaning,"X-"))
df$meaning <- ifelse(grepl("fci", df$descriptions),paste0(df$meaning,"fci-"),paste0(df$meaning,"X-"))
df$meaning <- ifelse(grepl("nq", df$descriptions),paste0(df$meaning,"nq"),paste0(df$meaning,"X"))

length(unique(df$meaning))
#Create the data frame that stores for each meaning combination its (minimum) complexity
minimum.df = df[FALSE,]
for(i in unique(df$meaning)){
  temp = subset(df, meaning == i)
  minimum = min(temp$complexity)
  temp.min = subset(temp, complexity == minimum)
  minimum.df = rbind(minimum.df, temp.min[1,])
}

write.csv2(minimum.df,'../data/minimum-desc-indef-with-compl.csv')

