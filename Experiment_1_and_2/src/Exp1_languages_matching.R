library(data.table)
library(magrittr)
library(ompr)
library(ROI)
library(ROI.plugin.glpk)
library('geometry')
library(ompr.roi)

source("./Indefinites_functions.R")

####################################
# DATA
####################################
Folder = "../data/"
exp1languagesname = paste0(Folder, "languages_exp1.csv")
df = read.csv(exp1languagesname)
   
####################################
# SYNONYMY AND COVERAGE INDICES
####################################
# Retrieve synonymy and coverage indices
syncovindfile <- paste0(Folder, "syncovindfile_exp1.csv")

# If generate = FALSE, we are importing already created file with overlap and coverage indices
generate <- FALSE

if(generate) {
  syncovdf <-syncovindices(df)
  write.csv(syncovdf, syncovindfile, row.names=FALSE)
} 


syncovdf <- fread(syncovindfile)

####################################
# MATCHING
####################################
# Split data
df_natural = syncovdf[type == 'natural']
df_artificial = syncovdf[type == 'artificial']

# Goals
goal_mean_syn = mean(df_natural$syn_index)
goal_mean_cov = mean(df_natural$cov_index)

# Create OR model
n <- df_artificial[, .N] # one variable for each data point in artificial dataset
err <- 0.01
model <- MIPModel() %>% #mixed integer programming model
  add_variable(x[k], k = 1:n, type = "binary") %>% # decision variable (1 or 0 value)
  set_objective(sum_expr(x[k], k = 1:n), "max") %>% # maximize the number of datapoints in the artificial dataset 
  add_constraint(sum_expr(x[k]*df_artificial$syn_index[[k]], k = 1:n)<= (goal_mean_syn +err*goal_mean_syn)*sum_expr(x[k], k = 1:n))%>%
  add_constraint(sum_expr(x[k]*df_artificial$syn_index[[k]], k = 1:n)>= (goal_mean_syn -err*goal_mean_syn)*sum_expr(x[k], k = 1:n))%>%
  add_constraint(sum_expr(x[k]*df_artificial$cov_index[[k]], k = 1:n)<= (goal_mean_cov +err*goal_mean_cov)*sum_expr(x[k], k = 1:n))%>%
  add_constraint(sum_expr(x[k]*df_artificial$cov_index[[k]], k = 1:n)>= (goal_mean_cov -err*goal_mean_cov)*sum_expr(x[k], k = 1:n))%>%
  add_constraint(sum_expr(x[k], k = 1:n) >= 40) %>%
  solve_model(with_ROI(solver = "glpk", verbose = TRUE, tm_limit = 86400000))  
  #solve_model(with_ROI(solver = "alabama"))

model$status
model$solution

# Validate solution
solution_idx = get_solution(model, x[k]) %>% setDT() %>% .[value > 0, k]
df_sol = df_artificial[solution_idx]

sol_mean_syn = mean(df_sol$syn_index)
sol_mean_cov = mean(df_sol$cov_index)

# Print stats
goal_mean_syn
sol_mean_syn 

goal_mean_cov
sol_mean_cov

# Output file
matched = rbind(df_natural, df_sol)
matched_name = paste0(Folder, "Exp1_languages_matched001_timeout.csv")

write.csv(matched, matched_name, row.names=FALSE)

