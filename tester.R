library(here)
# if (getwd() != here("assessment")){
#   setwd("assessment")
#   
# }
library(jjmR)
# mod0.00 <- runit(geth("0.00"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod0.00 <- readJJM("h2_0.00", path = "config", input = "input")

# mod_prev <- readJJM(geth("1.00"), path = "config", input = "input")
# save(mod_prev, file="results/mod_prev_h1.Rdat")

load("results/mod_prev_h1.Rdat")

old_vs_new_mods <- combineModels(mod0.00,mod_prev)


selectivities <- get_selectivities(old_vs_new_mods)


plot_selectivities(selectivities)


plot(old_vs_new_mods,what="selectivity",fleet="fsh", alpha = 0.2, scale = 10,
     years = 2000:2020)


plot(mod0.00,what="selectivity",fleet="ind", alpha = 0.2, scale = 10,
     years = 2000:2020)

a = get_msy_mt(old_vs_new_mods)

kobe(mod0.00, engine = "ggplot")

kobe(old_vs_new_mods, engine = "lattice")