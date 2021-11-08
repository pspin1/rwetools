devtools::load_all()

sim_icd10_pat <- rwetools:::sim_codes()
save(sim_icd10_pat, file = "./data/sim_icd10_pat.RData")

