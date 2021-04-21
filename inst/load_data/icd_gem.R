# icd::set_icd_data_dir('inst/icd/')
# icd::download_all_icd_data()

# Copy 2014 ICD9CM to data folder
icd9_2014 <- icd::get_icd9cm2014()
save(icd9_2014, file = "./data/icd9cm_2014.RData")

# Copy latest ICD10CM to data folder
icd10_2019 <- icd::get_icd10cm2019()
save(icd10_2019, file = "./data/icd10cm_2019.RData")

# General equivalence maps
gem_9_to_10 <- read.table('./inst/gem/2018_I9gem.txt')
colnames(gem_9_to_10) <- c("icd9", "icd10", "flag")
save(gem_9_to_10, file = "./data/gem_2018_9_to_10.RData")


gem_10_to_9 <- read.table('./inst/gem/2018_I10gem.txt')
colnames(gem_10_to_9) <- c("icd10", "icd9", "flag")
save(gem_10_to_9, file = "./data/gem_2018_10_to_9.RData")



