###############################################################################
###                                                                         ###
###   Plotting number of species for which you can produce a useful         ###
###    occupancy model                                                      ###
###                                                                         ###
###############################################################################
library('reshape2')
library('ggplot2')
library('tidyr')
source('./Results/plotting_function.R')

###############################################################################
###                                                                         ###
###   ALL DATA                                                              ###
###    Upload your metadata.  For this you need a csv with the following    ###
###    headers:                                                             ###
###                                                                         ###
###   Species: the name of the species                                      ###
###   Taxa: the taxonomic group for the species                             ###
###   P90: the 90th percentile of visits per year                           ###
###         This can be calculated with:                                    ###
###         quantile(x=(list of numbers of records each year for species),  ###
###                  probs = .9)                                            ###
###   prop_abs: The proportion of data from the taxonomic group which does  ###
###             does not include the species of interest, e.g. if there     ###
###             are 20 records for the species and 1000 records for the     ###
###             taxonomic group, prop_abs = 980/1000 = .98                  ###
###                                                                         ###
###   NOTE: all sites with a record in the dataset must be revisited in at  ###
###     least one subsequent year, and a species within the same taxonomic  ###
###     group then recorded.  Every record which does not meet this         ###
###     requirement should be removed before calculating these metrics.     ###
###     For more on this, see 'Input Data Caveat' section of                ###
###     TSDA_Analysis.pdf                                                   ###
###                                                                         ###
###############################################################################
RM <- read.csv('Results/metrics/ALL_rawMetrics.csv') 
# Remove all last 10 yr data
RM$Taxa <- as.character(RM$Taxa)
if(length(RM$Taxa_Root)!=0){RM <- RM[RM$Taxa==as.character(RM$Taxa_Root),]}
RM <- calc_bad(RM)

# Plot the graphs
num_spec(RM) %>% stack_taxa()
num_spec(RM,prop=TRUE) %>% stack_taxa()
# Plot aspirational graphs
num_spec(RM,aspire=TRUE) %>% stack_taxa(prefix = 'Aspirational')
num_spec(RM,aspire=TRUE,prop=TRUE) %>%
  stack_taxa(prefix = 'Aspirational')

###############################################################################
###                                                                         ###
###   HABITAT DATA                                                          ###
###     This section calculates how many species can be modelled for        ###
###     habitats in the UK                                                  ###
###     If you want to run this for non-UK datasets, you need to sub-set    ###
###     your species data by habitat for your region of interest, and       ###
###     create a csv with the same headers as for the first secion, plus    ###
###     a new header: habitat                                               ###
###                                                                         ###
###############################################################################
RM_hab <-
  read.csv(file = file.path('Habitat/HabMetrics/ALL_habMetrics.csv'))
# Load list of possible habitats
hab_list <- read.csv('Habitat/Habitat_List.csv')
RM_hab <- calc_bad(RM_hab)
RM_hab$habitat <- as.character(RM_hab$habitat)

# Produce a table for all habitats in the UK
hab_df <- NULL
for(hab in hab_list$Habitat_code){
  hab_spec <- num_spec(df=RM_hab,hab=hab,melt=FALSE)
  hab_spec$numrec <- hab
  names(hab_spec)[2] <- 'habitat'
  hab_df <- rbind(hab_df,hab_spec)
}

# Plot up a sample graph for broad leaf woodland
num_spec(RM_hab,hab='BLW') %>% stack_taxa(prefix='Broad-leaf Woodland')
num_spec(RM_hab,hab='BLW',prop=TRUE) %>%
  stack_taxa(prefix='Broad-leaf Woodland')
# And for coastal
num_spec(RM_hab,hab='C') %>% stack_taxa(prefix='Coastal')
num_spec(RM_hab,hab='C',prop=TRUE) %>% stack_taxa(prefix='Coastal')

###############################################################################
###                                                                         ###
###   REGION DATA                                                           ###
###     This section calculates how many species can be modelled for        ###
###     NUTS regions in the UK                                              ###
###     If you want to run this for non-UK datasets, you need to sub-set    ###
###     your species data by region, and create a csv with the same headers ###
###     as for the first secion, plus a new header: region                  ###
###                                                                         ###
###############################################################################
RM_reg <-
  read.csv(file =  file.path('Region/RegMetrics/ALL_regMetrics.csv'))
region_lookup <- read.csv(file = file.path('Region/NUTS_lookup.csv'))
RM_reg <- merge(RM_reg,region_lookup)
RM_reg <- calc_bad(RM_reg)

# Produce a table for all habitats in the UK
reg_df <- NULL
for(reg in region_lookup$code){
  reg_spec <- num_spec(df=RM_reg,reg=reg,melt=FALSE)
  reg_spec$numrec <- reg
  names(reg_spec)[2] <- 'region'
  reg_df <- rbind(reg_df,reg_spec)
}

# Plot graphs (possible regions are in region_lookup)
# Plot up a sample graph for Scotland
num_spec(RM_reg,reg='Scotland') %>% stack_taxa(prefix='Scotland')
num_spec(RM_reg,reg='Scotland',prop=TRUE) %>% stack_taxa(prefix='Scotland')
# And for Wales
num_spec(RM_reg,reg='UKL') %>% stack_taxa(prefix='Wales')
num_spec(RM_reg,reg='UKL',prop=TRUE) %>% stack_taxa(prefix='Wales')
num_spec(RM_reg,reg='UKN',prop=TRUE) %>% stack_taxa(prefix='N. Ireland')
num_spec(RM_reg,reg='UKI',prop=TRUE,aspire=TRUE) %>% stack_taxa(prefix='London')
