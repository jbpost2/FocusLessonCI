
variable_info <- c(
  "HHL" = "Household language",
  "HHLANP" = "Detailed household language",
  "LANP" = "Language spoken at home",
  "LANX" = "Language other than English spoken at home",
  "FLANP" = "Language spoken at home allocation flag",
  "FLANXP" = "Language other than English allocation flag",
  "FFSP" = "Yearly food stamp/Supplemental Nutrition Assistance Program (SNAP) recipiency allocation flag",
  "WAOB" = "World area of birth",
  "FER" = "Gave birth to child within the past 12 months",
  "SCHL" = "Educational attainment",
  "SCH" = "School enrollment",
  "FSCHP" = "School enrollment allocation flag",
  "RAC1P" = "Recoded detailed race code"
)


#get big sample for the first part that is common to everyone
my_sample <- get_sample(var =FFSP,
                       ages = 15:100, state = "all", year = 2022)
#convert everything to factors...
my_sample2 <- my_sample %>%
  mutate(HHLfac = factor(as.character(HHL), labels = HHLvals, levels = names(HHLvals)),
         HHLANPfac = factor(as.character(HHLANP), labels = HHLANPvals, levels = names(HHLANPvals)),
         LANPfac = factor(as.character(LANP), labels = LANPvals, levels = names(LANPvals)),
         LANXfac = factor(as.character(LANX), labels = LANXvals, levels = names(LANXvals)),
         FLANPfac = factor(as.character(FLANP), labels = FLANPvals, levels = names(FLANPvals)),
         FLANXPfac = factor(as.character(FLANXP), labels = FLANXPvals, levels = names(FLANXPvals)),
         FFSPfac = factor(as.character(FFSP), labels = FFSPvals, levels = names(FFSPvals)),
         WAOBfac = factor(as.character(WAOB), labels = WAOBvals, levels = names(WAOBvals)),
         FERfac = factor(as.character(FER), labels = FERvals, levels = names(FERvals)),
         SCHLfac = factor(as.character(SCHL), labels = SCHLvals, levels = names(SCHLvals)),
         SCHfac = factor(as.character(SCH), labels = SCHvals, levels = names(SCHvals)),
         FSCHPfac = factor(as.character(FSCHP), labels = FSCHPvals, levels = names(FSCHPvals)),
         RAC1Pfac = factor(as.character(RAC1P), labels = RAC1Pvals, levels = names(RAC1Pvals)),
         STfac = factor(as.character(ST), labels = state_names, levels = names(state_names))
         )


#########################################
#functions for sampling data
get_sample <- function(var, ages, state, year){
  get_pums(
    variables = var, 
    variables_filter = list(AGEP = ages),
    state = state,
    recode = TRUE,
    year = year,
    survey = "acs1",
    key = my_key
  )
}



my_key <- "e267f117801b2ef741e54620602b0903c5f4d3c8"
library(tidyverse)
library(tidycensus)
travel_income <- get_pums(
  variables = c("JWMNP", "PINCP"), #travel and income, likely need to subset out 0 travel time people... dont' worry about too much of that now!
  state = "PA",
  recode = TRUE,
  year = 2022,
  survey = "acs1",
  key = my_key
)
#https://api.census.gov/data/2022/acs/acs1/pums/variables/FS.json
#1 implies SNAP benefits

name	"FFSP"
label	"Yearly food stamp/Supplemental Nutrition Assistance Program (SNAP) recipiency allocation flag"
predicateType	"int"
group	"N/A"
limit	0
suggested-weight	"WGTP"
values
item
0	"No"
1	"Yes"

name	"FS"
label	"Yearly food stamp/Supplemental Nutrition Assistance Program (SNAP) recipiency"
predicateType	"int"
group	"N/A"
limit	0
suggested-weight	"WGTP"
values
item
0	"N/A (vacant)"
1	"Yes"
2	"No"

#looks right!
sum(PA_snap$PWGTP*(PA_snap$FS == 1))



PA_snap <- get_pums(
  variables = c("FS", "HHL"), 
  state = "PA",
  recode = TRUE,
  year = 2022,
  survey = "acs1",
  key = my_key
)
#HHL = 1 is English
#2 is Spanish
sum(PA_snap$PWGTP*((PA_snap$FS == 1) & (PA_snap$HHL == 1)))
sum(PA_snap$PWGTP*(PA_snap$FS == 1))



PA_rent <- get_pums(
  variables = c("GRPIP", "FRNTP"), 
  state = "PA",
  recode = TRUE,
  year = 2022,
  survey = "acs1",
  key = my_key
)

GRPIP
FRNTP


PA_r <- PA_rent |>
  filter(FRNTP == 1) |>
  mutate(GRPIP = as.numeric(GRPIP)) |>
  filter(GRPIP < 100)
Rent <- c()
for (i in 1:nrow(PA_r)){
    Rent <- c(Rent, rep(PA_r$GRPIP[i], times = PA_r$PWGTP[i]))
}
hist(as.numeric(Rent))
