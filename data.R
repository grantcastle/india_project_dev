# This R script performs data wrangling that: combines information on 
# 1. central government funding allocations
# 2. control variables
# 3. election results
# 4. Hindu population to create a comprehensive dataset for analysis

# Loading Necessary Libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)
library(gdata)
library(rvest)
library(sf)
library(stringi)
library(ggplot2)

# Setting the working directory
setwd("/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/raw_data")

############ Data: Central government funding & control variables ###############

##### Preparing the India Data Including Central Funding and Controls #####

# Uploading datasets on central government funding allocations to Indian states
jjm_df <- read.xls('State wise Allocation Release Expenditure (3).xls')
sbmg_df <- read.csv('RS_Session_260_AU_1210_1.csv')
pm_kisan_df <- read.csv('RS_Session_258_AU_321_1.csv')
pm_jdy_df <-read.xls('Statewise Report 16_02_2024 00_15_20.xls')
mgnrega_df <- read.csv('mgnrega.csv')
pmayg_df <- read.csv('pmay---g (1).csv')
total_devolution_df <- read.xls('STATEMENTS1767A7D187DE374B978D236D809F6AA764.XLSX')

# Cleaning the JJM dataset to keep only the name of each state and the total central government
# funding allocation under JJM to that state for the financial year 2021-2022. 
jjm_clean <- jjm_df |> 
  select(c(X, X.2)) |>
  slice(-(1:6)) |>
  slice(-35) |>
  rename(
    state = X,
    jjm_funding = X.2)

# Cleaning the SBMG dataset to keep only the name of each state and the total central government
# funding allocation under SMBG to that state for the financial year 2021-2022. 
sbmg_clean <- sbmg_df |> 
  select(2, 6) |>
  slice(-35) |>
  rename(
    state = State.UT,
    sbmg_funding = 2)
  
# Cleaning the PM Kisan dataset to keep only the name of each state and the total number of 
# beneficiaries paid under the scheme in each state for the financial year 2021-2022. This dataset is
# split into three sections per financial year (4 months each), so we will also combine the data from
# these three sections to achieve a single financial year total. Data on funding was not available,
# for this scheme, so instead we use beneficiaries paid under this scheme by state as a proxy for funding
# under this scheme by state 
pm_kisan_clean <- pm_kisan_df %>% 
  select(2, 10, 11, 12) %>% 
  slice(-37) %>% 
  mutate(pm_kisan_beneficiaries = rowSums(select(., 2:4))) %>% 
  select(-c(2:4)) %>%
  rename(state = State)

# Cleaning the PM JDY dataset to keep only the name of each state and the total number of 
# beneficiaries under the scheme in each state. Data was not available on the funding per state
# or the beneficiaries per year. Therefore, we use the total number of beneficiaries  by state 
# since the program was initiated (2014) under Modi as a proxy for funding under this scheme by state.
pm_jdy_clean <- pm_jdy_df |> 
  select(c(State.Name, Total.Beneficiaries)) |>
  slice(-37) |>
  rename(
    state = State.Name,
    jdy_beneficiaries = Total.Beneficiaries)

# Cleaning the MGNREGA dataset to keep only the name of each state and the total central government
# funding allocation under JJM to that state for the financial year 2021-2022. 
mgnrega_clean <- mgnrega_df |> 
  filter(Financial.Year == '2021-22') |>
  select(c(State, Total.Funds.Available)) |>
  rename(
    state = State,
    mgnrega_funding = Total.Funds.Available)

# Cleaning the PMAY-G dataset to keep only the name of each state and the total central government
# funding allocation under JJM to that state for the financial year 2021-2022. 
pmayg_clean <- pmayg_df |> 
  filter(Fiscal.Year == '2021-22') |>
  select(c(State_UT, Allocation.of.Government.of.India.s.Share)) |>
  rename(
    state = State_UT,
    pmayg_funding = Allocation.of.Government.of.India.s.Share)

# Cleaning the Total Devolution dataset to keep only the name of each state and the total
# amount of funding the central government has devolved to each state. We use the 2021-2022
# budget estimate data, since other yearly data is missing. We also use the 'gross' rather than
# 'net' devolution since we are interested in the total amount the central government is allocating
# to each state for potentially political reasons, rather than the technical financial details including
# loans and repayments that comprise the net accounts. 
total_devolution_clean <- total_devolution_df |> 
  select(1, 6) |>
  slice(-(1:5)) |>
  slice(-(32:35)) |>
  rename(
    state = 1,
    total_funding = 2) |>
  mutate(state = gsub("^\\d+\\.\\s*", "", state))

# Uploading datasets on controls 
population_df <- read.xls('1T_151120231B719A3B3FB54571BC2734C0E46D35C0.XLSX')
rural_population_df <- read.xls('2T_1511202349241689F9D84E3498083E57333A1BE1 (1).XLSX')
gdp_df <- read.xls('27T_15112023E301A02422494F73BFAFD6CDD84EEEAE.XLSX', sheet = 'T_27(iv)')
power_df <- read.xls('137T_151120232F082F87CB0E4ACFA9D5A729A588CE64.XLSX', sheet = 'T_137(ii)')
tax_revenue_df <- read.xls('164T_1511202373889E6B391E4FEBA99E14283FAA4BD9 (1).XLSX', sheet = 'T_164(ii)')
rural_unemp_df <- read.xls('15T_151120231039403CA0F14AE2A7B1E5BE0013AEBB.XLSX', sheet = 'T_15 (Concld.)')
urban_unemp_df <- read.xls('16T_1511202316D3A8CA1A054072A0616576931B3CBE.XLSX', sheet = 'T_16 (Concld.) ')
literacy_df <- read.xls('6T_15112023078654FD8A3F4938AE67231CB8148F96.XLSX')
mpi_df <- read_excel('ind_hot (4).xlsx', sheet = 'Harmonised MPI Region')
mpi_extra_df <- read.xls('18T_15112023599424EFBB2644E29EC8ECF31DE3CFEB (2).XLSX')

# Cleaning the population dataframe to isolate population numbers for each state in 
# the most recent year the census was conducted, 2011. We also convert the figures into
# raw population counts, rather than population in thousands for easier interprtation.
population_clean <- population_df |> 
  select(1, 8) |>
  slice(-(38:45)) |>
  slice(-(1:2)) |>
  rename(
    state = 1,
    population = 2) |>
  mutate(population = population*1000)

# Cleaning the rural population dataframe to isolate rural population numbers for each state in 
# the most recent year the census was conducted, 2011. We also convert the figures into
# raw population counts, rather than population in thousands for easier interprtation.
rural_population_clean <- rural_population_df |> 
  select(1, 8) |>
  slice(-(38:46)) |>
  slice(-(1:2)) |>
  rename(
    state = 1,
    rural_population = 2) |>
  mutate(rural_population = rural_population*1000)

# Cleaning the GDP dataframe to isolate GDP numbers for each state in 2021-2022. This also
# removes special characters in the names of states.
gdp_clean <- gdp_df |> 
  select(1, 6) |>
  slice(-(39:40)) |>
  slice(-(1:4)) |>
  rename(
    state = 1,
    gdp = 2) |>
  mutate(state = gsub("[^[:alnum:] &]", "", state))

# Cleaning the tax_revenue dataframe to isolate each state's tax revenue collected in 2021-2022
tax_revenue_clean <- tax_revenue_df |> 
  select(1, 9) |>
  slice(-(34:40)) |>
  slice(-(1:2)) |>
  rename(
    state = 1,
    tax_revenue = 2)

# Cleaning the rural_unemployment dataframe to isolate each state's rural unemployment figures in 2021-2022. 
rural_unemp_clean <- rural_unemp_df |> 
  select(1, 11) |>
  slice(-(41:44)) |>
  slice(-(1:3)) |>
  rename(
    state = 1,
    rural_unemp = 2) |>
  mutate(rural_unemp = gsub("\\*", "", rural_unemp)) |>
  mutate(rural_unemp = as.numeric(rural_unemp))

# Cleaning the urban_unemployment dataframe to isolate each state's urbam unemployment figures in 2021-2022. 
urban_unemp_clean <- urban_unemp_df |> 
  select(1, 11) |>
  slice(-(41:44)) |>
  slice(-(1:3)) |>
  rename(
    state = 1,
    urban_unemp = 2) |>
  mutate(urban_unemp = gsub("\\*", "", urban_unemp)) |>
  mutate(urban_unemp = as.numeric(urban_unemp))

# Cleaning the literacy dataframe to isolate literacynumbers for each state in 
# the most recent year the census was conducted, 2011. 
literacy_clean <- literacy_df |> 
  select(1, 8) |>
  slice(-(38:42)) |>
  slice(-(1:2)) |>
  rename(
    state = 1,
    literacy_rate = 2)
  
# Cleaning the power availability dataframe to isolate power availability numbers for each state in 2021-2022.
power_clean <- power_df |> 
  select(1, 10) |>
  slice(-(38:47)) |>
  slice(-(1:2)) |>
  rename(
    state = 1,
    power_availability = 2) |>
  mutate(state = gsub("[^[:alnum:] &]", "", state))

# Cleaning the MPI dataframe to isolate the multidimensional poverty index scores for each
# Indian state during the period 2019-2021. We also round the MPI scores to the 10th decimal
# place for easier computation.
mpi_clean <- mpi_df |> 
  filter(mpi_df[[8]] %in% c('2019-2021')) |>
  select(9, 11) |>
  rename(
    state = 1,
    mpi_score = 2) |>
  mutate(mpi_score = as.numeric(mpi_score)) |>
  mutate(mpi_score = round(mpi_score, digits = 10))

# Cleaning the MPI extra dataframe to isolate the multidimensional poverty index scores for each
# Indian state during the period 2019-2021. 
mpi_extra_clean <- mpi_extra_df |> 
  select(1, 7) |>
  slice(-(39:41)) |>
  slice(-(1:2)) |>
  rename(
    state = 1,
    mpi_score = 2)

# Merging the Central Government funding datasets
# State names and states utilized differ slightly across each of the 7 datasets. To
# maintain simplicity and order, we will merge the datasets one at time, correcting
# and aligning state names as we proceed.

# Merging PM JDY and PM Kisan, and aligning state names in PM JDY to match those in PM JDY
pm_jdy_clean[32, 1] <- 'Dadra and Nagar Haveli and Daman and Diu'
pm_jdy_clean[13, 1] <- 'Jammu and Kashmir'
pm_jdy_clean[1, 1] <- 'Andaman and Nicobar Islands'

kisan_jdy <- merge(pm_kisan_clean, pm_jdy_clean, by = "state", all = TRUE)

# Merging the MGRNREGA dataframe into the existing merged dataframe and aligning state names 
# in MGNREGA to match those in merged dataframe.

mgnrega_clean[29, 1] <- 'Andaman and Nicobar Islands'
mgnrega_clean[32, 1] <- 'Dadra and Nagar Haveli and Daman and Diu'
mgnrega_clean <- mgnrega_clean |>
  slice(-(31))
mgnrega_clean[33, 1] <-'Jammu and Kashmir'

kisan_jdy_mgnrega <- merge(kisan_jdy, mgnrega_clean, by = "state", all = TRUE)

# Merging the PMAY-G dataframe into the existing merged dataframe and aligning state names 
# in PMAY-G to match those in merged dataframe.

pmayg_clean[29, 1] <- 'Andaman and Nicobar Islands'
pmayg_clean[34, 1] <-'Jammu and Kashmir'
new_pmayg_row <- c('Dadra and Nagar Haveli and Daman and Diu', pmayg_clean[31,2] + pmayg_clean[32,2])
pmayg_clean <- rbind(pmayg_clean, new_pmayg_row)
pmayg_clean <- pmayg_clean  |> slice(-(c(31, 32)))

kisan_jdy_mgnrega_pmayg <- merge(kisan_jdy_mgnrega, pmayg_clean, by = "state", all = TRUE)

# Merging the SBM-G dataframe into the existing merged dataframe. No alignment of state names 
# in SBM-G to match those in merged dataframe needed

kisan_jdy_mgnrega_pmayg_sbmg <- merge(kisan_jdy_mgnrega_pmayg, sbmg_clean, by = "state", all = TRUE)

# Merging the JJM dataframe into the existing merged dataframe and aligning state names 
# in JJM to match those in merged dataframe.
jjm_clean[7, 1] <- 'Dadra and Nagar Haveli and Daman and Diu'
jjm_clean[12, 1] <- 'Jammu and Kashmir'
jjm_clean[1, 1] <- 'Andaman and Nicobar Islands'

kisan_jdy_mgnrega_pmayg_sbmg_jjm <- merge(kisan_jdy_mgnrega_pmayg_sbmg, jjm_clean, by = "state", all = TRUE)

# Merging the total_devolution dataframe into the existing merged dataframe and aligning state names 
# in total_devolution to match those in merged dataframe.
total_devolution_clean[30, 1] <- 'Delhi'

central_gov_funding <- merge(kisan_jdy_mgnrega_pmayg_sbmg_jjm, total_devolution_clean, by = "state", all = TRUE)

# Ensuring that all values in the final central_gov_funding dataframe are numerical
central_gov_funding <- central_gov_funding |>
  mutate(jdy_beneficiaries = (gsub(",", "", jdy_beneficiaries))) |>
  mutate(total_funding = (gsub(",", "", total_funding))) |>
  mutate_at(vars(2:8), as.numeric)

# Merging the control datasets
# State names and states utilized differ slightly across each of the 9 datasets. To
# maintain simplicity and order, we will merge the datasets one at time, correcting
# and aligning state names as we proceed.

# Merging the population total and rural population datasets. These can be combined without
# any manipulation
pop_rural <- merge(population_clean, rural_population_clean, by = "state", all = TRUE)

# Adjusting the state names in the combined population dataset to match state names in the
# funding schemes dataset, including combining the data from two small territories (Dadra & Nagar Haveli and 	
# Daman & Diu which are treated as combined in central government funding data)

pop_rural[1, 1] <- 'Andaman and Nicobar Islands'
pop_rural[15, 1] <-'Jammu and Kashmir'
new_pop_rural_row <- c('Dadra and Nagar Haveli and Daman and Diu', pop_rural[8, 2] + pop_rural[9, 2], pop_rural[8, 3] + pop_rural[9, 3])
pop_rural <- rbind(pop_rural, new_pop_rural_row)
pop_rural <- pop_rural |> slice(-(c(8, 9)))

# Merging the rural and urban unemployment and datasets. These can be combined without
# any manipulation
unemp <- merge(rural_unemp_clean, urban_unemp_clean, by = "state", all = TRUE)

# Adjusting the state names in the combined unemployment dataset to match state names in the
# population dataset, including combining the data from two small territories (Dadra & Nagar Haveli and 	
# Daman & Diu which are treated as combined in central government funding data). We assume congruence between
# these two territories and use the available data for Dadra & Nagar Haveli as a proxy for the missing data
# in Daman & Diu when we treat them as a single region. 
unemp[15, 1] <-'Jammu and Kashmir'
unemp[8, 1] <- 'Dadra and Nagar Haveli and Daman and Diu'
unemp <- unemp |> slice(-9)

# Merging the unemployment and population dataframes
pop_unemp <- merge(pop_rural, unemp, by = "state", all = TRUE)

# Preparing to add the tax_revenue dataframe into the existing merged dataframe by 
# aligning state names within the tax_revenue dataframe. 
tax_revenue_clean[30, 1] <- 'Delhi'
tax_revenue_clean[10, 1] <-'Jammu and Kashmir'

# Merging the tax_revenue dataframe into the existing merged dataframe 
pop_unemp_tax <- merge(pop_unemp, tax_revenue_clean, by = "state", all = TRUE)

# Preparing to add the gdp dataframe into the existing merged dataframe by 
# aligning state names within the gdp dataframe. 
gdp_clean[1, 1] <- 'Andaman and Nicobar Islands'
gdp_clean[14, 1] <-'Jammu and Kashmir'
gdp_clean <- gdp_clean |> slice(-13)

# Merging the tax_revenue dataframe into the existing merged dataframe 
pop_unemp_tax_gdp <- merge(pop_unemp_tax, gdp_clean, by = "state", all = TRUE)

# Preparing to add the power dataframe into the existing merged dataframe by 
# aligning state names within the power dataframe. We also combine the Dadra & Nagar Haveli a
# and Daman & Diu states into a single state. 
power_clean <- power_clean |>
  mutate(power_availability = as.numeric(power_availability))
power_clean[1, 1] <- 'Andaman and Nicobar Islands'
new_power_row <- c('Dadra and Nagar Haveli and Daman and Diu', power_clean[8, 2] + power_clean[9, 2])
power_clean <- rbind(power_clean, new_power_row)
power_clean <- power_clean |> slice(-(c(8, 9)))

# Ladakh is a very sparsely populated region north of Jammu and Kashmir. In some datasets,
# its statistics appear combined with Jammu and Kashmir while in others, it is represented 
# separately. Since it is such a small region population-wise relative to Jammu and Kashmir,
# we will treat the data in the power dataframe as largely coming from Jammu and Kashmir and 
# rename the row as such.
power_clean[13, 1] <-'Jammu and Kashmir'

# Merging the power dataframe into the existing merged dataframe 
pop_unemp_tax_gdp_power <- merge(pop_unemp_tax_gdp, power_clean, by = "state", all = TRUE)

# Preparing to add the literacy rate dataframe into the existing merged dataframe by 
# aligning state names within the power dataframe. We also will combine the Dadra and 
# Nagar Haveli and Daman and Diu rows into a single row. However, since this is literacy rate
# rather than literacy counts, we take the average literacy rates of the two states and use it
# as a single literacy rate for the combined region
literacy_clean[1, 1] <- 'Andaman and Nicobar Islands'
literacy_clean[15, 1] <-'Jammu and Kashmir'
new_literacy_row <- c('Dadra and Nagar Haveli and Daman and Diu', (literacy_clean[8, 2] + literacy_clean[9, 2])/2)
literacy_clean <- rbind(literacy_clean, new_literacy_row)
literacy_clean <- literacy_clean |> slice(-(c(8, 9)))

# Merging the literacy dataframe into the existing merged dataframe 
pop_unemp_tax_gdp_power_lit <- merge(pop_unemp_tax_gdp_power, literacy_clean, by = "state", all = TRUE)

# Preparing to add the mpi dataframe into the existing merged dataframe by 
mpi_clean[11, 1] <- 'Jammu and Kashmir'

# Merging the mpi dataframe into the existing merged dataframe 
controls <- merge(pop_unemp_tax_gdp_power_lit, mpi_clean, by = "state", all = TRUE)

# The outside MPI dataset is missing MPI scores for several smaller regions. However,
# another dataset contains missing MPI scores for these states. We therefore will merge the
# additional MPI scores into the existing MPI scores to complete these scores. The mpi scores
# from the first dataset are more detailed and reliable (conducted by and outside expert actor),
# so we wish to keep those mpi scores, but replace the missing values with the scores from the
# potentially less reliable/accurate 'extra' database. 

# Preparing to add the mpi_extra dataframe into the existing control dataframe from aligning 
# state names in the mpi_extra dataframe 
mpi_extra_clean[29, 1] <- 'Andaman and Nicobar Islands'
mpi_extra_clean[33, 1] <-'Jammu and Kashmir'
mpi_extra_clean[31, 1] <- 'Dadra and Nagar Haveli and Daman and Diu'

controls <- merge(controls, mpi_extra_clean, by = "state", all.x = TRUE)
controls$mpi_score.x[is.na(controls$mpi_score.x)] <- controls$mpi_score.y[is.na(controls$mpi_score.x)]
controls <- controls |>
  select(-11) |>
  rename(mpi_score = mpi_score.x)
  
# Ensuring that all values in the final controls dataframe are numerical
controls <- controls |>
  mutate_at(vars(2:10), as.numeric)

# Aligning measurements in both final dataframes. Some numbers are reported in Lakhs and
# others are reported in crores. 1 Lakh = 100,000. 1 crore = 10,000,000. We convert all
# numbers to raw counts in rupees to allow for easier manipulation and interpretation. The 
# unemployment figures are also reported per 1000. We therefore convert those into unemployment 
# percents (divide by 10) for consistent interpretation. 
controls <- controls |>
  mutate(
    tax_revenue = tax_revenue*10000000,
    gdp = gdp*100000,
    power_availability = power_availability*1000000,
    rural_unemp = rural_unemp/10,
    urban_unemp = urban_unemp/10,
  )

central_gov_funding <- central_gov_funding |>
  mutate(
    jjm_funding = jjm_funding*100000,
    sbmg_funding = sbmg_funding*10000000,
    mgnrega_funding = mgnrega_funding*10000000,
    pmayg_funding = pmayg_funding*10000000,
    total_funding = total_funding*10000000
  )
    
# Exporting the final two dataframes as CSV files 

write.csv(central_gov_funding, 
          file = "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets/central_gov_funding.csv", 
          row.names = FALSE)
write.csv(controls, 
          file = "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets/controls.csv", 
          row.names = FALSE)

# Merging the controls and central government funding dataframes 
india_data_merged <- merge(central_gov_funding, controls, by = "state")

# Standardizing the central government funding variables by dividing by state population
# to create a figure per capita. The goal is to assess whether some states are receiving 
# more central government funding than they theoretically should be getting based on 
# their socioeconomic needs alone. Yet, there is considerable disparity in population
# among Indian states. Therefore, we assess them on an equal playing field of per capita 
# rather than just raw funding numbers. We name each new standardized variable as x_pgpc 
# to denote variables that are standardized by points generalized per capita (pgpc).

india_data_merged <- india_data_merged |>
  mutate(
    pm_kisan_beneficiaries_pgpc = (pm_kisan_beneficiaries / population),
    jdy_beneficiaries_pgpc = (jdy_beneficiaries / population),
    mgnrega_funding_pgpc = (mgnrega_funding / population),
    pmayg_funding_pgpc = (pmayg_funding / population),
    sbmg_funding_pgpc = (sbmg_funding/ population),
    jjm_funding_pgpc = (jjm_funding/ population),
    sbmg_funding_pgpc = (sbmg_funding / population),
    jjm_funding_pgpc = (jjm_funding / population),
    total_funding_pgpc = (total_funding / population)
  )

# Some of the sub-national political districts represented here are states, while others
# are Union Territories. Union Territories do not have their own 'state' government and
# are instead administered directly by the central government, and therefore receive
# most, if not all, of their funding from the central government. Thus, we create an indicator
# variable for union territories since the central government funding for union territories will inherently
# be much greater than for states. 
india_data_merged <- india_data_merged |>
  mutate(UT = ifelse(state %in% c("Dadra and Nagar Haveli and Daman and Diu", "Jammu and Kashmir", "Ladakh",
                            "Chandigarh", "Delhi", "Puducherry", "Lakshadweep", "Andaman and Nicobar Islands"), 1,0))

####################### Data: Hindu population by state ###########################

# Extracting the table of Hindu population by state/UT from Wikipedia

# Setting the URL to the correct wikipedia page
url <- "https://en.wikipedia.org/wiki/Hinduism_in_India"

# Setting the url as an html vector
response <- read_html(url)

# Scraping the tables from the web page
hindu_table <- html_table(response, fill = TRUE)

# Identifying the specific table from the webpage needed
hindu_population_table <- hindu_table[[3]]

# Cleaning the table to remove commas, ensure numbers are numeric, and altering column names
hindu_population <- hindu_population_table |> 
  rename(pct = `% Hindus`) |> 
  mutate(
    state = Region,
    hindu_pop = gsub(",", "", Hindus),
    total_pop = gsub(",", "", Total),
    pct_hindus = gsub("%", "", pct)
  ) |>
  select(5:8) |>
  mutate(across(c(hindu_pop, total_pop, pct_hindus), as.numeric))

# Cleaning the table to align state names with state names used throughout other dataframes, including
# merging the Dadra and Nagar Haveli and Daman and Diu states into a single state row.
new_hindu_row <- data.frame(
  state = 'Dadra and Nagar Haveli and Daman and Diu', 
  hindu_pop = hindu_population[7, 2] + hindu_population[3, 2],
  total_pop = hindu_population[7, 3] + hindu_population[3, 3],
  pct_hindus = ((hindu_population[7, 2] + hindu_population[3, 2]) / (hindu_population[7, 3] + hindu_population[3, 3])))

new_hindu_row <- new_hindu_row |>
  rename(pct_hindus = hindu_pop.1) |>
  mutate(pct_hindus = pct_hindus * 100)

hindu_population <- rbind(hindu_population, new_hindu_row)

hindu_population <- hindu_population |> 
  slice(-c(1, 3, 7))

#################### Data: 2019 elections results by state ###########################

# Loading the election results database
elections_india <- read.csv("TCPD_GE_All_States_2024-2-18.csv")

# Removing unnecessary columns
elections_india_clean <- elections_india |>
  select(c(1, 3, 4, 11, 12, 15, 16))

# Isolating votes only for the BJP Party
elections_india_clean <- elections_india_clean |>
  filter(Party == "BJP")

# Isolating votes only from the 2019 elections
elections_2019_clean <- elections_india_clean |>
  filter(Year == "2019")

# Consolidating the number of votes for BJP candidates by state
elections_2019_clean_state <- elections_2019_clean |>
  group_by(State_Name) |>
  summarize(
    votes = sum(Votes),
    eligible_voters = sum(Electors)
  )

# Adjusting the state names to align with state name formats used in other data. This includes
# combining the Dadra and Nagar Haveli and Daman and Diu into a single 'state.' 
new_elections_2019_clean_state_row <- c('Dadra and Nagar Haveli and Daman and Diu', 
                                        elections_2019_clean_state[8, 2] + elections_2019_clean_state[9, 2], 
                                        elections_2019_clean_state[8, 3] + elections_2019_clean_state[9, 3])
names(new_elections_2019_clean_state_row)[1] <- "State_Name"
elections_2019_clean_state <- rbind(elections_2019_clean_state, new_elections_2019_clean_state_row)
elections_2019_clean_state <- elections_2019_clean_state |> slice(-(c(8, 9)))

elections_2019_clean_state$State_Name <- gsub("_", " ", elections_2019_clean_state$State_Name)
elections_2019_clean_state[1,1] <- "Andaman and Nicobar Islands"
elections_2019_clean_state[13,1] <- "Jammu and Kashmir"

# Creating new column to display the share (as a percentage) of votes in each state
# won by BJP candidates
elections_2019_clean_state <- elections_2019_clean_state |>
  mutate(BJP_vote_share = (votes/eligible_voters) * 100)

# Exporting the final dataframe as a CSV files
write.csv(elections_2019_clean_state, 
          file = "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets/elections_2019_state.csv", 
          row.names = FALSE)

# Merging the 2019 election data with the existing fill India data dataframe 
india_data_merged <- left_join(india_data_merged, elections_2019_clean_state, by = c("state" = "State_Name"))

####################### Data: final clean dataframe ############################

# Merging the cleaned hindu_population dataframe into the full dataframe with 
# all other variables
india_data_merged <- left_join(india_data_merged, hindu_population, by = "state")

# Final cleaning of the final dataframe: adjusting column names, adding new column,
# and removing superfluous column
india_data <- india_data_merged |>
  mutate(pct_rural_pop = (rural_population / population)*100) |>
  relocate(pct_rural_pop, .before = "rural_unemp") |>
  select(-total_pop) |>
  rename(BJP_votes = votes) 

# Writing the finalized dataframe as a csv to save for future use
write.csv(india_data, 
          file = "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets/india_data.csv", 
          row.names = FALSE)


################# Data: election results by constituency #########################

# Loading the full elections dataset from the working directory 
elections <- read.csv("TCPD_GE_All_States_2024-2-18.csv")

# Removing unnecessary columns
elections_clean <- elections |>
  select(c(1, 3, 4, 11, 12, 15, 16))

# Isolating votes only for the BJP Party
elections_clean <- elections_clean |>
  filter(Party == "BJP")

# Isolating votes only from the parliamentary elections which the BJP contested 
elections_clean <- elections_clean |>
  filter(Year %in% c("1984", "1989", "1991", "1996", "1998", "1999", "2004", "2009", "2014", "2019"))

# Adding a new column to the elections datframe to display the share of eligible voters who voted
# for the BJP
elections_clean <- elections_clean |>
  mutate(BJP_vote_share = (Votes/Electors) * 100)

# Changing the state names within the elections dataset to lowercase and removing any symbols/spaces
# to enable easier merging with the shapefile
elections_clean$State_Name <- tolower(elections_clean$State_Name)
elections_clean$State_Name <- gsub("[[:space:][:punct:]]", "", elections_clean$State_Name)

# Outputting the cleaned elections dataframe as a CSV file for use in other formats
write.csv(elections_clean, 
          file = "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets/elections_clean.csv",
          row.names = FALSE)

