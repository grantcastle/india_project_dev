# This R script includes code that produces 
# 1. 8 static plots that provide insight into India’s central government funding distribution
# 2. 4 static plots that prepare for the Shiny app

##################### FUNDING & VOTE SHARE DISTRIBUTION ########################

# Loading necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(stringi)

# Setting the working directory
path <- "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data"
setwd(path)

# Setting the directory to save images to
image_directory <- "/Users/grantcastle/Documents/final-project-castle-huang-thampi/images/"

# Loading the cleaned dataframe of Indian funding data and controls
india_data <- read.csv(file.path(path,'cleaned_datasets/india_data.csv'))

###################### Plot 1: Raw Funding Per State #############################

# Creating barplots which show the central government funding levels / number of beneficiaries
# for each state. We utilize a for loop in order to quickly display similar barplots for 
# each of the 6 specific central programs, as well as the overall total funding, 
# and save them all to the working directory.
india_data_funding <- india_data |>
  select(1:8) |>
  rename(
    `JDY Beneficiaries` = jdy_beneficiaries,
    `JJM Funding` = jjm_funding,
    `MGNREGA Funding` = mgnrega_funding,
    `PM Kisan Beneficiaries` = pm_kisan_beneficiaries,
    `PMAY-G Funding` = pmayg_funding,
    `SBM-G Funding` = sbmg_funding,
    `Total Funding` = total_funding
    )

for (i in names(india_data_funding)[2:8]) {
  funding_plot <- ggplot(data = india_data_funding) +
                  geom_bar(aes(x = state, y = !!sym(i)), stat = "identity", fill = "royalblue") +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                  labs(
                    title = paste0(i, ' by State in FY 2021-2022'),
                    subtitle = "Funding levels in rupees",
                    caption = 'Data from Indian Ministry of Finance',
                    x = 'State or Union Territory',
                    y = i
                    )

  print(funding_plot)
  ggsave(paste0(image_directory, "static_plot1_raw_", gsub(" ", "_", i),".png"), plot = funding_plot, width = 6, height = 4, dpi = 300)
}

################### Plot 2: Standardized Funding Per State #########################

# Creating barplots which show the central government funding levels / number of beneficiaries
# for each state, standardized per capita. States with larger populations are likely to receive 
# more funding, thus this standardization aims to display the funding levels for 
# each state on a level playing field. We utilize a for loop in order to quickly 
# display similar barplots for  each of the 6 specific central programs, as well 
# as the overall total funding, and save them all to the working directory.
india_data_funding_pgpc <- india_data |>
  select(c(1, 19:25)) |>
  rename(
    `JDY Beneficiaries PC` = jdy_beneficiaries_pgpc,
    `JJM Funding PC` = jjm_funding_pgpc,
    `MGNREGA Funding PC` = mgnrega_funding_pgpc,
    `PM Kisan Beneficiaries PC` = pm_kisan_beneficiaries_pgpc,
    `PMAY-G Funding PC` = pmayg_funding_pgpc,
    `SBM-G Funding PC` = sbmg_funding_pgpc,
    `Total Funding PC` = total_funding_pgpc
  )

for (i in names(india_data_funding_pgpc)[2:8]) {
  funding_plot_pgpc <- ggplot(data = india_data_funding_pgpc) +
    geom_bar(aes(x = state, y = !!sym(i)), stat = "identity", fill = "royalblue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = paste0(i, ' by State in 2021-2022'),
      subtitle = 'Funding in rupees / beneficiaries per capita',
      caption = 'Data from Indian Ministry of Finance',
      x = 'State or Union Territory',
      y = i
    )
  
  print(funding_plot_pgpc)
  ggsave(paste0(image_directory, "static_plot2_std_", gsub(" ", "_", i),".png"), plot = funding_plot_pgpc, width = 6, height = 4, dpi = 300)
}

############# Plot 3: BJP's vote share in national elections over time #########

# Loading the cleaned total elections dataframe 
elections_clean <- read_csv("cleaned_datasets/elections_clean.csv")

# Modifying the total elections dataframe to aggregate votes received by BJP candidates across
# all constituencies each year as well as the total eligible voters across all constituencies
# each year, then calculating the vote share won by BJP cnadidates (as a percent) nationally in
# each year.
elections_clean_aggregated <- elections_clean |>
  group_by(Year) |>
  summarize(
    votes = sum(Votes),
    eligible_voters = sum(Electors)
  ) |>
  mutate(BJP_vote_share = (votes/eligible_voters) * 100) |>
  mutate(year = as.character(Year)) 

# Creating a barplot to display the vote share won by the BJP party nationally in 
# each year India has held national elections since 1984.
BJP_year_plot <- ggplot(elections_clean_aggregated) +
                    geom_bar(aes(x = `year`, y = BJP_vote_share), stat = "identity", fill = "red4") +
                    labs(
                      title = 'Vote Share Won by BJP in Indian National Elections: 1984-2019',
                      subtitle = 'BJP vote share is votes received by all BJP candidates divided by eligible voters',
                      caption = 'Data from Ahsoka University',
                      x = 'Election Year',
                      y = 'BJP Vote Share (%)')

print(BJP_year_plot)

# Saving plot to the working directory
ggsave(paste0(image_directory, "static_plot3_BJP_vote_share_years.png"), plot = BJP_year_plot, width = 6, height = 4, dpi = 300)

################### Plot 4: BJP Vote Share by State (2019) ############################

# Creating a barplot to display the vote share won by the BJP party in each state/UT
# during the 2019 parliamentary elections (India's most recent national elections).
BJP_2019_plot <- ggplot(data = india_data) +
                  geom_bar(aes(x = state, y = BJP_vote_share), stat = "identity", fill = "dark red") +
                  scale_y_continuous(limits = c(0, 100)) + 
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                  labs(
                    title = 'Vote Share Received by BJP in 2019 Indian Elections by State',
                    subtitle = 'BJP vote share: votes received by BJP candidates divided by eligible voters',
                    caption = 'Data from Ashoka University',
                    y = 'BJP Vote Share (%)',
                    x = 'State or Union Territory')

print(BJP_2019_plot)

# Saving plot to the working directory
ggsave(paste0(image_directory, "static_plot4_BJP_vote_share_state.png"), plot = BJP_2019_plot, width = 6, height = 4, dpi = 300)

################### Plot 5: BJP Vote Share and Hindu Population Share ##################

# Creating a plot to show, side-by-side, the vote share received by the BJP as well
# as the proportion of the population that is Hindu for each state/union territory.

# Filtering the full dataset to focus on specific funding columns of interest
india_data_votes_hindu <- india_data |> 
  select(c(1, 29, 31))

# Pivoting the dataframe to enable bars to be displayed side-by-side
india_data_votes_hindu <- pivot_longer(india_data_votes_hindu, cols = c(2, 3), names_to = "Variable", values_to = "Percentage")

# Renaming variables for improved aesthetics when displayed on plots
india_data_votes_hindu <- india_data_votes_hindu |>
  mutate(Variable = 
           case_when(
             Variable == "pct_hindus" ~ "Prop. Hindu Population",
             Variable == "BJP_vote_share" ~ "BJP Vote Share"
           )
  )

# Modifying the dataframe to arrange states into order of BJP vote share, then converting
# state variable to factor to maintain the order when placed in a plot.
india_data_votes_hindu <- india_data_votes_hindu |>
  arrange(desc(case_when(Variable == "BJP Vote Share" ~ Percentage, TRUE ~ NA_real_))) |>
  mutate(state = factor(state, levels = unique(state)))

# Creating the plot which will flip the bars to be displayed vertically and back-to-back
BJP_hindu_bar <- ggplot(india_data_votes_hindu, aes(x = state, y = ifelse(Variable == "BJP Vote Share", -Percentage, Percentage), fill = Variable)) +
                  geom_bar(stat = "identity", position = "identity", color = "black") +
                  scale_fill_manual(values = c("red4", "darkgreen")) +
                  scale_y_continuous(limits = c(-50, 100)) +
                  coord_flip() + 
                  labs(
                    title = "State-Wise BJP Vote Share (2019) and Hindu Population Prop.",
                    caption = 'Data from Ashoka University',
                    x = "State", 
                    y = "Percentage") +
                  theme_minimal()

print(BJP_hindu_bar)

# Saving plot to the working directory
ggsave(paste0(image_directory, "static_plot5_BJP_vote_hindu_barplot.png"), plot = BJP_hindu_bar, width = 6, height = 4, dpi = 300)


#### Plot 6: Scatterplot of BJP Vote Share by State and Hindu Population Makeup by State ##############
# Creating a scatterplot with a line of best fit to display the correlation between the 
# vote share received by the BJP and the Hindu population makeup for each state/UT.
BJP_hindu_scatter <- ggplot(data = india_data, aes(x = BJP_vote_share, y = pct_hindus)) +
                      geom_point(color = "darkred") +
                      geom_smooth(method = "lm", se = FALSE, color = "black") +
                      scale_y_continuous(limits = c(0, 100)) +
                      labs(
                        title = "State-Wise Vote BJP Share in 2019 Elections and Hindu Proportion of Population",
                        x = "Vote Share for BJP Party in 2019 Elections (%)",
                        y = "Proportion of Population that is Hindu (%)",
                        caption = "Data from Ahsoka University"
                      )

print(BJP_hindu_scatter)

# Saving plot to the working directory
ggsave(paste0(image_directory, "static_plot6_BJP_vote_hindu_scatter.png"), plot = BJP_hindu_scatter, width = 6, height = 4, dpi = 300)

########## Plot 7: Scatterplots of Standardized Funding by State and BJP Vote Share ##################

# Creating scatterplots to display the correlation between vote share received 
# by the BJP and the standardized funding for different funding programs
# received by each state. We use facet wrapping to display multiple plots side-by-side

# Filtering the full dataset to focus on specific funding columns of interest
india_data_funding_pgpc_plots <- india_data |> 
  select(c(1, 19:25, 29))

# Pivoting the dataframe to enable facet wrapping
india_data_funding_pgpc_plots <- india_data_funding_pgpc_plots |>
  pivot_longer(cols = 2:8, names_to = "Variable", values_to = "Value")

# Renaming the funding variable types for improved aesthetics when displayed on plots
india_data_funding_pgpc_plots <- india_data_funding_pgpc_plots |>
  mutate(Variable = case_when(
    Variable == "jdy_beneficiaries_pgpc" ~ "JDY Beneficiaries Std.",
    Variable == "jjm_funding_pgpc" ~ "JJM Funding Std.",
    Variable == "mgnrega_funding_pgpc" ~ "MGNREGA Funding Std.",
    Variable == "pm_kisan_beneficiaries_pgpc" ~ "PM Kisan Beneficiaries Std.",
    Variable == "pmayg_funding_pgpc" ~ "PMAY-G Funding Std.",
    Variable == "sbmg_funding_pgpc" ~ "SBM-G Funding Std.",
    Variable == "total_funding_pgpc" ~ "Total Funding Std."))

# Creating the scatterplots and using facet_wrap to display multiple plots
# on each display. 
BJP_facet_scatter <- ggplot(data = india_data_funding_pgpc_plots, aes(x = BJP_vote_share, y = Value)) +
                      geom_point(color = "darkred") +
                      geom_smooth(method = "lm", se = FALSE, color = "black") +
                      facet_wrap(~ Variable, scales = "free_y") +
                      labs(
                        title = "Central Gov. Transfers States vs. BJP Vote Shares",
                        subtitle = "Transfers standardized per capita",
                        x = "BJP Vote Share in 2019 Elections",
                        y = "Funding in Rupees / Number of Beneficiaries",
                        caption = "Data from Ashoka University and Indian Ministry of Finance"
                      )

print(BJP_facet_scatter)

# Saving plot to the working directory
ggsave(paste0(image_directory, "static_plot7_std_funding_BJP_vote_scatter.png"), plot = BJP_facet_scatter, width = 6, height = 4, dpi = 300)

######### Plot 8: Scatterplots of Standardized Total Funding by State and various development indicators ##################

# Creating scatterplots to display the correlation between various development indicators and
# the total transfers from the central government received by each state. If BJP vote share
# does not explain differential funding levels, what does? We use facet wrapping to 
# display multiple plots side-by-side

# Filtering the full dataset to focus on specific funding columns of interest
india_data_predictors_plots <- india_data |>
  select(c(1, 11:14, 16:18, 25, 31))

# Pivoting the dataframe to enable facet wrapping
india_data_predictors_plots <- india_data_predictors_plots %>% 
  pivot_longer(cols = c(2:8, 10), names_to = "Variable", values_to = "Value")

# Renaming the predictor variable types for improved aesthetics when displayed on plots
india_data_predictors_plots <- india_data_predictors_plots  %>%
  mutate(Variable = case_when(
    Variable == "pct_rural_pop" ~ "Rural Population (%)",
    Variable == "rural_unemp" ~ "Rural Unemployment (%)",
    Variable == "urban_unemp" ~ "Urban Unemployment (%)",
    Variable == "tax_revenue" ~ "Central Tax Revenue Collected",
    Variable == "power_availability" ~ "Availability of Power",
    Variable == "literacy_rate" ~ "Literacy Rate",
    Variable == "mpi_score" ~ "Multidimensional Poverty Index Score",
    Variable == "pct_hindus" ~ "Hindu Population (%)"
    ))

# Creating the scatterplots and using facet_wrap to display multiple plots
# on each display. 
econ_facet_scatter <- ggplot(data = india_data_predictors_plots, aes(x = Value, y = total_funding_pgpc)) +
                        geom_point(color = "darkred") +
                        geom_smooth(method = "lm", se = FALSE, color = "black") +
                        facet_wrap(~ Variable, scales = "free_x") +
                        labs(
                          title = "Development Indictors vs. Central Gov.Transfers to States (2021-2022)",
                          subtitle = "Transfers standardized as rupees per capita",
                          x = "Indicator Variable Value",
                          y = "Standardized Central Gov. Funding to States",
                          caption = "Data from Indian Ministry of Finance, Reserve Bank of India, and the U.N."
                        )

print(econ_facet_scatter)

# Saving plot to the working directory
ggsave(paste0(image_directory, "static_plot8_std_funding_development_indicators_scatter.png"), plot = econ_facet_scatter, width = 8, height = 6, dpi = 300)

######################## SHINY: DATA WRANGLING & PLOTTING ########################

# load shapefile and set crs
boundaries <- st_read(file.path(path, "shape_files/state_boundaries_simplified.geojson")) |> st_transform(boundaries, crs = 4326)
# clean up special characters
boundaries_clean <- boundaries |> 
  mutate(shapeName = stri_trans_general(shapeName, "Latin-ASCII")) |> 
  rename(state = shapeName) |> 
  select(state, geometry) |> 
  arrange(state)

# write merged shapefile for use in Shiny app
st_write(boundaries_clean, file.path(path, "shape_files/state_boundaries_clean.shp"), append=FALSE)

################### Plot 1: Hindu population by state #########################

# merge india df with boundaries
india_sf <- india_data |> 
  select(1, 19:31) |> 
  inner_join(boundaries_clean, by = "state") |> 
  st_sf()

# write merged shapefile for use in Shiny app
st_write(india_sf, file.path(path, "shape_files/india_sf.shp"), append=FALSE)

# choropleth of hindu population by state
hindu_state_map <- ggplot() +
                    geom_sf(data = boundaries_clean, fill = "white") +
                    geom_sf(data = india_sf, aes(fill = pct_hindus)) +
                    scale_fill_gradient(low = "white", high = "steelblue") +
                    labs(title = "Hindu Population by State in India",
                         subtitle = "(according to the 2011 census)",
                         fill = "Percentage") +
                    theme_minimal() +
                    theme(
                      plot.title = element_text(hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5),
                      legend.position = "right",
                      plot.caption = element_text(hjust = 0.5)
                    )

print(hindu_state_map)

# Saving plot to the working directory
ggsave(paste0(image_directory, "shiny_population_by_state.png"), plot = hindu_state_map, width = 8, height = 6, dpi = 300)

################### Plot 2: BJP vote share by state #########################

# choropleth of BJP vote share by state
BJP_2019_state_map <- ggplot() +
                        geom_sf(data = boundaries_clean, fill = "white") +
                        geom_sf(data = india_sf, aes(fill = BJP_vote_share)) +
                        scale_fill_gradient(low = "white", high = "maroon") +
                        labs(title = "BJP Vote Share by State in India",
                             subtitle = "(2019)",
                             fill = "Percentage") +
                        theme_minimal() +
                        theme(
                          plot.title = element_text(hjust = 0.5),
                          plot.subtitle = element_text(hjust = 0.5),
                          legend.position = "right",
                          plot.caption = element_text(hjust = 0.5)
                        )

print(BJP_2019_state_map)

# Saving plot to the working directory
ggsave(paste0(image_directory, "shiny_BJP_vote_by_state.png"), plot = BJP_2019_state_map, width = 8, height = 6, dpi = 300)

# find the centroid of each India state
india_centroid <- india_sf |> 
  mutate(geometry = st_centroid(geometry))

################### Plot 3: Total fund transfer from central gov #########################

# choropleth of fund transfer by program
# the grey states were established in 2017, so we lack data to plot them
fund_by_state_map <- ggplot() +
                      geom_sf(data = boundaries_clean, fill = "white") +
                      geom_sf(data = india_sf, aes(fill = total_funding_pgpc)) +
                      geom_sf(data = india_centroid,
                              pch = 21, fill = "orange", col = "black",
                              aes(fill = BJP_vote_share, size = BJP_vote_share)) +
                      scale_size_continuous(range = c(1, 5)) +
                      scale_fill_continuous(low = "lightblue", high = "navy") +
                      theme_minimal() +
                      labs(title = ("Total Fund Transfer from Central to State Government"),
                                          size = "BJP vote share", fill = "Funding amount") +
                      theme(plot.title = element_text(hjust = 0.5))

print(fund_by_state_map)

# Saving plot to the working directory
ggsave(paste0(image_directory, "shiny_fund_allocation.png"), plot = fund_by_state_map, width = 8, height = 6, dpi = 300)

####################### Plot 4: Constituency vote share #############################

# Loading the shapefiles for Indian parliamentary constituencies
cons <- st_read(file.path(path, "shape_files/india_pc_2019_simplified.geojson"))

cons <- cons |> 
  mutate(st_name = tolower(gsub("[[:space:][:punct:]]", "", st_name))) |> 
  st_transform(cons, crs = 4326)

# Merge the election dataframe with India boundaries shapefile
cons_vote_sf <- elections_clean |> 
  merge(cons,
        by.x = c("State_Name", "Constituency_No"),
        by.y = c("st_name", "pc_no"), all = TRUE) |> 
  st_as_sf() 

cons_vote_1984 <- cons_vote_sf|> 
  filter(Year == 1984) 

# plot the constituency vote share by year
con_map <- ggplot() +
            geom_sf(data = cons, fill = "white") +
            geom_sf(data = cons_vote_1984, aes(fill = BJP_vote_share)) +
            scale_fill_gradient(low = "orange", high = "red4", na.value = "white", limits = c(0, 100)) +
            labs(title = "BJP Constituency Vote Share (1984)",
                 fill = "Vote share (%)") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme_minimal()

print(con_map)

# Saving plot to the working directory
ggsave(paste0(image_directory, "shiny_constituency_vote_share.png"), plot = con_map, width = 8, height = 6, dpi = 300)

