# Data and Programming in R: Final Project

#### Author: Grant Castle, Bella Huang, Varun Thampi
#### Date created: Mar 4, 2024
#### R version: 4.3.2

## Overview
This project is part of the "Data and Programming in R" course and provides code and output for the final project. It includes ten R scripts that perform data wrangling, plotting, text processing, and analysis on the relationship between political favoritism and fiscal allocation from the central government to administrative subdivisions in India.

Broadly, the project collects and cleans datasets related to Indian central government fiscal allocations to states, state-level economic and demographic indicators, and election results. The project uses this data to make a series of static plots, shiny apps, and regression models to  investigate the relationship between fiscal allocations and the BJP's vote share in Indian states. The project also conducts text processing using human rights reports to analyze the potential decline of human rights in India under the BJP's rule (2014-2022), by contrasting it with the state of human rights under the tenure of the INC (2005-2013).

Note: the published shiny app is available here: https://bella-huang.shinyapps.io/india-political-support-and-fiscal-rewards/

## Files to Run
1. **data.R**
* Reads various datasets (contained in the data/raw_data folder) related to central government funding allocations, control variables, economic/development indicators, and election results.
* Scrapes data from a Wikipedia page related to the Hindu population in India.
* Cleans all datasets by extracting relevant information, renaming columns, filtering unnecessary rows, and calculating funding per GDP per capita 
* Merges the cleaned datasets for central government funding, Hindu population, and control variables by state into a single, solidified india_data dataframe which is used throughout the remainder of the project.
* Creates a separate dataset election dataset which contains election results by parliamentary constituency for use in a later shiny application. 
* Exports the cleaned final datasets to be used for further analysis.
* Note: Different versions of R may read the excel files in this code differently or may need to read them as read_xls rather than read.xls, largely due to issues in the original formatting of the excel spreadsheet. The current code runs properly and loads/cleans all data on the group members' devices, but other users may need to slightly modify how the excel files are read / cleaned depending on their version of R. 

2. **staticplot.R**
* A. Creatres eight static plots that provide insight into India’s central government funding distribution, the BJP's electoral success, and the link between the BJP's vote share and the Hindu makeup of each state.
	* Plot 1: Displays the central government's funding allocation to each state in raw numbers (rupees) including 		both the total funding allocation as well as funding allocations for six central government funding schemes 		related to sanitation, financial inclusion, employment guarantees, support for farmers, drinking water, and 		housing.
	* Plot 2: Displays the central government's funding allocation to each state in rupees per capita including 		both the total funding allocation as well as funding allocations for six central government funding schemes 		related to sanitation, financial inclusion, employment guarantees, support for farmers, drinking water, and 		housing.
	* Plot 3: Displays the share of eligible voters who voted for BJP candidates in each national parliamentary 		election 1984 - 2019.
	* Plot 4: Displays the share of eligible voters who voted for BJP candidates in each state in the 2019 			elections.
	* Plot 5: Displays a dual barplot comparing side-by-side the proportion of votes won by BJP candidates in each 		state in the 2019 elections and the proportion of that state's population which is Hindu.
	* Plot 6: Displays a scatterplot with a line of best fit of the proportion of votes won by BJP candidates in 		each state in the 2019 elections and the proportion of that state's population which is Hindu.
	* Plot 7: Displays facet-wrapped scatterplots of the standardized funding (i.e., per capita) for each state 		(including both total funding and funding for each of the six specific government schemes) and the BJP's vote 		share in each state in 2019.
	* Plot 8: Displays facet-wrapped scatterplots of the standardized funding (i.e., per capita) for each state 		(only the total funding) in 2019 and several economic/development indicatators by state such as unemployment 		rates, MPI, literacy rates, power availability, etc. 
* B. Four static plots for Shiny.R
	* Plot 1: Displays a choropleth map of India containing the proportion of each state's population which is 		Hindu. 
	* Plot 2: Displays a choropleth map of India containing the proportion of each state's eligibile electors which 	voted for BJP candidates in the 2019 elections. 
	* Plot 3: Displays a choropleth map of India containing the total standardized funding (i.e., per capita) from 		the central government received by each state in FY 2021-2022.
	* Plot 4: Displays a choropleth map of India containing the proportion of each constituency's eligibile electors 	which voted for BJP candidates in the 1984 elections.

3. **shinyapp.R**
* Creates a Shiny dashboard app with three tabs displaying different interactive maps:
* A. “Fiscal allocation” tab:
	* State-wise fiscal allocation per capita based on user-selected programs (total funding and funding under six 		different government schemes) displayed via chorpleth coloring of state.
	* Bubble dot on each state displaying the BJP vote share in that state in 2019, with larger dots indicating 		larger vote shares.
	* Dynamically updated plot and table based on user-selected inputs. 
* B. “Demographic” tab:
	* Choropleth plots displaying the state-wise Hindu population proportion or BJP vote share in 2019 elections, 		based on user-selected inputs.
	* Choropleth plots with tooltips
* C. “Election Outcome” tab:
	** Choropleth map displaying the constituency-wise BJP vote share for all national parliamentary elections 		(1984-2019).
	** Dynamically updated plot and table based on user-selected inputs of election years. 

4. **model.R**
* Conducts seven separate regressions to assess the relationship between central government funding allocations and state-level characteristics (such as BJP vote share, MPI, literacy rates, GDP, etc.) as well as the relationship between state-level BJP vote share and Hindu population proportions. 
* Model 1: OLS regression of raw total central government funding on BJP vote share and various controls.
	* Identified that the funding is highly correlated with population.
*  Model 2: OLS regression of total central government funding per capita on BJP vote share and controls.
	* Observed that BJP vote share is not statistically significantly correlated with total central government 		funding.
* Model 3: OLS regression of BJP vote share on the proportion of the population that is Hindu with other controls.
	* Found a statistically significant positive correlation between BJP vote share and the proportion of the 		population that is Hindu. 
* Model 4: OLS regression of funding/beneficiaries per capita for six specific programs on BJP vote share and various economic/development indicators.
	* Confirmed the results from model 2, namely that BJP vote share is not statistically significantly correlated 		with central government funding under any of the six examined central government funding schemes. 
* Models 5 and 6: Ridge and Lasso regression of total central government funding per capita on BJP vote share and various economic/development indicator variables.
	* In both models, three predictors BJP_vote_share, urban_unemp, and mpi_score remained large / not shrunk to 		zero, suggesting that each variable may contain some predictive power. 
	* However, both models had high MSEs, suggesting possible overfitting, and overall the results were largely 		inconclusive.
* Model 7: OLS regression of total central government funding per capita on standardized/scaled predictors.
	* Model found slightly statistically significant correlations between pct_hindus and mpi_score and total 		funding per capita, but neither results is very significant, makes practical sense, or aligns with results from
	prior regressions, suggeseting they may simply be due to random error.
* Broadly, none of the models show a statisticall significant, practically significant, or consistent association between central government funding allocations and BJP vote share or other economic/development indicator variables.


5. **textprocess.R**
* Web scrapes and analyzes:
	* Human Rights Watch reports for India (2006-2023)
	* US State Department reports for India (2005-2013 for INC, 2014-2022 for BJP)
* Conducts sentiment analysis using sentimentr and Bing lexicon and creates plots.
  	* The State Department BING and SentimentR plots (3 plots) provide an overall negative outlook (sentimentR values are less than zero). However, no marked change has taken place since the BJP took office. Therefore, the results from this analysis does not indicate an increase in negative sentiment since the BJP came to power. These results could however indicate that the State Department Reports use a consistently neutral langauge across all their reports. In order to validate whether there was genuinely no change in negative sentiment, we conduct a second set of text analysis, with Human Rights Watch reports.
  	* The Human Rights Watch BING and SentimentR plots (3 plots) provide an interesting pattern. The BING analysis clearly indicates a decline in positive sentiment, and an increase in negative sentiment since the BJP took office in 2014. The Sentiment analysis provides a gradual decline in the sentiment scores since 2014, however the confidence intervals are overlapping, which does not indicate a strong result. Nevertheless, we conclude the text processing section of the project with some evidence for human rights concerns since the BJP took office. Subsequent research will make use of more robust NLP approaches to arrive at a definite answer. 
 * Plots the output of the sentiment analysis. Details provided in the output section of this file.

## Before Running the Files:
Make sure the following R packages are installed: 
* tidyverse(v2.0.0), shiny(v1.8.0), shinydashboard(v0.7.2), plotly(v4.10.4), sf(v1.0.15), DT(v0.32), dplyr(v1.1.4), readxl(v1.4.2), openxlsx(v4.2.5.2), gdata(v2.18.0.1), rvest(v1.0.3), tidytext(v0.4.1), textdata(v0.4.4), sentimentr(v2.9.0), udpipe(v0.8.11), SnowballC(v0.7.1) , glmnet(v4.1.8), sf(v1.0.15), stringi(v1.8.3), ggplot2(v3.4.4)

Edit the path in each R file to your relevant working directory and read files into your desired folder:
*data.R*:
line 19 setwd("/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/raw_data")
(set the working directory to the raw_data folder within the data folder to extract all of the raw data files)

line 430: file = "/Users/grantcastle/Documents/final-project-castle-huang-		thampi/data/cleaned_datasets/central_gov_funding.csv"
(export to the cleaned_datasets subfolder within the data folder)

line 433: file = "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets/controls.csv"
(export to the cleaned_datasets subfolder within the data folder)

line 560: file = "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets/elections_2019_state.csv"
(export to the cleaned_datasets subfolder within the data folder)

line 582: file = "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets/india_data.csv"
(export to the cleaned_datasets subfolder within the data folder)

line 615: file = "/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets/elections_clean.csv"
(export to the cleaned_datasets subfolder within the data folder)

*model.R*:
line 15: setwd("/Users/grantcastle/Documents/final-project-castle-huang-thampi/data/cleaned_datasets")
(set the working directory to the cleaned_datasets folder within the data folder to pull all of the cleaned data file)

*shinyapp.R*:
line 126: path <- "/Users/bellahuang/Documents/GitHub/final-project-castle-huang-thampi/data"
(set the path to the generalized data folder, since we will later pull files from both the shape_files and cleaned_datasets subfolders, which are pre-specified)

*staticplot.R*:
line 15: path <- "/Users/bellahuang/Documents/GitHub/final-project-castle-huang-thampi/data"
(set the path to the generalized data folder, since we will later pull files from both the shape_files and cleaned_datasets subfolders, which are pre-specified)
line 19: image_directory <- "/Users/grantcastle/Documents/final-project-castle-huang-thampi/images/"
(set the image_directory to the location in the working directory where you would like to save all image files to)

*textprocess.R*:
line 21: path <- "C:/Users/Dell/Documents/GitHub/final-project-castle-huang-thampi"
(set the path to the general folder for the project)
line 22: image_path <- "C:/Users/Dell/Documents/GitHub/final-project-castle-huang-thampi/images"
(set the path to the images sub_folder for the project to export images)

## Order of Execution
1. Run data.R to clean and consolidate datasets on central government funding, control variables, and election results.
2. Run staticplot.R for static plots on funding, vote share, and demographic insights, including those tailored for the Shiny app.
3. Run shinyapp.R to create an interactive Shiny dashboard with tabs on fiscal allocation, demographics, and election outcomes.
4. Run textprocess.R to analyze sentiment in Human Rights Watch and US State Department reports on India.
5. Run model.R for regression analyses exploring relationships between central government funding, BJP vote share, and state-level characteristics.

## Source Data:
* *TCPD_GE_All_States_2024-2-18.csv*: Election data detailing the votes received and total number of electors for all candidates from all parliamentary constitencies for India's elections over the past several decades. Source: Ahsoka Univeristy (https://tcpd.ashoka.edu.in/lok-dhaba/)
* *State wise Allocation Release Expenditure (3).xls* Data detailing the total funding received by each state under the JJM program for FY 2021-2022. Source: Department of Drinking Water and Sanitation (https://ejalshakti.gov.in/JJM/JJMReports/Financial/JJMRep_StatewiseAllocationReleaseExpenditure.aspx)
* *RS_Session_260_AU_1210_1.csv*: Data detailing the total funding received by each state under the SBM-G program for FY 2021-2022. Source: Indian Open Government Data Platform (https://data.gov.in/resource/stateut-wise-details-centre-share-funds-allocated-funds-drawn-states-and-utilization)
* *RS_Session_258_AU_321_1.csv*: Data detailing the total number of benefciaires by state under the PM Kisan program for FY 2021-2022. Source: Indian Open Government Data Platform ([RS_Session_258_AU_321_1.csv](https://data.gov.in/resource/state-wise-details-number-beneficiaries-paid-under-pradhan-mantri-kisan-samman-nidhi))
* *Statewise Report 16_02_2024 00_15_20.xls*: Data detailing the total number of beneficiaries by state under the PMJDY program since its inception in 2014. Source: Minsitry of Finance (https://pmjdy.gov.in/statewise-statistics)
*  *mgnrega.csv*: Data detailing the total funding received by each state under the MGNREGA program for FY 2021-2022. Source: Open Budgets India (https://openbudgetsindia.org/dataset/mahatma-gandhi-national-rural-employment-guarantee-scheme)
*  *pmay---g (1).csv*: Data detailing the total funding received by each state under the PMAY-G program for FY 2021-2022. Source: Open Budgets India (https://openbudgetsindia.org/dataset/pradhan-mantri-awaas-yojana-grameen)
*  *STATEMENTS1767A7D187DE374B978D236D809F6AA764.XLSX*: Data detailing the total funding from all sources devolved from the Indian Central Government to each state in FY 2021-2022. Source: Reserve Bank of India - Statement 17 (https://m.rbi.org.in/Scripts/AnnualPublications.aspx?head=State+Finances+%3a+A+Study+of+Budgets)
*  *1T_151120231B719A3B3FB54571BC2734C0E46D35C0.XLSX*: Data detailing the total population of each state in India, figures based on 2011 census. Source: Reserve Bank of India - Table 1 (https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States)
*  *2T_1511202349241689F9D84E3498083E57333A1BE1 (1).XLSX*: Data detailing the total rural population of each state in India, figures based on 2011 census. Source: Reserve Bank of India - Table 2 (https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States)
*  *27T_15112023E301A02422494F73BFAFD6CDD84EEEAE.XLSX*: Data detailing the GDP (in rupees) of each Indian state in FY 2021-2022. Source: Reserve Bank of India - Table 27 (https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States)
*  *137T_151120232F082F87CB0E4ACFA9D5A729A588CE64.XLSX*: Data detialing the availability of power in each India state in FY 2021-2022. Source: Reserve Bank of India - Table 137 (https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States)
*  *164T_1511202373889E6B391E4FEBA99E14283FAA4BD9 (1).XLSX*: Data detailing the amount of tax revenue (in rupees) collected by each state in India in FY 2021-2022. Source: Reserve Bank of India - Table 164 (https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States)
* *15T_151120231039403CA0F14AE2A7B1E5BE0013AEBB.XLSX*: Data detailing the unemployment rate among the rural population in each state in India in FY 2021-2022.  Source: Reserve Bank of India - Table 15 (https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States)
* *16T_1511202316D3A8CA1A054072A0616576931B3CBE.XLSX*: Data detailing the unemployment rate among the urban population in each state in India in FY 2021-2022.  Source: Reserve Bank of India - Table 16 (https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States)
* *6T_15112023078654FD8A3F4938AE67231CB8148F96.XLSX*: Data detailing the literacy rate of the population for each state in India, figures based on 2011 census. Source: Reserve Bank of India - Table 6 (https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States)
* *ind_hot (4).xlsx*: Data detailing the multidimensional poverty index (MPI) score for each Indian State, calculated between 2019-2021. Source: United Nations Office of the Coordinator of Humanitarian Affairs (OCHA) Humanitarian Data Exchange / Oxford Poverty & Human Development Initiative (https://data.humdata.org/dataset/india-mpi?)
* *18T_15112023599424EFBB2644E29EC8ECF31DE3CFEB (2).XLSX*: Data detailing the multidimensional poverty index (MPI) score for each Indian State, calculated between 2019-2021. Source: Reserve Bank of India - Table 18. (https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States)
* *shape_files/india_pc_2019_simplified.geojson*: Shapefiles delineating the geographic boundaries of India's parliamentary constituencies. Source: Community Created Maps of India Project (https://projects.datameet.org/maps/parliamentary-constituencies/)
* *state_coundaries_simplified.geojson:* Shapefiles delineating the geographic boundaries of India's state's and union territories. Source: United Nations Office of the Coordinator of Humanitarian Affairs (OCHA) Humanitarian Data Exchange / geoBoundaries Global Database of Political Administrative Boundaries Database (https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-india)
* *hindu_population_table*: Datatable created by web-scraping a data table of each state in India and the count and proportion of that state's population which is Hindu, based on the 2011 census. Source: Wikipedia ([hindu_population_table](https://en.wikipedia.org/wiki/Hinduism_in_India)
* *hrw_texts*: Compilation of text files created by web-scraping reports on human rights in India from Human Rights Watch (HRW) from 2005-2023. Source: Human Rights Watch (https://www.hrw.org/world-report/2023/country-chapters/india)
* *text_df*: Compilation of text files created by web-scraping reports on human rights in India from the U.S. State Department's Annual Couintry Reports on Human Rights Practices from 2005 to 2023. Source: U.S. State Department (https://2009-2017.state.gov/j/drl/rls/hrrpt/2005/61707.htm) and (https://www.state.gov/reports/2022-country-reports-on-human-rights-practices/india).


## Output

1. Plots
* *hrw_bing_plot.png:* Bing Sentiment analysis plot for the Human rights reports, aggregated by year
* *hrw_sentiment_plot.png:* SentimentR analysis plot for the Human rights reports, aggregated by year
* *hrw_sentiment_plot_party.png:* SentimentR analysis plot for the Human rights reports, aggregated by party
* *state_dep_bing_plot.png:* Bing Sentiment analysis plot for the US State Department reports, aggregated by year
* *state_dep_sentiment_plot.png:* SentimentR analysis plot for the US State Department reports, aggregated by year
* *state_dep_sentiment_plot_party.png:* SentimentR analysis plot for the US State Department reports, aggregated by party
* *static_plot1_raw_{x}.png*: Displays the central government's funding allocation to each state in raw numbers (rupees) including both the total funding allocation (total_funding) as well as funding allocations for six central government funding schemes related to sanitation (SBMG_funding), financial inclusion (PMJDY_beneficiaries), employment guarantees (MGNREGA_funding), support for farmers (PM Kisan_beneficiaries), drinking water (JJM_funding), and housing (PMAY-G_funding).
* *static_plot1_std_{x}.png*: Displays the central government's funding allocation per capita to each state (rupees) including both the total funding allocation (total_funding) as well as funding allocations for six central government funding schemes related to sanitation (SBMG_funding), financial inclusion (PMJDY_beneficiaries), employment guarantees (MGNREGA_funding), support for farmers (PM Kisan_funding), drinking water (JJM_funding), and housing (PMAY-G_funding).
* *static_plot3_BJP_vote_share_years.png* Displays the share of eligible voters who voted for BJP candidates in each national parliamentary election 1984 - 2019.
* *static_plot4_BJP_vote_share_state.png* Displays the share of eligible voters who voted for BJP candidates in each state in the 2019 elections.
* *static_plot5_BJP_vote_Hindu_barplot.png*: Displays a dual barplot comparing side-by-side the proportion of votes won by BJP candidates in each state in the 2019 elections and the proportion of that state's population which is Hindu.
* *static_plot6_BJP_vote_Hindu_scatter.png*: Displays a scatterplot with a line of best fit of the proportion of votes won by BJP candidates in each state in the 2019 elections and the proportion of that state's population which is Hindu.
* *static_plot7_std_funding_BJP_vote_scatter.png*: Displays facet-wrapped scatterplots of the standardized funding (i.e., per capita) for each state (including both total funding and funding for each of the six specific government schemes) and the BJP's vote share in each state in 2019.
* *static_plot8_std_funding_development_indicators_scatter.png*: Displays facet-wrapped scatterplots of the standardized funding (i.e., per capita) for each state (only the total funding) in 2019 and several economic/development indicatators by state such as unemployment rates, MPI, literacy rates, power availability, etc. 
* *shiny_population_by_sate.png*: Displays a choropleth map of India containing the proportion of each state's population which is Hindu. 
* *shiny_BJP_vote_by_state.png*: Displays a choropleth map of India containing the proportion of each state's eligibile electors which voted for BJP candidates in the 2019 elections. 
* *shiny_fund_allocation.png*: Displays a choropleth map of India containing the total standardized funding (i.e., per capita) from the central government received by each state in FY 2021-2022.
* *shiny_constituency_vote_share.png*: Displays a choropleth map of India containing the proportion of each constituency's eligibile electors which voted for BJP candidates in the 1984 elections.

2. Text
* *{20XX}-Country Reports on Human Rights Practices:* The folder *human_rights_reports* includes human rights reports for the BJP (2014-2022) downloaded from the US State Department website. Each file is named after its publishing year. Note: State Department Human Rights reports for the INC (2005-2013), and all Human Rights Watch reports were webscraped, and directly used within the textprocess.R code

3. Cleaned Data Sets
* *hindu_population_table.csv:* Hindu population table scraped from the wikipedia page "Hinduism in India" (Source: https://en.wikipedia.org/wiki/Hinduism_in_India#:~:text=Hinduism%20is%20the%20largest%20religion,of%20the%20global%20Hindu%20population.). The file has four column denoting region (state or union territory name), the number of Hindus in that state, the total population of that state, and the percent of that state's population which is Hindu.
* *central_gov_funding.csv:* Dataset containing metrics on India's central government funding allocated to states that has been collated from seven separate datasets availble from various government ministries in India. The dataset contains one column 'state' denoting the state name and 6 columns each denoting the raw total amount of funding (in rupees) allocated to that state under the program specified in the column title or the total number of beneficiaries in each state under the program specified in the column title.
* *controls.csv:* Dataset containing metrics on on India's state for a variety of demographic, economic, and development indicators sourced from various Indian government ministries and international agencies. The dataset contains one column 'state' denoting the state name and 9 columns denoting characteristics for that state including total population, total rural population, rural unemployment rate (%), urban unemployment rate (%), tax revenue (rupees), gdp (rupees), power availability (power units), literacy rate (%), and MPI score.
* *elections_2019_state.csv*: Dataset containing metrics on the BJP's vote share in each Indian state, source from Ahsoka University and cleaned/filtered to only display state-level BJP vote shares in the 2019 parliamentary elections. The dataset contains four columns: 'state' denoting the state/union territory name, 'votes' indicating the total number of votes won by BJP candidates in that state in 2019, 'eligible_voters' indicating the total number of eligible voters in that state in 2019, and BJP_vote_share indicating the percentage of eligble voters who voted for BJP candidates in 2019.
* *india_data.csv:* Dataset created by merging the prior four datasets hindu_population_table, central_gov_funding, controls, and elections_2019_state. This is the primary cleaned and merged dataset used for static plots, shiny applications, and models. The dataset contains 31 columns: a 'state' column indicating the name of the state, 7 columns from central_gov_funding.csv which indcate the raw funding/beneficiaries for each state (total as well as for the six selected government programs), 7 new columns indicating the funding/beneficiries for each state per capita (i.e., raw total as well six program-specific totals divided by population to create per capita metrics), 9 columns from controls.csv indicating the total population, total rural population, rural unemployment rate (%), urban unemployment rate (%), tax revenue (rupees), gdp (rupees), power availability (power units), literacy rate (%), and MPI score for each state, 1 new column displaying the percent of a state's population that is rural (rural population divided by total population), 1 new column UT which is an indicator variable for if the state is a union territory (1) or not (0), 2 columns from hindu_population_table.csv indicating the total number of hindus in each state and the percentage of the state's population that is Hindu, 3 columns from elections_2019_state.csv indicating the number of votes in each state won by the BJP, the total number of eligible voters in each state, and the percent of eligible voters who voted for the BJP.
* *elections_clean_csv:* A separate election dataset compiled using the same raw data from Ahsoka University, but cleaned and filtered to display the total number of votes won by BJP candidates in each parliamentary constituency (rather than state) for all parliamentary elections 1984-2019 (rather than just 2019). The dataset has 8 columns denoting the name of the state (State_name), the constituency number within that state (Constituency_No), the election year (year), the party (party), the votes received by the BJP in that constituency in that year (Votes), the total number of eligible voters in that constituency in that year (Electors), the name of the constituency, which corresponds to the constituency number (Constituency_Name), and that BJP vote share denoting the share of eligible electors who voted for BJP candidates in that constituency in that year (BJP_vote_share). This dataset is used for the shiny application tab displaying the BJP vote share by constituency over election years.

4. Shape files
* *india_sf.{dbf/prj/shp/shx}:* Merged India shapefile, combining demographic data and state boundaries. It saved as "india_sf.shp" for utilization in the Shiny app.
* *state_boundaries_clean.{dbf/prj/shp/shx}:* Cleaned Indian state boundaries shapefile prepared for use in the Shiny app.
* *AC_All_Final.{dbf/prj/shp/shx}:* Shapefiles used to construct the parliamentary constituencies of India maps in the Shiny app.

5. Models (contained within the model.R file)
* Model 1: OLS regression of raw total central government funding on BJP vote share and various controls.
	* Identified that the funding is highly correlated with population.
*  Model 2: OLS regression of total central government funding per capita on BJP vote share and controls.
	* Observed that BJP vote share is not statistically significantly correlated with total central government funding.
* Model 3: OLS regression of BJP vote share on the proportion of the population that is Hindu with other controls.
	* Found a statistically significant positive correlation between BJP vote share and the proportion of the population that is Hindu. 
* Model 4: OLS regression of funding/beneficiaries per capita for six specific programs on BJP vote share and various economic/development indicators.
	* Confirmed the results from model 2, namely that BJP vote share is not statistically significantly correlated with central government 		funding under any of the six examined central government funding schemes. 
* Models 5 and 6: Ridge and Lasso regression of total central government funding per capita on BJP vote share and various economic/development indicator variables.
	* In both models, three predictors BJP_vote_share, urban_unemp, and mpi_score remained large / not shrunk to zero, suggesting that each 	variable may contain some predictive power. 
	* However, both models had high MSEs, suggesting possible overfitting, and overall the results were largely inconclusive.
* Model 7: OLS regression of total central government funding per capita on standardized/scaled predictors.
	* Model found slightly statistically significant correlations between pct_hindus and mpi_score and total funding per capita, but 		neither results is very significant, makes practical sense, or aligns with results from prior regressions, suggeseting they may simply 		be due to random error.
* Broadly, none of the models show a statisticall significant, practically significant, or consistent association between central government funding allocations and BJP vote share or other economic/development indicator variables.


