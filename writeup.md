# Data & Programming II - R: Final Project Write-Up

#### Author: Grant Castle (grantcastle), Bella Huang (bellahxy), Varun Thampi (varunthampi)
#### Date created: Mar 4, 2024


In federal political systems, the central government collects national tax revenues and
redistributes funding to political subdivisions via a budget allocation process.(1) Much political
economy literature explores whether this budget process is influenced by politics, namely, whether
lawmakers redirect funding to their supporters.(2) This project examines this question in India,
where the Bharatiya Janata party (BJP) currently dominates, and asks two research questions. First,
broadly, do victorious political parties reward their voters via fiscal allocations? And second,
specifically, does the BJP redirect central government funding toward states in which they received
more support?


Since independence, Indian politics has been dominated by the Indian National Congress (INC), a
big-tent party supported by a broad coalition of religious and ethnic groups. In the 2014
elections, however, the rival Bharatiya Janata Party (BJP) secured a majority of seats in India’s
Parliament, the Lok Sabha.(3) The BJP expanded their majority in 2019 elections(4) and is expected
to do so again in upcoming 2024 elections.(5) The BJP has embraced a Hindu nationalist ideology(6)
and has enacted discriminatory laws against Muslims and other minority groups.(7) Given the
dominance of a party which prioritizes its Hindu supporters over others, India is a fruitful
context to examine the political budget cycle.


Data on India’s funding allocations was accessed from the Reserve Bank of India, the Open
Government Data Platform, the OpenBudgets Platform, the Ministry of Drinking Water and Sanitation,
and the Ministry of Finance. Demographic and development indicators data was accessed from the
Reserve Bank of India, the UN, and Wikipedia (via web-scraping). Election data was accessed from
Ahsoka University, shapefiles were accessed from the UN and the ‘Community Created Maps of India’
project on Github, and human rights reports were web-scraped from the U.S. State Department and
Human Rights Watch webpages. The funding and demographics/development indicators datasets were
first cleaned and merged at the state-level to isolate FY 2021-2022 data. We used both the total
central government funding allocation to each state as well as allocations under six specific
government programs.(8) Due to variations in populations of Indian states, the data was
standardized per capita. The state-wise funding allocations were then merged with shapefiles and
added to a shiny application to allow users to view the standardized funding by state on a map of
India, with toggles for funding programs.


Next, we cleaned election data from Ahsoka University. To create a measure of ‘BJP vote share,’ we
filtered the dataset to BJP candidates and calculated the number of votes won by BJP candidates divided
by the total number of electors in each constituency. This was repeated at the state level by
aggregating votes received by all BJP candidates within a state divided by the total number of electors
in that state. These metrics were then merged with shapefiles and converted into a shiny app to create
maps (at the constituency level for 1984-2019 and the state level for 2019) to display BJP vote shares,
with toggles by year. The relationship between the BJP’s state-level vote share (2019) and the
state-level Hindu population proportion was also displayed via a shiny map, a barplot with the two
metrics side by side, and a scatterplot and a regression analysis, which showed that all else constant,
a 1 percentage point increase in a state’s Hindu population proportion is associated with a
statistically significant 0.274 percentage point increase in the BJP’s vote share. Finally, we used
web-scraped U.S. State Department and Human Rights Watch reports explore human rights under the BJP.
Via Bing and sentimentr sentiment analysis, both sets of reports revealed consistently negative
sentiment associated with human rights in India, while the Human Rights Watch reports revealed an
increasingly negative sentiment since 2014. 

Finally, scatterplots of standardized central government funding by state (total funding and funding
for specific schemes) on the BJP vote share by state revealed no clear visual association between the
two. This was confirmed by OLS regressions, which did not find any statistically significant
relationship between BJP vote share and central government funding. We then explored whether other
economic/development indicators, such as unemployment rates, GDP, or Multidimensional Poverty Index
(MPI) scores explain funding allocations. Scatterplots of standardized funding on these indicators fail
to show any clear or persistent correlations, and OLS, lasso, ridge, and scaled coefficient regressions
confirm the lack of statistically significant correlation. Therefore, we lack evidence of a
statistically significant relationship between state-level BJP vote share and funding allocations,
which may be a sign of an independent bureaucracy. However, we also surprisingly lack evidence of a
statistically significant relationship between economic/development indicators and funding allocations,
which suggests that other, potentially malign, dynamics may still influence funding allocations.


Three key weaknesses emerged in this project. First, by aggregating data by state, the analysis only
had 36 observations, which is insufficient to conduct robust regression analysis. Initially, we hoped
to use constituency-level data (543 observations). However, funding data was not publicly available at
the constituency level. Second, even the state-level data for India was difficult to utilize.
Statistics on funding and demographics were not available in a single dataset, which necessitated
aggregating dozens of different datasets from various government data portals and ministries. These
data sources used inconsistent naming conventions, units, and methods, which made merging difficult.
Third, using BJP vote share as a proxy for the BJP’s support in a region is an imprecise estimator. The
BJP works in a coalition with dozens of very small parties. However, these parties are so numerous and
inconsistent that it was infeasible to account for votes for these parties in the ‘BJP vote share’
metric. Therefore, our metric of support for the BJP government reflects only support for the BJP
itself rather than, as would be more practical, support for the BJP coalition. To improve and expand on
these results, three steps could be taken. First, one could find data on funding allocations and BJP
vote shares at constituency or district levels, which may reveal more subtle variations and allow more
observations to improve the regressions. Second, one could examine time-series data. Doing so may
reveal, for example, whether districts with lower BJP vote shares have a downward trend in funding
allocations over time. Third, given that U.S. State Department reports use subtle and diplomatic
language, more advanced NLP tools such as the Stanford coreNLP Process may be needed to fully discern
sentiment trends in these documents.


Notes and Sources:
* (1) Lawhorn, Julie M. “Federal Grants to State and Local Governments: A Historical Perspective on
Contemporary Issues.” crsreports.com, Congressional Research Service (CRS), 22 May 2019.
* (2) Aaskoven, Lasse, and David Dreyer Lassen. “Political Budget Cycles.” Oxford Research Encyclopedia
of Politics, 26 Apr. 2017.
* (3) Price, Gareth. “Democracy in India .” chathamhouse.org, Chatham House, 7 Apr. 2022.
* (4) Haas, Nicholas, and Rajeshwari Majumdar. “How Ideology Shapes Indian Politics.”
carnegieendowment.org, Carnegie Endowment for International Peace, 18 Dec. 2023.
* (5) Ellis-Peterson, Hannah. “BJP Win in India’s 2024 General Election ‘Almost an Inevitability.’” The
Guardian, Guardian News and Media, 31 Dec. 2023.
* (6) Haas.
* (7) Truschke, Audrey. “How India’s BJP Is Weaponizing History against Muslims.” Time, Time, 6 Oct.
2023.
* (8) The six funding programs examined are SBM (public sanitation), JJM (clean water), PM Kisan (income
support for poor farmers), PMAY-G (rural housing), MGNREGA (national employment guarantee), and PMJDY
(banking services and financial inclusion). 
