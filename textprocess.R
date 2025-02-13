# This file contains text processing code for two parts:
# 1. Human Rights Watch reports for India (2006-2023)
# 2. US State Department reports for India (2005-2013 for INC, 2014-2022 for BJP)

# The code performs the following operations for both reports:
# 1. Web scrapes and analyzes the reports
# 2. Conducts sentiment analysis using sentimentr and Bing lexicon.
# 3. Visualizes sentiment trends over the years and by political party (BJP or INC).
# 4. Generates and saves sentiment analysis plots

# Loading the required libraries
library(tidytext)
library(textdata)
library(sentimentr)
library(udpipe)
library(SnowballC)
library(tidyverse)
library(rvest)

# Setting your own path here
path <- "/Users/grantcastle/Documents/final-project-castle-huang-thampi/"
image_path <- "/Users/grantcastle/Documents/final-project-castle-huang-thampi/images"
setwd(path)

######################### HRW HUMAN RIGHTS ANALYSIS ###############################

#Reading in files for through webscraping
url <- "https://www.hrw.org/world-report/2023/country-chapters/india"
url_content <- read_html(url)

text <- url_content %>% 
  html_elements("div.rich-text.mx-auto") %>% 
  html_text2()

url_list <- vector("character", length = 18)
y <- 2006

for (i in 1:18) {
  url_list[i] <- 
    str_c("https://www.hrw.org/world-report/", y, "/country-chapters/india")
  y <- y + 1
}

years <- c(2005:2022)

hrw_texts <- list()
for (i in 1: length(url_list)) {
  url_content <- read_html(url_list[i])
  text <- url_content %>% 
    html_elements("div.rich-text.mx-auto") %>% 
    html_text2() 
  
  hrw_texts[[i]] <- list(text = text, year = years[i])
}

#Creating a dataframe from the list

text_df <- bind_rows(hrw_texts)

#Adding a column for identifying the political party in charge

text_df <- text_df %>% 
  mutate(party = ifelse(year >= 2014, "bjp", "inc")) %>% 
  select(year, party, text)

#--- Basic sentiment analysis using sentimentr ----

text_df_sentences <- unnest_tokens(text_df, sentence_tokens, text,
                                   token = "sentences")

sentiment_scores <- sentiment_by(text_df_sentences$sentence_tokens, 
                                 by = text_df_sentences$year)

sentiment_scores <- sentiment_scores %>% 
  mutate(upper_ci = ave_sentiment + 1.96*sd,
         lower_ci = ave_sentiment - 1.96*sd)

sentiment_plot <- sentiment_scores %>% 
  ggplot()+
  geom_point(aes(x=year, y = ave_sentiment), color = "darkred")+
  geom_errorbar(aes(x = year, ymin = lower_ci, ymax = upper_ci), width = 0)+
  scale_x_continuous(limits = c(2005, 2022), breaks = seq(2005, 2022, by = 1))+
  scale_y_continuous(limits = c(-1.5, 1.5))+
  labs(y = "Mean sentiment",
       title = "Average sentiment from Human Rights Watch Reports")

sentiment_plot
ggsave(file.path(image_path, "hrw_sentiment_plot.png"), sentiment_plot,
       width = 8, height = 6, dpi = 300)

#--- Sentiment analysis using sentimentr grouping by party ----

sentiment_scores_party <- sentiment_by(text_df_sentences$sentence_tokens, 
                                       by = text_df_sentences$party)

sentiment_scores_party <- sentiment_scores_party %>% 
  mutate(upper_ci = ave_sentiment + 1.96*sd,
         lower_ci = ave_sentiment - 1.96*sd)

sentiment_plot_party <- sentiment_scores_party %>% 
  ggplot()+
  geom_point(aes(x=party, y = ave_sentiment), color = "darkred")+
  geom_errorbar(aes(x = party, ymin = lower_ci, ymax = upper_ci), width = 0)+
  scale_y_continuous(limits = c(-2, 2))+
  labs(y = "Mean sentiment",
       x = "Party",
       title = "Average sentiment from Human Rights Watch Reports",
       subtitle = "No significant change in sentiment is observed for both parties: INC from 2005-13, BJP from 2014-22")

sentiment_plot_party
ggsave(file.path(image_path, "hrw_sentiment_plot_party.png"), sentiment_plot_party,
       width = 8, height = 6, dpi = 300)

#--- Basic sentiment analysis using Bing ----

text_df_words <- unnest_tokens(text_df, word_tokens, text,
                               token = "words")
text_df_words <- text_df_words %>% 
  anti_join(stop_words, by = c("word_tokens" = "word"))

bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")

text_df_words <- text_df_words %>% 
  inner_join(bing, by = c("word_tokens" = "word")) %>% 
  inner_join(afinn, by = c("word_tokens" = "word"))

bing_plot <- text_df_words %>% 
  filter(!is.na(sentiment)) %>% 
  group_by(year, sentiment) %>% 
  summarize(count = n()) %>% 
  ggplot()+
  geom_col(aes(x = factor(year), y = count, fill = sentiment), position = "dodge")+
  labs(x = "Year", y = "Count", fill = "Sentiment",
       title = "BING Sentiment Analysis from Human Rights Watch Reports")

bing_plot
ggsave(file.path(image_path, "hrw_bing_plot.png"), bing_plot,
       width = 8, height = 6, dpi = 300)

#--- Basic sentiment analysis using AFINN ----
afinn_plot <- text_df_words %>% 
  filter(!is.na(value)) %>% 
  group_by(year) %>% 
  summarize(mean = mean(value)) %>% 
  ggplot()+
  geom_line(aes(x = year, y = mean))

afinn_plot

###################### STATE DEPARTMENT HUMAN RIGHTS ANALYSIS ########################

#Reading in files for INC in power through webscraping

url_list <- c("https://2009-2017.state.gov/j/drl/rls/hrrpt/2005/61707.htm",
              "https://2009-2017.state.gov/j/drl/rls/hrrpt/2006/78871.htm",
              "https://2009-2017.state.gov/j/drl/rls/hrrpt/2007/100614.htm",
              "https://2009-2017.state.gov/j/drl/rls/hrrpt/2008/sca/119134.htm",
              "https://2009-2017.state.gov/j/drl/rls/hrrpt/2009/sca/136087.htm",
              "https://2009-2017.state.gov/j/drl/rls/hrrpt/2010/sca/154480.htm",
              "https://2009-2017.state.gov/j/drl/rls/hrrpt/2011/sca/186463.htm",
              "https://2009-2017.state.gov/j/drl/rls/hrrpt/2012/sca/204399.htm",
              "https://2009-2017.state.gov/j/drl/rls/hrrpt/2013/sca/220392.htm")

years <- c(2005:2013)

inc_texts <- list()
for (i in 1: length(url_list)) {
  url_content <- read_html(url_list[i])
  text <- url_content %>% 
    html_elements("div#centerblock") %>% 
    html_text2() 
  
  inc_texts[[i]] <- list(text = text, year = years[i])
}

#Creating a dataframe from the list

inc_text_df <- bind_rows(inc_texts)

#--- Reading in files for BJP in power----

file_names <- paste(2014:2022, "Country Reports on Human Rights Practices.txt", sep=" ")

file_contents <- list()
for (name in file_names) {
  path_for_file <- file.path(path, "data", "human_rights_reports", name)
  file_contents[[name]] <- read_file(path_for_file)
}


#--- Converting individual files to a dataframe ----

bjp_text_df <- bind_rows(file_contents, row.names = NULL)
bjp_text_df <- bjp_text_df %>% 
  pivot_longer(cols = everything(),
               names_to = "title",
               values_to = "text") %>% 
  mutate(year = c(2014:2022)) %>% 
  select(!title)

text_df <- rbind(inc_text_df, bjp_text_df)

#Adding a column for identifying the political party in charge

text_df <- text_df %>% 
  mutate(party = ifelse(year >= 2014, "bjp", "inc")) %>% 
  select(year, party, text)


#--- Basic sentiment analysis using sentimentr ----

text_df_sentences <- unnest_tokens(text_df, sentence_tokens, text,
                                   token = "sentences")

sentiment_scores <- sentiment_by(text_df_sentences$sentence_tokens, 
                                 by = text_df_sentences$year)

sentiment_scores <- sentiment_scores %>% 
  mutate(upper_ci = ave_sentiment + 1.96*sd,
         lower_ci = ave_sentiment - 1.96*sd)

sentiment_plot <- sentiment_scores %>% 
  ggplot()+
  geom_point(aes(x=year, y = ave_sentiment), color = "darkred")+
  geom_errorbar(aes(x = year, ymin = lower_ci, ymax = upper_ci), width = 0)+
  scale_x_continuous(limits = c(2005, 2022), breaks = seq(2005, 2022, by = 1))+
  scale_y_continuous(limits = c(-1.5, 1.5))+
  labs(y = "Mean sentiment",
       title = "Average sentiment from US State Department Human Rights Reports",
       subtitle = "The BJP was elected in 2014 and 2019. No significant change in sentiment is observed")

sentiment_plot
ggsave(file.path(image_path, "state_dep_sentiment_plot.png"), sentiment_plot,
       width = 8, height = 6, dpi = 300)

#--- Sentiment analysis using sentimentr grouping by party ----

sentiment_scores_party <- sentiment_by(text_df_sentences$sentence_tokens, 
                                       by = text_df_sentences$party)

sentiment_scores_party <- sentiment_scores_party %>% 
  mutate(upper_ci = ave_sentiment + 1.96*sd,
         lower_ci = ave_sentiment - 1.96*sd)

sentiment_plot_party <- sentiment_scores_party %>% 
  ggplot()+
  geom_point(aes(x=party, y = ave_sentiment), color = "darkred")+
  geom_errorbar(aes(x = party, ymin = lower_ci, ymax = upper_ci), width = 0)+
  scale_y_continuous(limits = c(-2, 2))+
  labs(y = "Mean sentiment",
       x = "Party",
       title = "Average sentiment from US State Department Human Rights Reports",
       subtitle = "No significant change in sentiment is observed for both parties: INC from 2005-13, BJP from 2014-22")

sentiment_plot_party
ggsave(file.path(image_path, "state_dep_sentiment_plot_party.png"), 
       sentiment_plot_party,
       width = 8, height = 6, dpi = 300)

#--- Basic sentiment analysis using Bing ----

text_df_words <- unnest_tokens(text_df, word_tokens, text,
                               token = "words")
text_df_words <- text_df_words %>% 
  anti_join(stop_words, by = c("word_tokens" = "word"))

bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")

text_df_words <- text_df_words %>% 
  inner_join(bing, by = c("word_tokens" = "word")) %>% 
  inner_join(afinn, by = c("word_tokens" = "word"))

bing_plot <- text_df_words %>% 
  filter(!is.na(sentiment)) %>% 
  group_by(year, sentiment) %>% 
  summarize(count = n()) %>% 
  ggplot()+
  geom_col(aes(x = factor(year), y = count, fill = sentiment), position = "dodge")+
  labs(x = "Year", y = "Count", fill = "Sentiment",
       title = "BING Sentiment Analysis from US State Department Human Rights Reports")

bing_plot
ggsave(file.path(image_path, "state_dep_bing_plot.png"), bing_plot,
       width = 8, height = 6, dpi = 300)

#--- Basic sentiment analysis using AFINN ----
afinn_plot <- text_df_words %>% 
  filter(!is.na(value)) %>% 
  group_by(year) %>% 
  summarize(mean = mean(value)) %>% 
  ggplot()+
  geom_line(aes(x = year, y = mean))

afinn_plot

#--- Party sentiment analysis using Bing ----
bing_party_plot <- text_df_words %>% 
  filter(!is.na(sentiment)) %>% 
  group_by(party, sentiment) %>% 
  summarize(count = n()) %>% 
  ggplot()+
  geom_col(aes(x = factor(party), y = count, fill = sentiment), position = "dodge")+
  labs(x = "Party", y = "Count", fill = "Sentiment",
       title = "BING Sentiment Analysis from US State Department Human Rights Reports")

bing_party_plot


