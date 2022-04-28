library(tidyverse) # metapackage with lots of helpful functions
library(reshape2)
library(ggrepel)
library(dplyr)
library(forcats)
library(ggplot2)
library(stringr)
library(tidyr)

theme_custom_sk_90 <- theme_bw() +  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1, vjust = 0.5), axis.text.y = element_text(size = 18),
                                          axis.title = element_text(size = 20),strip.text  = element_text(size = 14))

# Change plot size to 8 x 3
options(repr.plot.width=12, repr.plot.height=8)

df_netflix <- read.csv("C:/Users/logon/Downloads/Netflix/netflix_titles.csv")
df_netflix$date_added <- as.Date(df_netflix$date_added, format = "%B %d, %Y")
head(df_netflix)
summary(df_netflix)
typeof(df_netflix['rating'])
barplot(unlist(df_netflix['rating']))

# Number of Rows in the Input

unique_counts <- apply(df_netflix, MARGIN = 2, FUN = function(x) length(unique(x)))
unique_counts <- data.frame(Columns = names(unique_counts), UniqueCounts = unique_counts, stringsAsFactors = F)
unique_counts %>% ggplot(aes(x = Columns, y = UniqueCounts)) + 
  geom_bar(stat = 'identity') + 
  scale_x_discrete(limits = colnames(df_netflix)) + 
  geom_hline(yintercept = nrow(df_netflix)) + 
  geom_label(aes(x = 6, y = nrow(df_netflix), label = 'Number of rows in the input'), size = 8) +
  theme_custom_sk_90


# Shows in Netflix
df_by_date <- df_netflix %>% group_by(date_added,type) %>% summarise(addedToday = n()) %>% 
  ungroup() %>% group_by(type) %>% mutate(Total_Number_of_Shows = cumsum(addedToday), label = if_else(date_added == max(date_added,na.rm = T), as.character(type), NA_character_))


df_by_date  %>% 
  ggplot(aes(x = date_added, y = Total_Number_of_Shows, color = type)) + geom_line(size = 2) + 
  theme_bw(base_size = 20) + 
  scale_x_date(date_breaks = '2 years', date_labels = "%Y") + 
  theme(legend.position = 'none') +
  geom_text_repel(aes(label = label), size = 8,na.rm = TRUE, nudge_y = 100)


# Countries

df_netflix %>% group_by(type) %>% mutate(country = fct_infreq(country)) %>% ggplot(aes(x = country)) + 
  geom_histogram(stat = 'count') + facet_wrap(~type, scales = 'free_x') + 
  theme_custom_sk_90 + coord_cartesian(xlim = c(1,10)) + scale_x_discrete(labels = function(x){str_wrap(x,20)}, breaks = function(x) {x[1:10]})


# plto
df_netflix %>% select(c('show_id','cast','director')) %>% 
  gather(key = 'role', value = 'person', cast, director) %>% 
  filter(person != "") %>% separate_rows(person, sep = ',') -> df_show_people

df_show_people$person <- trimws(df_show_people$person)
head(df_show_people)
df_people_freq<- df_show_people %>% group_by(person,role) %>% 
  summarise(count = n()) %>% arrange(desc(count))

df_people_freq %>% group_by(role) %>% top_n(10,count) %>% ungroup() %>% ggplot(aes(x = fct_reorder(person,count,.desc = T), y = count, fill = role)) + 
  geom_bar(stat = 'identity') + scale_x_discrete() + facet_wrap(~role, scales = 'free_x') + 
  theme_custom_sk_90 + theme(legend.position = 'none') + labs(x = 'Name of the actor / director')



