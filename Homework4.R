library("tidyverse")
baby_names <- read_csv("http://personal.stevens.edu/~fmai/data/Most_Popular_Baby_Names.csv")
# select the  data for year 2013 and 2014
my_data <- baby_names %>% 
  filter(BRTH_YR == 2013 | BRTH_YR == 2014)
# a. The total number of UNIQUE names in the dataset.
length(unique(my_data$Name))
# 1423
# b. Assuming that the ethnicity is non-overlapping, 
# for each year calculate the total number of babies 
# born for each ethnicity in the dataset. 
born_number <- my_data %>%
  group_by(BRTH_YR, ETHCTY) %>%
  summarise_each(funs(sum),Count)
# c. For year 2013 and 2014 combined, what are the 
# top 2 most popular male and female baby names for each ethnicity?
top_2 <- my_data %>%
  group_by(ETHCTY, Gender, Name) %>%
  summarise_each(funs(sum),Count) %>%
  top_n(2)
top_2



  

  