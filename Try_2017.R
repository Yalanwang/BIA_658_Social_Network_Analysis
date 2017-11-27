library(readr)
library(igraph)
library(dplyr)
ym_2017 <- read_csv("F:/2017 fall third semester/BIA 658/Final Project/ym_latest.csv")
country <- read_csv("F:/2017 fall third semester/BIA 658/Final Project/country_eng.csv")


ym_2017 <- ym_2017[,-c(10:12)]  # get rid of hs4/6/9
ym_2017[is.na(ym_2017)] <- 0    # replace NA --> 0
head(ym_2017)



sum_value = function(data) {
  data <- data %>%
    group_by(Year,Country,exp_imp, Unit1, Unit2, hs2) %>% 
    summarize(Q2=sum(Q2), Q1=sum(Q1),Value=sum(as.numeric(Value))) %>%
    select(exp_imp, Year, Country,Unit1, Unit2,Q1, Q2,Value, hs2)
  return(data)
}


ym_2017_sum <- sum_value(ym_2017) 

ym_2017_data <- left_join(ym_2017_sum, country, by='Country') %>% filter(exp_imp == "1" & Area == "Asia") 

ym_2017_imp <- ym_2017_data[c(8, 9, 10)]

imp_data <- ym_2017_imp %>% 
                    group_by(Country_name,hs2)%>% 
                        summarize(Value=sum(as.numeric(Value)))

na_data <- imp_data %>% filter(Value == "NA")

imp_data[is.na(imp_data)] <- 0

mean(as.numeric(imp_data$Value))  ## 17780900

## analyze the country which above the average import

my_data <- imp_data[imp_data$Value >= 17780900,] 


names <- as.character(unique(my_data$Country_name))
#matrix_all <- matrix(0L, nrow = 86, ncol = 86)
matrix_all <- matrix(0L, nrow = length(names), ncol = length(names))
rownames(matrix_all) <- names
colnames(matrix_all) <- names

items <- unique(my_data$hs2)
for(i in 2:length(items)){
  my_data_groupby <- my_data[my_data$hs2 == items[i],]
  names_groupby <- as.character(unique(my_data_groupby$Country_name))
  matrix_groupby <- matrix(1L, nrow = length(names_groupby), ncol = length(names_groupby))
  rownames(matrix_groupby) <- names_groupby
  colnames(matrix_groupby) <- names_groupby
  matrix_all[names_groupby,names_groupby] = matrix_all[names_groupby,names_groupby] + matrix_groupby
  print(names_groupby)
}

g =  graph_from_adjacency_matrix(matrix_all,mode="undirected", weighted = TRUE)
g_1 <- simplify(g)



plot(g, vertex.size = 3,layout = layout.fruchterman.reingold(g))
plot(g_1, vertex.size = 3, layout = layout.fruchterman.reingold(g_1))



g2 = graph.adjacency(matrix_all, mode = "undirected",weighted = TRUE)
g2 <- simplify(g2)
plot(g2, vertex.size = 5, layout = layout.fruchterman.reingold(g2))





### try to plot more pretty figure
test.layout <- layout_(g,with_dh(weight.edge.lengths = edge_density(g)/100000))
plot(g, layout = test.layout)


### create several networks using the QAP to test the similarity od the network

















                      
