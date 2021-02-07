#install.packages('gsheet')
library(gsheet)
library(fastDummies)
library(tidyverse)
library(knitr)

#Grab my coffee data from google docs
coffee.data <- gsheet2tbl('docs.google.com/spreadsheets/d/1-SJa7tgXUdDIa3PlKONEkL44xQ8-RAhQK4v9Bp_-UUs/edit#gid=0')
# Admin stuff here, nothing special 
options(digits=2)

#Clean some columns 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
coffee.data$Cultivar <- trim(coffee.data$Cultivar)
coffee.data$Roast <- trim(coffee.data$Roast)
coffee.data$Process <- trim(coffee.data$Process)
coffee.data$Cultivar <- tolower(coffee.data$Cultivar)
coffee.data$Notes <- trim(coffee.data$Notes)
coffee.data$Notes <- tolower(coffee.data$Notes)

#keep relevant columns
coffee.select <- coffee.data %>%  select(Country,Process,Cultivar,Roast,Notes)
head(coffee.select)
#make dummy variables from comma separated columns
cultivar.split <- strsplit(coffee.select$Cultivar, ", ")
lev <- unique(unlist(cultivar.split))
cultivar.dummy <- lapply(cultivar.split, function(x) table(factor(x, levels=lev)))

Notes.split <- strsplit(coffee.select$Notes, ", ")
lev2 <- unique(unlist(Notes.split))
Notes.dummy <- lapply(Notes.split, function(x) table(factor(x, levels=lev2)))

#New DataSet
Data2 <- with(coffee.select, data.frame(Country,Process, Roast,do.call(rbind, cultivar.dummy),do.call(rbind,Notes.dummy)))
View(Data2)
############################
#  Item Based Similarity   #
############################   
results <- fastDummies::dummy_cols(Data2,remove_first_dummy = TRUE)
results <- results %>% select(-c(Country:Roast))

results[is.na(results)] <- 0 
# Create a helper function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# Create a placeholder dataframe listing item vs. item
holder <- matrix(NA, nrow=ncol(results),ncol=ncol(results),dimnames=list(colnames(results),colnames(results)))
results.similarity <- as.data.frame(holder)

# Lets fill in those empty spaces with cosine similarities
for(i in 1:ncol(results)) {
  for(j in 1:ncol(results)) {
    results.similarity[i,j]= getCosine(results[i],results[j])
  }
}
View(results.similarity)
# Output similarity results to a file
write.csv(results.similarity,file="final-coffee-similarity.csv")

# Get the top 3 neighbors for each
data.coffee.neighbors <- matrix(NA, nrow=ncol(results.similarity),ncol=4,dimnames=list(colnames(results.similarity)))

for(i in 1:ncol(results)) 
{
  data.coffee.neighbors[i,] <- (t(head(n=4,rownames(results.similarity[order(results.similarity[,i],decreasing=TRUE),][i]))))
}
(data.coffee.neighbors)

#View(data.coffee.neighbors)
data.coffee.neighbors<- cbind(Descriptor = rownames(data.coffee.neighbors), data.coffee.neighbors)
rownames(data.coffee.neighbors) <- 1:nrow(data.coffee.neighbors)
names(data.coffee.neighbors)
df <- as.data.frame(data.coffee.neighbors) %>% 
  select(-V2) %>% 
  rename(Sim_1=V3,Sim_2=V4,Sim_3=V5)

df 
#filter by country
df %>% filter(grepl('Country',Descriptor)) %>% view()
#View(data.coffee.neighbors)
# Output neighbor results to a file  
df %>% write_csv("final-coffee-item-neighbors.csv")