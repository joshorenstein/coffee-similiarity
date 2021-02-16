#install.packages('gsheet')
library(gsheet)
library(fastDummies)
library(tidyverse)
library(knitr)

#Grab my coffee data from google docs
coffee.data <- gsheet2tbl('docs.google.com/spreadsheets/d/1-SJa7tgXUdDIa3PlKONEkL44xQ8-RAhQK4v9Bp_-UUs/edit#gid=0')
# Admin stuff here, nothing special 
head(coffee.data)
#View(coffee.data)
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
cultivar.split
lev <- unique(unlist(cultivar.split))
cultivar.dummy <- lapply(cultivar.split, function(x) table(factor(x, levels=lev)))

lev <- unique(unlist(cultivar.split))
leva <- unlist(cultivar.split)
levb <- table(leva)
levc <- as.data.frame(levb) 
levd <- levc %>% filter(Freq>4)
levd

Notes.split <- strsplit(coffee.select$Notes, ", ")
lev2 <- unique(unlist(Notes.split))
lev3 <- unlist(Notes.split)
lev4 <- table(lev3)
lev5 <- as.data.frame(lev4) 
lev6 <- lev5 %>% filter(Freq>4) 
Notes.dummy <- lapply(Notes.split, function(x) table(factor(x, levels=lev2)))

#New DataSet
Data2 <- with(coffee.select, data.frame(Country,Process, Roast,do.call(rbind, cultivar.dummy),do.call(rbind,Notes.dummy)))

############################
#  Item Based Similarity   #
############################   
results <- fastDummies::dummy_cols(Data2)
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

# Output similarity results to a file
write.csv(results.similarity,file="final-coffee-similarity.csv")

# Get the top 3 neighbors for each
data.coffee.neighbors <- matrix(NA, nrow=ncol(results.similarity),ncol=11,dimnames=list(colnames(results.similarity)))

for(i in 1:ncol(results)) 
{
  data.coffee.neighbors[i,] <- (t(head(n=11,rownames(results.similarity[order(results.similarity[,i],decreasing=TRUE),][i]))))
}
(data.coffee.neighbors)

#View(data.coffee.neighbors)
data.coffee.neighbors<- cbind(Descriptor = rownames(data.coffee.neighbors), data.coffee.neighbors)
rownames(data.coffee.neighbors) <- 1:nrow(data.coffee.neighbors)
names(data.coffee.neighbors)
df <- as.data.frame(data.coffee.neighbors) %>% 
  select(-V2) %>% 
  rename(Sim_1=V3,Sim_2=V4,Sim_3=V5,Sim_4=V6,Sim_5=V7,Sim_6=V8,Sim_7=V9,Sim_8=V10,Sim_9=V11,Sim_10=V12)

#View(df)
#filter by country
names(df)

roast <- df %>% filter(grepl('Roast',Descriptor)) %>% arrange(Descriptor)
process <- df %>% filter(grepl('Process',Descriptor)) %>% arrange(Descriptor)
country <- df %>% filter(grepl('Country',Descriptor)) %>% arrange(Descriptor)

note <- df %>% 
                    filter(!grepl('Country',Descriptor)) %>% 
                    filter(!grepl('Process',Descriptor)) %>% 
                    filter(!grepl('Roast_',Descriptor)) %>% 
                    inner_join(lev6,by=c("Descriptor"="lev3")) %>% arrange(Descriptor) %>% 
                    select(-c(Freq))

varietal <- df %>% 
  filter(!grepl('Country',Descriptor)) %>% 
  filter(!grepl('Process',Descriptor)) %>% 
  filter(!grepl('Roast_',Descriptor)) %>% 
  inner_join(levd,by=c("Descriptor"="leva")) %>% 
  arrange(Descriptor) %>% 
  select(-c(Freq))

country %>% write_csv("by-country.csv")
roast %>% write_csv("by-roast.csv")
process %>% write_csv("by-process.csv")
note %>% write_csv("by-flavor-note.csv")
varietal %>% write_csv("by-varietal.csv")
#View(varietal)
#View(country)
#View(note)
