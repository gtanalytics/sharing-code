# sharing-code
---
title: "Graph Your Mailbox Network"
author: "GjT"
date: "Wednesday, February 04, 2015"
output:
  html_document:
    highlight: textmate
    theme: cerulean
---

It can be very interesting to see with whom you mainly interact, for a project or in general. 

The library `networkD3`, previously `d3Network`, the wrap up of d3.js into R by [christopher gandrud](http://christophergandrud.github.io/networkD3/) can be very helpful.

This is a simple tuto using d3.js and R to plot your network interaction based on my email-box

### Useful libraries
```{r,eval = T,warning=FALSE}
#install.packages("networkD3") # For graphing network
library(networkD3)
options(stringsAsFactors = FALSE)
library(RCurl) # Http protocol, doesn't work at AXA because of proxy
library(reshape) # manipulate data
library(dplyr) # The famous library for data munging
```

### Data Preparation


Read all the datasets from data directory
```{r}
# Get the datasets
data_input = "./data/"
files <- list.files(path =data_input,pattern = "graph",full.names=F)
# Read files
for (i in seq_along(files)) {
  assign(gsub("[.]csv$","",files[i]), read.csv(file.path(data_input,files[i]), 
                                               header =T, sep =",",nrows =-1))
}
```

Clean and organize


This step can be longer according to your data; If your data are dirty, you should spend a lot of time to make it clear and usable


```{r}
l_dataframe = list(graph1.CSV,graph2.CSV,graph3.CSV) 

# A wrap up function to set the colnames of data.frame
nameit = function(df){ 
  names(df) = c("Source","Target")
  return(df)
}

#  Applying function
l_dataframe = lapply(l_dataframe, nameit)

# Split columns in order to get all the message for one sender and 
# melt data : in order to get for each people, all the messages sent
library(splitstackshape)
melting= function(df){
  tmp_split = cSplit(df, "Target", ";", drop = TRUE)
  tmp = na.omit(melt(tmp_split, id = "Source"))
  tmp = select(tmp,Source,value)%>%rename(Target=value)
  tmp$Target = as.character(tmp$Target)
  return(tmp)
}


l_dataframe = lapply(l_dataframe, melting)

# Merging alldataset
df_network <- Reduce(rbind, l_dataframe) %>% as.data.frame()%>%filter(substr(Source,1,4)!="AGPC")
# ----- 

# Extra cleaning
df_network$charsource = sapply(strsplit(df_network$Source, " "), length)
df_network$chartarget=sapply(strsplit(df_network$Target, " "), length)
df_network$hasarobase = grepl("@",df_network$Source) | grepl("@",df_network$Target) 
df_network$hasspecific = grepl("'",df_network$Source) | grepl("'",df_network$Target) 
# Filter dataset to keep only relevant infos
df_network = df_network %>% filter(hasarobase==FALSE) %>%
  filter(charsource==2 | charsource==3) %>% filter(hasspecific==FALSE)
```

After cleaning, the data look like 

```{r}
head(select(df_network, Source, Target),8)
tail(select(df_network, Source, Target),8)
```

### First Vizualisation

The vizu is very simple after cleaning the data, we just have to call the function ```simplenetwork`` like : 

```{r}
simpleNetwork(df_network, width = 800, height = 600,
                textColour = "skyblue2", linkColour = "pink",
                opacity = 0.8, linkDistance = 200)
```

The options textColour allow to click on one node to display the name.

Per default, you can disable and if you don't like the pink link, you can modify the options : 
```{r}
simpleNetwork(df_network, width = 800, height = 600,fontSize = 12,
              linkColour = "darkgrey",opacity = 0.8, linkDistance = 150)
```


### Second Vizualisation

Sometimes, Interaction depends on group, and It can be very interesting if we can see different group and the way that they interact together. 

We can make some statistics, to compute the number of times than each people have an exchange.

```{r}
# Compute cross tabulation between source and target
links = as.data.frame(table(df_network$Source,df_network$Target)) %>% filter(Freq!=0)
names(links) = c("source","target","value")
links = arrange(links, source)
head(links)
```
As ```networkd3``` needs figure, we have to set up the variable to be considered as the figures. 

It can be done like that : 

```{r}

tmp = data.frame(source2 = c(links$source)) # Create the factor for each variable
head(tmp)
links = cbind(links, tmp)
# Nodes : All the value available
nodes = data.frame(name = c(as.character(links$source),as.character(links$target))) %>%
  arrange(name)
nodes = unique(nodes)
head(nodes)
links$target2 = sapply(as.character(links$target), 
                       function(x) {
                         i <- which(x == nodes$name)
                         if (length(i)) i else NA}) -1

head(links)

```
For the group, do it yourself according to what you want to display. Group per project/team/etc, For demo : random group
```{r}
nodes$group = sample(x = c(1,2,3,4,5),90,replace = T)
```

And the vizualisation

```{r}
forceNetwork(Links = links, Nodes = nodes,
             Source = "source2", Target = "target2",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)
```

That's it !

GjT,

[http://sciencendata.wordpress.com](sciencendata.wordpress.com)

