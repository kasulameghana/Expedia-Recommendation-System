library(igraph)
library(data.table)
#Set the directory path to the path where file is located
path ="C:/Users/akula/Desktop/uconn/SPRING/R/project/Expedia"
setwd(path)

#give the file name
filename = "expedia_training_final_sample.csv"

#performing the read operation
final_sample = fread(filename)

########### ONLY BOOKING DATA ##############
expedia_train_sample_booked =final_sample[final_sample$is_booking==1,]

####### DEFAULT VALUES AS PER THE SITE #############
expedia_train_sample_booked$srch_adults_cnt = ifelse(expedia_train_sample_booked$srch_adults_cnt ==0,1,expedia_train_sample_booked$srch_adults_cnt)
expedia_train_sample_booked$srch_rm_cnt = ifelse(expedia_train_sample_booked$srch_rm_cnt ==0,1,expedia_train_sample_booked$srch_rm_cnt)


####### The below function takes in country & Number of Top clusters needed to be genereated ##################################
Top_N_HClusters_from_Country = function(Hct,n) 
{
  df <- data.frame(expedia_train_sample_booked$hotel_country,expedia_train_sample_booked$hotel_cluster)
  
  df$expedia_train_sample_booked.hotel_cluster <- sub("^", "HCl_",df$expedia_train_sample_booked.hotel_cluster)
  
  length(unique(df$expedia_train_sample_booked.hotel_country))
  
  Hgraph = graph_from_data_frame(df, directed = TRUE, vertices = NULL)
  vgraph = as_data_frame(Hgraph, what="vertices")
  egraph = as_data_frame(Hgraph, what="edges")
  
  y = data.frame(table(egraph[egraph$from ==Hct,])) ###Which gives the connections of particular country with clusters
  Z= y[order(-y$Freq),,drop=FALSE] ## Clusters are ordered based on Max number of booking
  top_clusters = head(Z,n) ##TOP n connections
  return(data.frame(top_clusters$to)) #######Returns Top N clusters
}

###### The below function takes in country, Rooms,Adult ,Children & Number of Top clusters needed to be genereated ##################################

Top_N_HClusters_Search_No_Date = function(Hcountry,Hroom,Hadults,Hchildren,Hn)
{
  Hroom =  ifelse(is.na(Hroom) == TRUE,1,Hroom) 
  
  Hadults = ifelse(is.na(Hadults) == TRUE,1,Hadults)
  
  Hchildren = ifelse(is.na(Hchildren) == TRUE,0,Hchildren) 
  
  x= paste(Hcountry,Hroom,Hadults,Hchildren,sep = "_")
  s=toString(x)
  df <- data.frame(paste(expedia_train_sample_booked$hotel_country,expedia_train_sample_booked$srch_rm_cnt,expedia_train_sample_booked$srch_adults_cnt,expedia_train_sample_booked$srch_children_cnt,sep = "_"),expedia_train_sample_booked$hotel_cluster)
  
  colnames(df) = c("col1","hotel_cluster")
  df$hotel_cluster <- sub("^", "HCl_",df$hotel_cluster)
  
  Hgraph = graph_from_data_frame(df, directed = TRUE, vertices = NULL)
  vgraph = as_data_frame(Hgraph, what="vertices")
  egraph = as_data_frame(Hgraph, what="edges")
  
  y = data.frame(table(egraph[egraph$from == s,])) ### Which gives the connections of particular col1 with clusters
  Z= y[order(-y$Freq),,drop=FALSE] ## Clusters are ordered based on Max number of booking
  top_clusters = head(Z,Hn) ##TOP n connections
  return(data.frame(top_clusters$to))
}

##### The below function takes in country, Checkin, checkout dates & Number of Top clusters needed to be genereated ##################################

Top_N_HClusters_Search_only_Dates = function(Hcountry,Hcheckin,Hcheckout,Hn)
{
  Hcheckin = as.Date(Hcheckin)
  Hcheckout= as.Date(Hcheckout) 
  dif = as.numeric(abs(Hcheckin-Hcheckout))
  x= paste(Hcountry,dif,sep = "_")
  s=toString(x)
  df <- data.frame(paste(expedia_train_sample_booked$hotel_country,(as.Date(expedia_train_sample_booked$srch_co)-as.Date(expedia_train_sample_booked$srch_ci)),sep = "_"),expedia_train_sample_booked$hotel_cluster)
  
  colnames(df) = c("col1","hotel_cluster")
  df$hotel_cluster <- sub("^", "HCl_",df$hotel_cluster)
  
  Hgraph = graph_from_data_frame(df, directed = TRUE, vertices = NULL)
  vgraph = as_data_frame(Hgraph, what="vertices")
  egraph = as_data_frame(Hgraph, what="edges")
  
  y = data.frame(table(egraph[egraph$from == s,])) ### Which gives the connections of particular col1 with clusters
  Z= y[order(-y$Freq),,drop=FALSE] ## Clusters are ordered based on Max number of booking
  top_clusters = head(Z,Hn) ##TOP n connections
  return(data.frame(top_clusters$to))
}

##### The below function takes in country,Checkin,checkout dates, Rooms,Adult ,Children & Number of Top clusters needed to be genereated ##################################

Top_N_HClusters_Search_All_Inputs = function(Hcountry,Hcheckin,Hcheckout,Hroom,Hadults,Hchildren,Hn)
{
  
  Hcheckin = as.Date(Hcheckin)
  
  Hcheckout = as.Date(Hcheckout)
  
  
  Hroom =  ifelse(is.na(Hroom) == TRUE,1,Hroom) 
  
  Hadults = ifelse(is.na(Hadults) == TRUE,1,Hadults)
  
  Hchildren = ifelse(is.na(Hchildren) == TRUE,0,Hchildren) 
  
  
  dif= as.numeric(abs(Hcheckin - Hcheckout))
  
  
  x= paste(Hcountry,dif,Hroom,Hadults,Hchildren,sep = "_")
  s=toString(x)
  
  
  df <- data.frame(paste(expedia_train_sample_booked$hotel_country,(as.Date(expedia_train_sample_booked$srch_co) - as.Date(expedia_train_sample_booked$srch_ci)),expedia_train_sample_booked$srch_rm_cnt,expedia_train_sample_booked$srch_adults_cnt,expedia_train_sample_booked$srch_children_cnt,sep = "_"),expedia_train_sample_booked$hotel_cluster)
  colnames(df) = c("col1","hotel_cluster")
  df$hotel_cluster <- sub("^", "HCl_",df$hotel_cluster)
  
  Hgraph = graph_from_data_frame(df, directed = TRUE, vertices = NULL)
  vgraph = as_data_frame(Hgraph, what="vertices")
  egraph = as_data_frame(Hgraph, what="edges")
  
  y = data.frame(table(egraph[egraph$from == s,])) ### Which gives the connections of particular col1 with clusters
  Z= y[order(-y$Freq),,drop=FALSE] ## Clusters are ordered based on Max number of booking
  top_clusters = head(Z,Hn) ##TOP n connections
  return(data.frame(top_clusters$to)) ##TOP n connections
}

####Function which takes in User Inputs and returns required top clusters as per the search###########

SNA_Item_based_filter= function(country,checkin,checkout,room,adults,children,n)
{
  
  if (is.na(checkin) == TRUE & is.na(checkout) == TRUE & is.na(room)==TRUE & is.na(adults) ==TRUE & is.na(children) ==TRUE)
  {
    Top_N_HClusters_from_Country(Hct = country,n=n)
  }
  else if(is.na(checkin) == FALSE & is.na(checkout) == TRUE & is.na(room)==TRUE & is.na(adults) ==TRUE & is.na(children) ==TRUE)
  {
    checkout =as.Date(checkin)+1
    Top_N_HClusters_Search_only_Dates(country,checkin,checkout,n)
  }
  else if (is.na(checkin) == FALSE & is.na(checkout) == FALSE & is.na(room)==TRUE & is.na(adults) ==TRUE & is.na(children) ==TRUE)
  {
    Top_N_HClusters_Search_only_Dates(country,checkin,checkout,n)
  }
  
  else if(is.na(checkin) == TRUE & is.na(checkout) == TRUE) 
  {
    Top_N_HClusters_Search_No_Date(country ,room ,adults ,children ,n)
  }
  else
  {
    if(is.na(checkin) == FALSE & is.na(checkout) == TRUE ){checkout =as.Date(checkin)+1}
    
    Top_N_HClusters_Search_All_Inputs(country,checkin,checkout,room,adults,children,n )
  }
}

SNA_Item_based_filter_generic= function (country,checkin,checkout,room,adults,children,n)
{
  AA=SNA_Item_based_filter(country,checkin,checkout,room,adults,children,n)
  BB=c()
  if (nrow(AA)<n) ###If the output is less than the required
  {
    BB= Top_N_HClusters_from_Country(Hct =country,n=n) ### Top clusters pertaining to the country will be considered
  }
  head(unique(rbind.data.frame(AA,BB)),n) ####Unique list of both the searches will be displayed
}


###############Calling Function ##############
SNA_Item_based_filter_generic(country = 50,checkin = "2017-04-22",checkout = "2017-04-27",room =1,adults = 4,children = NA,n=5)

######## Input format for the Date "YEAR(4 DIGIT) - MONTH(2 DIGIT)- DATE(2 DIGIT)" ; IF NO INPUT NA ;COUNTRY ID should be from the Data###########
######top_clusters.to
#1          HCl_17
#2          HCl_26
#3          HCl_41
#4          HCl_48
#5          HCl_94

SNA_Item_based_filter_generic(country = 50,checkin = NA,checkout = NA,room =1,adults = 4,children = NA,n=5)

#  top_clusters.to
#1          HCl_48
#2          HCl_91
#3          HCl_42
#4          HCl_98
#5          HCl_41

SNA_Item_based_filter_generic(country = 99,checkin = NA,checkout = NA,room =1,adults = 2,children = NA,n=5)
#  top_clusters.to
#1          HCl_81
#2          HCl_82
#3          HCl_46
#4          HCl_36
#5          HCl_67