rm(list=ls())
library(data.table)
library(igraph)

options(warn=-1)

#Set the directory path to the path where file is located
path ="C:/Users/AvanthiNamburi/Desktop/Books_MSBAPM/R/Project/Datasets"
setwd(path)

#give  the file name
filename = "expedia_training_final_sample.csv"

#performing the read operation
final_sample = fread(filename)

#making the subset of the records with booking history, leaving the records with searach history
expedia_train_sample_booked =final_sample[final_sample$is_booking==1,]
View(expedia_train_sample_booked)
nrow(expedia_train_sample_booked)


# Basic Social_Network_Analysis for the input data 

  input_data_set = expedia_train_sample_booked
  df <- data.frame(paste(input_data_set$user_id,input_data_set$hotel_country,sep = "_"),input_data_set$hotel_cluster)
  
  #change the column names to user_id_country,hotel_Cluster
  colnames(df) = c("user_id_country","hotel_cluster")
  
  #adding a prefix HCl_ such that the graph does not treat the value as continuous
  df$hotel_cluster <- sub("^", "HCl_",df$hotel_cluster)
  
  #adding a prefix U_ such that the graph does not treat the value as continuous
  df$user_id_country <- sub("^", "U_",df$user_id_country)
  
  #converting the columns of dataframe as vectors - This is becuase of easeness to make the social network graph
  hotel_cluster = as.vector(df$hotel_cluster)
  user_id_country = as.vector(df$user_id_country)
  
  #creating a social network graph between columns user_id_country,hotel_Cluster to see the association
  Hgraph = graph_from_data_frame(df, directed = FALSE)
  
 
  #To display the Eigen Vectors of the Network(Users grouped through country across clusters)
  EV <- evcent(Hgraph)
  sort(unlist(EV), decreasing=TRUE) 
  x=data.frame(head(EV$vector,20))
  colnames(x)=c("Eigen Vector Centrality")
  z = data.frame( head(rownames(as.data.frame(EV)),20), x$`Eigen Vector Centrality`)
  colnames(z) = c("USER_COUNTRY","Eigen Vector Centrality")
  head(z,10)
  
  
  ##To display the Top most Clusters based on the Degree (User_Country_Clusters)
  Degree <-degree(Hgraph,mode="total")
  sort(unlist(Degree), decreasing=TRUE) 
  aa=head(as.data.frame(Degree),20)
  z = data.frame(head(rownames(as.data.frame(aa)),20), aa$Degree)
  colnames(z) = c("Popular People","Degree")
  head(z,10)
  
  #This is to represent network graph 
  plot(Hgraph, vertex.label = V(Hgraph)$name)   # This will take some time and the final plot is not interpretable since the data is very large




#############User Cluster Collaborative Filtering#############################

##################################################################################

                                        #Step 1

##################################################################################


#Function to get the top N clusters based on the user logged
#output will be the Top N clusters from the users history for that location.

User_Top_N_Clusters = function(user,country,n)
{
  # extracting the columns of interest to make the network graph.(user,country and clusters)
  # here combining the user and country with a seperater "_" to a single column and then in the second column,corresponding hotel cluster
  df <- data.frame(paste(expedia_train_sample_booked$user_id,expedia_train_sample_booked$hotel_country,sep = "_"),expedia_train_sample_booked$hotel_cluster)
 
  #change the column names to user_id_country,hotel_Cluster
  colnames(df) = c("user_id_country","hotel_cluster")
  
  #adding a prefix HCl_ such that the graph does not treat the value as continuous
  df$hotel_cluster <- sub("^", "HCl_",df$hotel_cluster)
  
  #adding a prefix U_ such that the graph does not treat the value as continuous
  df$user_id_country <- sub("^", "U_",df$user_id_country)
  
  #creating a social network graph between columns user_id_country,hotel_Cluster 
  # to see the association
  Hgraph = graph_from_data_frame(df, directed = TRUE, vertices = NULL)
 
  #to find total nodes in the graph
  vgraph = as_data_frame(Hgraph, what="vertices")
  
  #to find the associations(or edges) in the graph
  egraph = as_data_frame(Hgraph, what="edges")
  
  #Input string receved by the function
  x= paste("U",user,country,sep = "_")
  
  #converting the value received to string
  y=toString(x)
  
  #obtaining records of associations that match the input string, i.e user_with_given_country and the corresponding cluster list 
  y = data.frame(table(egraph[egraph$from == y,])) ### Which gives the connections of particular col1 with clusters
  
  #order the matching records in the decreasing order 
  Z= y[order(-y$Freq),,drop=FALSE] ## Max connections comes first
  
  #display the  top n records 'n' specified by the business requirement
  head(Z,n) ##TOP n connections
  
  #writing the results to a data frame
  r = data.frame(head(Z,n))
  
  #returning the results in the form of a data frame
  return(r)
}





##################################################################################

                                    # Step 2

##################################################################################

#Function to get the top users of the clusters obtained from step 1
#output will be the Top N users who used the Top N clusters obtained from step 1.

Clusters_Versus_Top_Users = function(User_CLuster_List,n)
{
  # extracting the columns of interest to make the network graph.hotel_cluster,user_id.here the order is changed since we are interested in associations of cluster to user
  df <- data.frame(expedia_train_sample_booked$hotel_cluster,expedia_train_sample_booked$user_id)
  
  #change the column names to hotel_Cluster,user_id
  colnames(df) = c("hotel_cluster","user_id")
  
  #adding a prefix U_ such that the graph does not treat the value as continuous
  df$user_id <- sub("^", "U_",df$user_id)
  
  #adding a prefix HCl_ such that the graph does not treat the value as continuous
  df$hotel_cluster <- sub("^", "HCl_",df$hotel_cluster)
  
  #creating a social network graph between columns user_id_country,hotel_Cluster to get the association
  Hgraph = graph_from_data_frame(df, directed = TRUE, vertices = NULL)
  
  #to find total nodes in the graph
  vgraph = as_data_frame(Hgraph, what="vertices")
  
  #to find the associations(or edges) in the graph
  egraph = as_data_frame(Hgraph, what="edges")
  
  
  #Input list of clusters received by the function
  y = User_CLuster_List
  
  #obtaining records of associations that match the input string, i.e users list for the corresponding cluster 
  y = data.frame(table(egraph[egraph$from == y$to,])) ### Which gives the connections of particular col1 with clusters
  
  #order the matching records in the decreasing order 
  Z= y[order(-y$Freq),,drop=FALSE] ## Max connections comes first
  
  #display the  top n records 'n' specified by the business requirement
  head(Z,n) ##TOP n connections
  
  #writing the results to a data frame
  r = data.frame(head(Z,n))
  
  #returning the results in the form of a data frame
  return(r)
}  




##################################################################################

                                          # Step 3

##################################################################################
#Function to get the top clusters of the users obtained from step 2
#output will be the Top N clusters to reccommend on the search page

Top_User_Cluster_User_Cluster = function(Top_Cluster_Top_Users,n)
{
  n = length(Top_Cluster_Top_Users)
  # extracting the columns of interest to make the network graph.(user_id and clusters)
  df <- data.frame(expedia_train_sample_booked$user_id,expedia_train_sample_booked$hotel_cluster)
  
  #change the column names to user_id_country,hotel_Cluster
  colnames(df) = c("user_id","hotel_cluster")
  
  #adding a prefix U_ such that the graph does not treat the value as continuous
  df$user_id <- sub("^", "U_",df$user_id)
  
  #adding a prefix HCl_ such that the graph does not treat the value as continuous
  df$hotel_cluster <- sub("^", "HCl_",df$hotel_cluster)
  
  #creating a social network graph between columns user_id_country,hotel_Cluster to get the association
  Hgraph = graph_from_data_frame(df, directed = TRUE, vertices = NULL)
  
  #to find total nodes in the graph
  vgraph = as_data_frame(Hgraph, what="vertices")
  
  #to find the associations(or edges) in the graph
  egraph = as_data_frame(Hgraph, what="edges")
  
  #Input list of users received by the function
  y = Top_Cluster_Top_Users
  
  #obtaining records of associations that match the input string, i.e users list for the corresponding cluster 
  y = data.frame(table(egraph[egraph$from == y$to,])) ### Which gives the connections of particular col1 with clusters
  
  #order the matching records in the decreasing order
  Z= y[order(-y$Freq),,drop=FALSE] ## Max connections comes first
  
  #display the  top n records 'n' specified by the business requirement
  head(Z,n) ##TOP n connections
  
  #writing the results to a data frame
  r = data.frame(head(Z,n))
  
  #returning the results in the form of a data frame
  return(r)
}  



SN_Collaborative_Filtering = function(user,country,n)
{
  #Top N Clusters from the users previous history(step1)
  R_Step1 = User_Top_N_Clusters(user,country,n)
  #converting the results to a vector, this list will be passed to next step.
  User_CLuster_List = R_Step1
  
  #step 2 Top Users of the clusters obtained from Top Clusters received from step 1
  R_Step2 = Clusters_Versus_Top_Users(User_CLuster_List,25)
  R_Step2_data = as.data.frame(Clusters_Versus_Top_Users(User_CLuster_List,25))
  
  #obtaining the list fo users from the output.This list will be passed to step 3.
  Top_Cluster_Top_Users = R_Step2_data
  
  #step 3 Top Clusters of the top users obtained from step 2
  R_Step3 = Top_User_Cluster_User_Cluster(Top_Cluster_Top_Users,length(Top_Cluster_Top_Users))
  Top_User_Cluster_User_Cluster_List = R_Step3
  
  if(nrow(Top_User_Cluster_User_Cluster_List)==0 && nrow(User_CLuster_List)==0)
  {
      print("No relevent search results")
    
  }
      
  else if(nrow(User_CLuster_List)!=0 && nrow(Top_User_Cluster_User_Cluster_List)==0)
      {
        head(unique(User_CLuster_List$to),n)
      }
    
  else 
  {
    Top_User_Cluster_User_Cluster_List = rbind(User_CLuster_List,Top_User_Cluster_User_Cluster_List)
    head(unique(Top_User_Cluster_User_Cluster_List$to),n)
  }
}

SN_Collaborative_Filtering(user=181651,country=8,n=10)

# resulting clusters for user=  181651,country=8,n=10 are : HCl_29 HCl_36 HCl_61 HCl_82 HCl_99 HCl_46 HCl_4 

# resulting clusters for user = 513782,country=50,n=10 are : HCl_7  HCl_21 HCl_42 HCl_28 HCl_91 HCl_59 HCl_2  HCl_48 HCl_13 HCl_15

# resulting clusters for user = 513782,country=125(nonexisting countryID),n=10 are : "No relevent search results"

# resulting clusters for user = 513782,country=125(nonexisting countryID),n=10 are : "No relevent search results"


system.time(SN_Collaborative_Filtering(user=181651,country=196,n=10))


