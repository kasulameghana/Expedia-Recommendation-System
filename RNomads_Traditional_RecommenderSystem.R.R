#######################################################################
#### This file contains the traditional approach to recommender systems
#### Team RNomads
#######################################################################

######### Installing Libraries ##############

#install.packages("data.table")

library(data.table)
#######################################################################
# clearing memory
rm(list = ls())

# setting working directory. to be changed as per system location of data file

setwd("E:/UConn/Data Analytics with R/Project/Expedia/")
#######################################################################
# importing data for around 10000 random users who have booked in the last 2 years by taking is_booking as 1

#expedia_training_booked = fread("top51usersdata.csv")
expedia_training_booked = fread("expedia_training_final_sample.csv")
# taking data for only bookings
expedia_training_booked = expedia_training_booked[expedia_training_booked$is_booking == 1,]
#View(expedia_training_booked)

#######################################################################
######### Creating a content based recommendation function ############
############ INPUT - The search destination id
############ OUTPUT - The clusters for this destination and their top 10 neighbours i.e similar clusters
#######################################################################
conventional_contentBasedFiltering = function(dest_id)
{
  #Filtering data based on the search destination
  expedia_training_booked = expedia_training_booked[,-1]
  #View(expedia_training_booked)
  x1 = expedia_training_booked[expedia_training_booked$srch_destination_id == dest_id,]
  
  if(nrow(x1)==0){# check if there is any data for the input destination id
    destination_clusters = c("Please enter a valid value for search destination id")
  } else {
    
    # save it into a file to obtain required formatting
    write.csv(x1,"testff.csv")
    # import the data for further inspection
    new_query_output = read.csv("testff.csv")
    #View(new_query_output)
    #new_query_output = new_query_output[,-1]
    
    # read that data and remove it the serial number column
    final_data1 = data.frame(unclass(table(new_query_output$user_id,new_query_output$hotel_cluster)))
    #View(final_data)
    #remove the X string from the column name to retain just the cluster number info
    
    ### here we have to standardize the data as we have on count not the rating
    
    final_data = final_data1 / (1 + final_data1)
    
    a = gsub("X", "", names(final_data), fixed = TRUE)
    
    # now assign that to the column name for better readibility of code
    
    colnames(final_data) = a
    #(new_query_output$hotel_cluster)
    
    write.csv(final_data,"user_cluster_mat.csv")
    # save the matrix
    
    # function for calculating cosine distances between clusters to find similar clusters
    
    getCosine <- function(x,y) 
    {
      this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
      return(this.cosine)
    }
    
    # now we create a similarity matrix between the clusters
    
    expedia_cluster_similarity  <- matrix(0,nrow = ncol(final_data),ncol = ncol(final_data))
    
    #View(expedia_cluster_similarity)
    
    colnames(expedia_cluster_similarity) = a
    #colnames(expedia_cluster_similarity) to keep matrices in sync
    
    # looping to populate the matrix
    for(i in 1:ncol(final_data)) {
      # Loop through the columns for each column
      for(j in 1:ncol(final_data)) {
        # Fill in placeholder with cosine similarities
        expedia_cluster_similarity[i,j] <- getCosine(as.matrix(final_data[i]),as.matrix(final_data[j]))
      }
    }
    # Back to dataframe
    expedia_cluster_similarity <- as.data.frame(expedia_cluster_similarity)
    
    # here we have the similarity matrix of the clusters
    
    #write.csv(expedia_cluster_similarity,"expedia_cluster_similarity.csv")
    
    #View(expedia_cluster_similarity)
    
    # Get the top 10 neighbours for each cluster
    # if clusters are less than 10, we will take num of clusters
    num = ifelse(ncol(final_data)>11,11,ncol(final_data)) 
    expedia_cluster_neighbours <- matrix(NA, nrow=ncol(expedia_cluster_similarity),
                                         ncol=num,dimnames=list(colnames(expedia_cluster_similarity)))
    
    #expedia_cluster_neighbours
    
    #num
    for(i in 1:ncol(final_data)) 
    {
      expedia_cluster_neighbours[i,] <- (t(head(n=num,rownames(expedia_cluster_similarity[order(expedia_cluster_similarity[,i],decreasing=TRUE),][i]))))
    }
    
    #expedia_cluster_neighbours
    # Here we have the top 10 similar clusters for each cluster in the desination 
    
    write.csv(expedia_cluster_neighbours,"expedia_cluster_neighbours.csv")
    rownames(expedia_cluster_similarity) = a
    # returning the neighbours ######## we can retuen something else may be
    #View(expedia_cluster_neighbours)
    destination_clusters = read.csv("expedia_cluster_neighbours.csv")
    colnames(destination_clusters) = gsub("V", "", names(destination_clusters), fixed = TRUE)
    setnames(destination_clusters, "X", "Hotel cluster")
    
  }
  return(destination_clusters)
}

##############################################
# Calling this function

top10Content = as.data.frame(conventional_contentBasedFiltering(dest_id = 8250))
View(top10Content)
### The output for this function gives us the top clusters similar to the clusters for this destination id 
#head(top10Content)

#Hotel cluster 1  2  3  4 5 6 7 8 9 10 11
#1             7 1  2  3  4 5 6 7 8 9 10 11
#2            15 1  2  3  4 5 6 7 8 9 10 11
#3            16 3  8 10  4 7 1 2 5 6  9 11
#4            18 4  3  8 10 7 1 2 5 6  9 11
#5            32 5  1  2  3 4 6 7 8 9 10 11
#6            39 6 11  7  1 2 3 4 5 8  9 10

# this is the outout for destination id 11320
# user can see which clusters are similar and device promotional strategies for similar clusters
#######################################################################
############ Creating a collaborative recommendation function #######
############ INPUT - The search destination id and user id
############ OUTPUT - The top clusters for the users for the input destination id
#######################################################################

conventional_collaborativeFiltering = function(dest_id)
{
  #expedia_cluster_similarity = as.matrix(findclustersimilarity(dest_id))
  
  x1 = expedia_training_booked[expedia_training_booked$srch_destination_id == dest_id,]
  
  if(nrow(x1)==0){
    user_score_top_3 = c("Please enter a valid value for search destination id")
  } else {
    
  # save it into a file to obtain required formatting
  write.csv(x1,"testff.csv")
  
  #read this file  
  new_query_output = read.csv("testff.csv")
  
  #View(new_query_output)
  #new_query_output = new_query_output[,-1]
  # read that data and remove it the serial number column
  
  final_data1 = data.frame(unclass(table(new_query_output$user_id,new_query_output$hotel_cluster)))
  #View(final_data1)
  
  final_data =  final_data1 / (1 + final_data1)
  
  #View(final_data)
  #remove the X string from the collumn name to retain just the cluster number info
  
  a = gsub("X", "", names(final_data), fixed = TRUE)
  
  # now assign that to the column name
  final_data
  colnames(final_data) = a
  #(new_query_output$hotel_cluster)
  
  
  write.csv(final_data,"user_cluster_mat.csv")
  # save the matrix
  
  # function for calculating cosine distances
  #View(fdf)
  
  getCosine <- function(x,y) 
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(this.cosine)
  }
  
  # now we create a similarity matrix between the clusters
  
  expedia_cluster_similarity  <- matrix(0,nrow = ncol(final_data),ncol = ncol(final_data))
  
  #View(expedia_cluster_similarity)
  
  colnames(expedia_cluster_similarity) = a
  #colnames(expedia_cluster_similarity)
  # same as the matrix
  
  # looping to populate the matrix
  for(i in 1:ncol(final_data)) {
    # Loop through the columns for each column
    for(j in 1:ncol(final_data)) {
      # Fill in placeholder with cosine similarities
      expedia_cluster_similarity[i,j] <- getCosine(as.matrix(final_data[i]),as.matrix(final_data[j]))
    }
  }
  # Back to dataframe
  expedia_cluster_similarity <- as.data.frame(expedia_cluster_similarity)
  
  #write.csv(expedia_cluster_similarity,"expedia_cluster_similarity.csv")
  
  # Get the top 10 neighbours for each cluster
  num = ifelse(ncol(final_data)>11,11,ncol(final_data))
  num
  # if clusters are less than 10, we will take num of clusters
  expedia_cluster_neighbours <- matrix(NA, nrow=ncol(expedia_cluster_similarity),
                                       ncol=num,dimnames=list(colnames(expedia_cluster_similarity)))

  #expedia_cluster_neighbours
  
  for(i in 1:ncol(final_data)) 
  {
    expedia_cluster_neighbours[i,] <- (t(head(n=num,rownames(expedia_cluster_similarity[order(expedia_cluster_similarity[,i],decreasing=TRUE),][i]))))
  }
  
  expedia_cluster_neighbours
  
  #View(expedia_cluster_similarity)
  ##colnames(expedia_cluster_similarity)
  
  a = colnames(expedia_cluster_similarity)
  rownames(expedia_cluster_similarity) = a
  # Lets make a helper function to calculate the scores
  getScore <- function(history, similarities)
  {
    x <- sum(history*similarities)/sum(similarities)
    x
  }
  
  write.csv(final_data,"user_cluster_mat.csv")
  
  final_data_user = read.csv("user_cluster_mat.csv")
  #View(final_data_user)
  
  colnames(final_data_user) = gsub("X", "", names(final_data_user), fixed = TRUE)
  setnames(final_data_user, "", "users")
  
  #colnames(final_data_user) = gsub("X", "", names(final_data_user), fixed = TRUE)
  
  holder <- matrix(0, nrow=nrow(final_data_user),ncol=ncol(final_data_user)-1,dimnames =list(final_data_user$users,colnames(final_data_user[-1])))
  colnames(holder)
  #View(final_data_user)
  
  # Loop through the users (rows)
  for(i in 1:nrow(holder)) 
  {
    # Loops through the products (columns)
    for(j in 1:(ncol(holder))) 
    {
      # Get the user's name and th product's name
      # We do this not to conform with vectors sorted differently 
      user <- rownames(holder)[i]
      product <- colnames(holder)[j]
      
      # We do not want to recommend products you have already consumed
      # If you have already consumed it, we store an empty string
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN<-((head(n=11,(expedia_cluster_similarity[order(expedia_cluster_similarity[,product],decreasing=TRUE),][product]))))
      #topN
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # Drop the first one because it will always be the same song
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # We then get the user's purchase history for those 10 items'
      #topN.names
      topN.purchases<- final_data_user[,c("users",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("users"))])
      
      # We then calculate the score for that product and that user
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
      #} # close else statement
    }# # end product for loop   
  } # end user for loop
  
  expedia_user_scores = holder
  
  #View(expedia_user_scores)
  
  ########
  
  # We first have to get a product's top 10 neighbours sorted by similarity
  topN<-((head(n=11,(expedia_cluster_similarity[order(expedia_cluster_similarity[,product],decreasing=TRUE),][product]))))
  topN.names <- as.character(rownames(topN))
  topN.similarities <- as.numeric(topN[,1])
  
  # Drop the first one because it will always be the same song
  topN.similarities<-topN.similarities[-1]
  topN.names<-topN.names[-1]
  
  topN.names
  
  # We then get the user's purchase history for those 10 items
  topN.purchases<- final_data_user[,c("users",topN.names)]
  topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
  topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("users"))])
  
  # We then calculate the score for that product and that user
  holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
  
  #View(holder)
  #ncol(expedia_scores_holder)
  
  # Lets make our recommendations pretty
  expedia_scores_holder <- matrix(NA, nrow=nrow(expedia_user_scores),ncol=ncol(expedia_user_scores),dimnames=list(rownames(expedia_user_scores)))
  for(i in 1:nrow(expedia_user_scores)) 
  {
    expedia_scores_holder[i,] <- names(head(n=100,(expedia_user_scores[,order(expedia_user_scores[i,],decreasing=TRUE)])[i,]))
  }
  write.csv(expedia_scores_holder,"expedia_scores_holder.csv")
  
  user_score = read.csv("expedia_scores_holder.csv")
  colnames(user_score) = gsub("V", "", names(user_score), fixed = TRUE)
  setnames(user_score, "X", "user id")
  user_score_top_3 = user_score[,1:4]
  }
  return(user_score_top_3)
}


######################################################
# Calling the function
######################################################

TopClustersforUsers = conventional_collaborativeFiltering(dest_id = 8250)
View(TopClustersforUsers)

# the output for this function gives us the top 10 recommended clusters for the user ids for that
# the output come completely because of 
# for sech_destination_id = 8250
#user id   1  2  3  
#1    1880 32 35 34 
#2    1916 99 79 19 
#3    2956 35 76 94 
#4    4397 45 24 88 
#5    6159 94 99 41 
#6    9265 10 71 79 

# this code can be used by the company on a middle layer before the ui
# based on the clusters suggested for the users,
# expedia can try to promote offers for hotels belonging to that cluster
######################################################
# pACKAGES FOR RECOMMENDATIONS
######################################################

# Using recommenderlab

library(recommenderlab)
library(arules)

affinity.data<-read.csv("user_cluster_mat.csv")
View(affinity.data)
u1 = affinity.data[1,1]
u1

# use this value in the predict function in double quotes

#affinity.data <- affinity.data[-1]

#affinity.matrix = as.matrix(affinity.data)
#summary(affinity.matrix)
affinity.matrix<- as(affinity.data,"realRatingMatrix")

#View(affinity.matrix)

Rec.model<-Recommender(affinity.matrix, method = "UBCF")

recommended.items.4397 <- predict(Rec.model, affinity.matrix["1880",], n=1)
# to display them
as(recommended.items.4397, "list")
# to obtain the top 3
# we tried to implement this package but it requires proper rating data which is unavailable in our case
# but this can be noted if recommender systems are to be implemented anytime with rating data



