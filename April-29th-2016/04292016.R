library(igraph)
library(ggplot2)
library(ggthemes)

#Overwrite default FiveThirtyEight theme in ggthemes
source('theme_fivethirtyeight.R')




#Set Intitial population of proud partygoers to 0, amount of partygoers to 25
proud.prop <-0
n<-25
#Set sequence starting at min. number of vertices in fully connecteted graph
#Min. vertices given by n-1 
#Max. Vertices given by n*(n-1)/2 for undirected graph
e.seq<-seq(from = n-1, to = n*(n-1)/2, by = 1)

#Matrix to hold data, later to be graphed
output <- matrix(ncol=2, nrow=length(e.seq))

#Iterations to keep track of row number
iterations<-1


# Generate random graph, fixed probability
for(e in e.seq){
  
  #For each graph of size n, repeat 50 times to ensure generated graph is optimal for Proud Partygoers (PP)
  for(i in 1:50){
    #Generate random graphs until a fully connected one is made
    repeat {
      g <- erdos.renyi.game(n, e,
                            type='gnm')
      if (is.connected(g)) {
        break
      }
    }
    
    #Get adjaceny matrix and degree of all nodes
    g.adj<-data.matrix(get.adjacency(g))
    g.deg<-degree(g)
    
    #Create matrix indicating which nodes are "proud"
    #sum(g.deg[x]) indicates amount of friends a given node has
    #sum((g.adj[x,]*g.deg))/(rowSums(g.adj)[x]) solves for the average amount of
    #friends of friends from the original partygoer
    g.friends <- sapply(1:nrow(g.adj), 
                        function(x)
                          sum(g.deg[x])>sum((g.adj[x,]*g.deg))/(rowSums(g.adj)[x])
    )
    #If proportion of PP, print results & save in output matrix
    if(sum(g.friends)/n>proud.prop){
      proud.prop<-sum(g.friends)/n
      print(paste0("Number of Edges: ",e))
      print(paste0("Proportion of Proud: ",proud.prop))
      output[iterations,]<-c(e,proud.prop) 
    }
  }
  iterations<-iterations+1
}


#Create chart from output data
#Chart shows that PP is maximum near maximum number of edges
output<-data.frame(output)
p2 <- ggplot(output, aes(x = X1, y = X2)) +
  geom_point(size=3,shape=21,color='coral',fill='coral')

p2+theme_fivethirtyeight()+
  labs(title='Total Friendships (Edges) versus Proportion Proud Partygoers (N=20)',
       x='Total Friendships (Edges)',
       y='Proportion Proud Partygoers')+
  geom_vline(xintercept=300,linetype=2)+
  geom_vline(xintercept=4,linetype=2)+
  annotate("text", x =20, y = 1, label = "Min. Edges",color='firebrick') +
  annotate("text", x =315, y = 1, label = "Max. Edges",color='firebrick') 




#Create data frame of verices (e) at a given amout of partygoers (N
#when PP is maximum
output <- matrix(ncol=2, nrow=100-4)
output<-data.frame(output)
output$X1<-5:100
output$X2<-output$X1*(output$X1-1)/2-1

p2 <- ggplot(output, aes(x = X1, y = X2)) +
  geom_point(size=3,shape=21,color='coral',fill='coral')

p2+theme_fivethirtyeight()+
  labs(title='Partygoers (N) versus Friendship at Maximum Proud Partygoers',
       x='Partygoers (N)',
       y='Friendship at Maximum Proud Partygoers')


#Print HTML table of data for website use
colnames(output)<-c('Partygoers (N)','Maximum Friendships (Edges')
output[output$Partygoers %in% c(5,10,20,40,50,100),]
print(xtable(output[output$Partygoers %in% c(5,10,20,40,50,100),]), type="html",
      include.rownames=FALSE,
      align='c')








proud.prop <-0
n.seq<-seq(from = 5, to = 100, by = 1)

output <- matrix(ncol=2, nrow=length(n.seq))
iterations<-1

#Create graphs for partygoers (N) = 5:100, at maximum number
#of vertices minus 1
for(n in n.seq){
  for(i in 1:10){
    e<-n*(n-1)/2-1
    g <- erdos.renyi.game(n, e,
                          type='gnm')
    
    
    
    g.adj<-data.matrix(get.adjacency(g))
    g.deg<-degree(g)
    
    g.friends <- sapply(1:nrow(g.adj), 
                        function(x)
                          sum(g.deg[x])>sum((g.adj[x,]*g.deg))/(rowSums(g.adj)[x])
    )
    
    if(sum(g.friends)/n>proud.prop){
      proud.prop<-sum(g.friends)/n
      print(paste0("Number of Nodes: ",n))
      print(paste0("Number of Edges: ",e))
      print(paste0("Proportion of Proud: ",proud.prop))
      
    }
  }
  output[iterations,]<-c(n,proud.prop)
  proud.prop <-0
  iterations<-iterations+1
}


#Create chart of generated graph
output <- data.frame(output)
p2 <- ggplot(output, aes(x = X1, y = X2)) +
  geom_point(size=3,shape=21,color='coral',fill='coral')



p2+theme_fivethirtyeight()+
  labs(title='Partygoers versus Proportion Proud Partygoers',
       x='Partygoers (N)',
       y='Proportion Proud Partygoers')
