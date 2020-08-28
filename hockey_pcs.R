load("hockey_pc.Rdata")

#Use numeric data
X=as.matrix(players[,c(4:14)])
X.cs=scale(X,scale=T)

#Plot pairwise scatterplots
pairs(X.cs,lower.panel=NULL,
      main="pairs plot of players data\n (standard deviation units)")
par(xpd=TRUE)

#Variance matrix
S=var(X.cs)

#Eigenvalues of variance matrix
eigenvalues=eigen(S)$values
round(eigenvalues,3)  #for presentation purposes only 

##Percentage contributed by each
round(100*eigenvalues/sum(eigenvalues),3)

#Cumulative sum of contribution of each PC
cumsum(round(100*eigenvalues/sum(eigenvalues),3))



########## Get the eigenvectors 

V <- eigen(S)$vectors 
V.r <- round(V,3)  # for presentation purposes 
V.r

## See relation of each data variable to the PC
## I fo with rounded values for presentation purposes only 

rownames(V.r)=colnames(X)
colnames(V.r)=c("V1", "V2", "V3","V4","V5","V6","V7","V8","V9","V10","V11")


##########################
# Obtain the principal components 
###########################

X.tilde <- X.cs%*%V  # X.tilde are the PC
round(var(X.tilde),2)

colnames(X.tilde) <- c("PC1","PC2","PC3","PC4","PC5","PC6", "PC7", "PC8", "PC9", "PC10", "PC11")

#### Check the variance-covariance  of the PC 
round(var(X.tilde),3)

############Pairwise plot of the PCs

pairs(X.tilde,lower.panel=NULL,
      main="Pairs plot of PC of players data.\n Same scale in axes to see variability",
      xlim=c(-3.5,3.5),ylim=c(-3.5,3.5))
par(xpd=TRUE)




### New plot, the biplot 

biplot(princomp(X.cs),
       main="Biplot of players data w.r.t. first two principal components" )


transform.matrix=diag(sqrt(eigenvalues))
transform.matrix


#Correlation between variables and PCs 
t(cor(X.tilde,X.cs))

###Plotting the first two PCs to find groupings
plot(X.tilde[,1], X.tilde[,2], xlab = "Size", ylab = "Shape")

players$Player[which(X.tilde[,4] == max(X.tilde[,4]))]
which(X.tilde[,1] == head(sort(X.tilde[,1]),n = 20))
prince <- as_tibble(X.tilde)
prince <- prince  %>% 
  mutate(person = 1:707) %>% 
  mutate(name = players$Player,
         pos = players$pos,
         offense = -1*PC1,
         defense = PC2,
         transition = PC3,
         penalty = -1*PC4)
ggplot(prince,aes(offense,defense,color = pos)) +
  geom_point() +
  xlab("Contribution to Offense") +
  ylab("Defensive Tendency") +
  ggtitle("Offense vs Defense -- 2019-20 Season")

forwards <- prince %>% filter(pos == "F")
ggplot(forwards,aes(offense,defense)) +
  geom_point(aes(col = pos)) +
  xlab("Contribution to Offense") +
  ylab("Defensive Tendency") +
  ggtitle("Offense vs Defense -- 2019-20 Season") +
  geom_hline(yintercept = mean(forwards$defense)) +
  geom_vline(xintercept = mean(forwards$offense))



