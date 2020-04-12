setwd("C:/Users/User/Desktop/product_recommendation_engine")
library(data.table)
library(dplyr)
library(reshape2)
#Loading the datasets
prod_mast<-fread("ProductMasterNovDec18.csv")
trans_data<-fread("TransactionDataNov18Dec18.csv")
nei_lo<-fread("NeighborLoyaltyCardData.csv")
nei_seg<-fread("NeighborSegmentJan19.csv")


#######################Joining Product Master and Transaction Data##################################
prod_trans<-trans_data[prod_mast,on=.(ItemId),nomatch=0][order(TransactionId)]
trans_nei<-merge(trans_data,nei_lo,by.x="LoyaltyCardId",by.y="LoyaltyCardNumber")
prod_nei<-merge(trans_nei,prod_mast,by="ItemId")
final_db<-merge(nei_seg,prod_nei,by="NEIGHBORID")

#######################Calculating the Score for Item Similarity####################################
final_demo<-final_db[,DSCS:=paste(Department,"/",SubDepartment,"/",Class,"/",SubClass)][,DSCS_Description:=paste(DSCS,"_",Description)]
final_demo<-final_db[,c("TransactionId","DSCS_Description","ItemId","NetAmount","NEIGHBORID","Qty","Segment","DepartmentId","SubDepartmentId")][order(TransactionId)][NetAmount>=0]
final_demo<-final_demo[,TotalAmount:=sum(NetAmount),by=.(ItemId,NEIGHBORID)]
final_demo<-final_demo[,TotalQty:=sum(Qty),by=.(ItemId,NEIGHBORID)]
final_demo<-unique(final_demo)
final_demo<-final_demo[,QtyScore:=TotalQty/sum(TotalQty),by=.(NEIGHBORID)]

final_demo<-final_demo[,Score:=(final_demo$DepartmentId*10+final_demo$SubDepartmentId+final_demo$QtyScore)]

###################function to generate the recommendations########################################
fly<-function(user)
{
  User_prof<-final_demo[NEIGHBORID==user,]
  Neigh_prof<-final_demo[(final_demo$Segment=="A1"|final_demo$Segment=="A2"|final_demo$Segment=="A3"),]
  Neigh_prof<-final_demo[(final_demo$DepartmentId==User_prof$DepartmentId & final_demo$SubDepartmentId==User_prof$SubDepartmentId),]
  UImatrix<-xtabs(Neigh_prof$Score~Neigh_prof$NEIGHBORID+Neigh_prof$ItemId,sparse = TRUE)

  cal_cos <- function(X, Y)
  {
    ones <- rep(1,nrow(X))		
    means <- drop(crossprod(X^2, ones)) ^ 0.5
    diagonal <- Diagonal( x = means^-1 )
    X <- X %*% diagonal
    
    ones <- rep(1,nrow(Y))		
    means <- drop(crossprod(Y^2, ones)) ^ 0.5
    diagonal <- Diagonal( x = means^-1 )
    Y <- Y %*% diagonal
    
    crossprod(X, Y)
    
  }
  sim<-cal_cos(UImatrix,UImatrix)
  sim<-as.matrix(sim)
  x<-(sim[rownames(sim)==User_prof$ItemId,])
  x<-as.data.frame(as.matrix(x))
  x<-melt(sim,value.name = "Similarity")
  x<-as.data.table(x)
  x<-x[x$Var2==User_prof$ItemId,]
  x<-x[order(desc(x$Similarity)),]
  colnames(x)[colnames(x)=="Var2"] <- "Items_bought"
  colnames(x)[colnames(x)=="Var1"] <- "ItemId"
 
  y<-final_demo[,c("DSCS_Description","ItemId")]
  x<-merge(y,x,by="ItemId")
  x<-x[order(desc(Similarity))]
  x<-unique(x)
  x$ItemId <-as.numeric(x$ItemId)
  x$Similarity <-as.numeric(x$Similarity)
  x<-anti_join(x,User_prof,by="ItemId")
  View(x)
  #z<-Neigh_prof[,c("DSCS_Description","SubDepartmentId","DepartmentId")]
  #z<-merge(x,z,by="DSCS_Description")
  #z<-z[order(desc(z$Similarity)),]
  #rownames(z)<-NULL
  #z<-unique(z)
  #z<-as.data.table(z)
  #z<-z[,head(.SD, 10), by =.(DepartmentId,SubDepartmentId)]
  #View(z)
  #print(User_prof)
}
