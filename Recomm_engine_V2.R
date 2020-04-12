setwd("C:/Users/User/Desktop/product_recommendation_engine")
library(data.table)
library(dplyr)
library(reshape2)
library(Matrix)
#Loading the datasets

dt<-fread("RecommenderSystemItern_one_month.csv")
dt<-unique(dt)
dt<-dt[,totalQty:=sum(Qty),by=.(ItemId,NEIGHBORID)][order(Transdate)]
#dt<-dt[order(Transdate),][Transdate<="2018-12-17 00:00:00 UTC" & Transdate>="2018-12-05" ][(Segment=="A1"|Segment=="A2"|Segment=="A3"),]

#Adding DSCS Column

dt2<-fread("RecommenderSystemIterndata_dog_one_month.csv")
dt2<-unique(dt2)
#dt2<-dt2[order(Transdate),][Transdate<="2018-12-17 00:00:00 UTC" & Transdate>="2018-12-05" ][(Segment=="A1"|Segment=="A2"|Segment=="A3"),]
dt2<-dt2[,totalQty:=sum(Qty),by=.(ItemId,NEIGHBORID)][order(Transdate)]
dt3<-rbind(dt2,dt)

DT<-dt3[,DSCS:=paste(Department,SubDepartment,Class,SubClass)]


DT<-DT[,DSCS_Description:=paste(DSCS,"_",Description)][order(NEIGHBORID)][order(NEIGHBORID)][Qty>0]

#DT<-DT[,head(.SD,50000),by=Segment]
write.csv(DT,file="Cat_dog_one.csv")
dT<-fread("Rules.csv")



recom_fun<-function(user)
{
NW1<-DT[NEIGHBORID==user,c("ItemId","Department","Class","DSCS_Description")]
NW2<-DT[DT$ItemId==NW1$ItemId,c("NEIGHBORID","ItemId","DSCS_Description")]
NW3<-DT[DT$ItemId!=NW1$ItemId]
NW3<-NW3[NW1$Department==NW3$Department]###Items of same department but 
#different than the items already 
#bought by user##########
NW4<-NW3
NW3<-NW3[NW1$Class==NW3$Class]
NW3<-NW3[,c("DSCS_Description")]###### Same Department and same class########
NW4<-NW4[,c("DSCS_Description")]
######Same Department but not same class######
NW3<-unique(NW3)
NW3<-NW3[!NW2,on=.(DSCS_Description)]
NW4<-unique(NW4)

colnames(NW3)[colnames(NW3)=="DSCS_Description"] <- "DSCS_desc"
colnames(NW4)[colnames(NW4)=="DSCS_Description"] <- "DSCS_desc"

#############################CROSSJOIN########################################
cross <- function(a,b){
  c = CJ(1:nrow(a),1:nrow(b))
  cbind(a[c[[1]],],b[c[[2]],])
}

A = NW3
B = NW1
C = NW4
k<-cross(B,A)
t<-cross(C,B)
########################String Similarity#######################################
k$score<-stringdist::stringdist(k$DSCS_Description,k$DSCS_desc,method = "cosine")
k<-data.table(k)
k<-unique(k)
k1<-k[,head(.SD,20),by=.(DSCS_Description)][order(DSCS_Description)][order(desc(score))]
k1<-unique(k1[,-c("DSCS_Description","ItemId","Department","Class","score")])
k2<-k1[1:10,]

t$score<-stringdist::stringdist(t$DSCS_Description,t$DSCS_desc,method = "cosine")
t<-data.table(t)
t<-unique(t)
t1<-t[,head(.SD,20),by=.(DSCS_Description)][order(DSCS_Description)][order(desc(score))]
t1<-unique(t1[,-c("DSCS_Description","ItemId","Department","Class","score")])
t2<-t1[1:20,]

bind_df1<-rbind(k2,t2)

dt1<-dT[dT$lhs==NW1$DSCS_Description,][order(desc(lift))]
Z<-NW1[,c("DSCS_Description")]
colnames(Z)[colnames(Z)=="DSCS_Description"] <- "DSCS_desc"

if(nrow(dt1)!=0)
{
  dt2<-dt1[,-c("lhs","support","count","lift","confidence","V1")]
  colnames(dt2)[colnames(dt2)=="rhs"] <- "DSCS_desc"
  bind_df<-rbind((bind_df1),as.data.frame(dt2))
  bind_df<-anti_join(bind_df,Z)
  bind_df<-unique(bind_df)
} else {
  bind_df<-bind_df1
}

View(bind_df) 
}
