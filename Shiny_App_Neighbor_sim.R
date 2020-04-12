library(shiny)
library(data.table)
library(dplyr)
library(DT)
library(dplyr)
setwd("C:/Users/User/Desktop/product_recommendation_engine")
DT<-fread("Cat_dog_one.csv")
dt<-fread("Rules.csv")
X<-fread("NEIGHBOR.csv")
mylist<-X[,x][1:15000]
# Define UI for dataset viewer app ----
ui <-shinyUI(
  fluidPage(
    tags$style("#T1{font-size:20px;color:red;display:block;}"),
    
    tags$style("#T2{font-size:17px;color:blue;display:block;}"),
    tags$style("#T3{font-size:17px;color:black;display:block;}"),
    tags$style("#user{font-size:17px;color:black;display:block;}"),
    tags$style(type='text/css',
               ".selectize-dropdown-content{
               height: 1000px;
              
              
               }"),
  # App title ----
  titlePanel("Product Recommendation Engine"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Specify the number of observations to view ----
      #selectInput("user","NEIGHBORID",choices=mylist$`X[, x][1:5000]`),
      #numericInput("user","NEIGHBORID",14),
      selectizeInput(inputId = "user", label = h3("NEIGHBORID"), choices = mylist,
                     options = list(placeholder = "Type NEIGHBOR ID",
                                    maxOptions = 60000)
      ),
    
      h3("SUMMARY"),
      h4("Number of Transactions made by Neighbor"),
      verbatimTextOutput("T1"),
      h4("Date of Transactions of Neighbor"),
      tableOutput("T2"),
      h4("Neighbor's Department"),
      tableOutput("T3")

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + table of distribution ----
     
      
      h3("Recommendations"),
      DT::dataTableOutput("table"),
      h3("Items Bought"),
      DT::dataTableOutput("table1")
      
      
    )
    
  )
)
)
# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  dataInput<-reactive({  NW1<-DT[NEIGHBORID==input$user,c("ItemId","Department","Class","DSCS_Description")]
  NW2<-DT[DT$ItemId==NW1$ItemId,c("NEIGHBORID","ItemId","DSCS_Description")]
  NW3<-DT[DT$ItemId!=NW1$ItemId]
  NW3<-NW3[NW1$Department==NW3$Department]###Items of same department but 
  #different than the items already 
  #bought by user##########
  NW4<-NW3
  NW3<-NW3[NW1$Class==NW3$Class]
  NW3<-NW3[,c("DSCS_Description","ItemId")]###### Same Department and same class########
  NW4<-NW4[,c("DSCS_Description")]
  ######Same Department but not same class######
  NW3<-unique(NW3)
  NW3<-NW3[!NW2,on=.(DSCS_Description,ItemId)]
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
  #########################Cross-Sell#############################################
  k$score<-stringdist::stringdist(k$DSCS_Description,k$DSCS_desc,method = "cosine")
  k<-data.table(k)
  k<-unique(k)
  colnames(k)[colnames(k)=="ItemId"] <- "ID"
  k1<-k[,head(.SD,30),by=.(DSCS_Description)][order(DSCS_Description)][order(desc(score))]
  k1<-unique(k1[,c("DSCS_desc")])
  k2<-k1[1:30,]
  
  t$score<-stringdist::stringdist(t$DSCS_Description,t$DSCS_desc,method = "cosine")
  t<-data.table(t)
  t<-unique(t)
  t1<-t[,head(.SD,30),by=.(DSCS_Description)][order(DSCS_Description)][order(desc(score))]
  t1<-unique(t1[,c("DSCS_desc")])
  t2<-t1[1:50,]
  
  bind_df1<-rbind(k2,t2)
  #################Combining the rules generated########################### 
  dt1<-dt[dt$lhs==NW1$DSCS_Description,][order(desc(lift))]
  Z<-NW1[,c("DSCS_Description","ItemId")]
  colnames(Z)[colnames(Z)=="DSCS_Description"] <- "DSCS_desc"
  
  if(nrow(dt1)!=0)
  {
    dt2<-dt1[,-c("lhs","support","count","lift","confidence","V1")]
    colnames(dt2)[colnames(dt2)=="rhs"] <- "DSCS_desc"
    dt2<-data.table(dt2)
    dt2<-dt2[1:5,]
    bind_df<-rbind((bind_df1),as.data.frame(dt2),fill=T)
    bind_df<-anti_join(bind_df,Z)
    bind_df<-unique(bind_df)
   
  } else {
    bind_df<-bind_df1
  }
  bind_df<-as.data.table(bind_df[1:10,])
  colnames(bind_df)[colnames(bind_df)=="DSCS_desc"] <- "Items"
  colnames(bind_df)[colnames(bind_df)=="V1"] <- "Items"
  bind_df<-as.data.table(bind_df)
  bind_df=bind_df[,c("Items")]
  unique(bind_df)
  })
  
  
  
  dataInput1<-reactive({User_prof<-DT[NEIGHBORID==input$user,]
  User_prof<-User_prof[,c("DSCS_Description")]
  colnames(User_prof)[colnames(User_prof)=="DSCS_Description"] <- "Items"
  User_prof
  })
  
  dataInput2<-reactive({User_prof<-DT[NEIGHBORID==input$user,]
  Transactions<-(uniqueN(User_prof$TransactionId))
  Transactions
  })
 
  dataInput3<-reactive({User_prof<-DT[NEIGHBORID==input$user,]
  Trans_date<-unique(User_prof[,c("Transdate")])
  #Trans_date<-as.Date((Trans_date),format="%Y%m%d")
  colnames(Trans_date)[colnames(Trans_date)=="Transdate"]<-"Date"
  Trans_date
  })
  
  dataInput4<-reactive({User_prof<-DT[NEIGHBORID==input$user,]
  Dep<-(unique(User_prof[,c("Department")]))
  Dep
  })
   
  output$table1 <- DT::renderDataTable(DT::datatable({
    data1 <-dataInput1()
    data1
  })) 
  output$table <- DT::renderDataTable(DT::datatable({
    data <-dataInput()
    data
  })) 
  
  output$T1 <- renderText({
    data <-dataInput2()
    data
  })

  
  output$T2 <- renderTable({
    Date <-dataInput3()
     Date
  })
  output$T3 <- renderTable({
    Department <-dataInput4()
    Department
  })
  
 
  }
# Create Shiny app ----
shinyApp(ui, server)
