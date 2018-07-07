#install.packages("openxlsx")
#install.packages("stringr")
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("caTools")
library("openxlsx")
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(stringr)
drivermaster_df <- read.xlsx("Driver_Master.xlsx", sheet = 1, colNames = TRUE)
driver_17_06<-read.csv("DriverInfo_2017-06.csv",stringsAsFactors = F,header = F)
driver_17_07<-read.csv("DriverInfo_2017-07.csv",stringsAsFactors = F,header = F)
driver_17_08<-read.csv("DriverInfo_2017-08.csv",stringsAsFactors = F,header = F)
driver_17_09<-read.csv("DriverInfo_2017-09.csv",stringsAsFactors = F,header = F)
driver_17_10<-read.csv("DriverInfo_2017-10.csv",stringsAsFactors = F,header = F)
driver_17_11<-read.csv("DriverInfo_2017-11.csv",stringsAsFactors = F,header = F)
driver_17_12<-read.csv("DriverInfo_2017-12.csv",stringsAsFactors = F,header = F)

driver_18_01<-read.csv("DriverInfo_2018-01.csv",stringsAsFactors = F,header = F)
driver_18_02<-read.csv("DriverInfo_2018-02.csv",stringsAsFactors = F,header = F)
driver_18_03<-read.csv("DriverInfo_2018-03.csv",stringsAsFactors = F,header = F)
driver_18_04<-read.csv("DriverInfo_2018-04.csv",stringsAsFactors = F,header = F)
driver_18_05<-read.csv("DriverInfo_2018-05.csv",stringsAsFactors = F,header = F)

#merging all driverinfo data frames
driver_info<-rbind(driver_17_06,driver_17_07,driver_17_08,driver_17_09,driver_17_10,driver_17_11,driver_17_12,driver_18_01,driver_18_02,driver_18_03,driver_18_04,driver_18_05)
str(driver_info)
#Provide the column names for driver info df
colnames(driver_info) <- c("Driver_Id",	"Job_ID",	"Drop_City",	"Drop_State",	"Drop_Country"	,"Distance_Miles"	,"Deliverd_DateTime",	"Standard_Pay_Ammount"	,"Non_Standard_Pay_Ammount",	"Total_Pay"	,"VOC_SENT",	"VOC_SATISFACTION")

# Blank Values
sapply(driver_info, function(x) length(which(x == ""))) # there are blanks,we treat columnwise
#driverId
driver_info$Driver_Id<-as.numeric(driver_info$Driver_Id)
#JOB_ID - this is unique value. So we calculate total jobs delivered by driver and check its impact on churn 
length(unique(driver_info$Job_ID))
driver_info$Job_ID<-as.numeric(driver_info$Job_ID)
#sum(duplicated(driver_info$Job_ID))

#City
driver_info$Drop_City<-trimws(driver_info$Drop_City)
driver_info$Drop_City<-toupper(driver_info$Drop_City)
#State
driver_info$Drop_State<-trimws(driver_info$Drop_State)
driver_info$Drop_State<-toupper(driver_info$Drop_State)
#Country
driver_info$Drop_Country<-trimws(driver_info$Drop_Country)
#Country info is mostly incorrect. checking the summary country as state postal code info and all states belong to US. Removing this column as this is not key paramter for analysis
driver_info$Drop_Country<-as.factor(driver_info$Drop_Country)
summary(driver_info$Drop_Country)
driver_info$Drop_Country<-NULL
#dropMiles
driver_info$Distance_Miles<-trimws(driver_info$Distance_Miles)
driver_info$Distance_Miles<-as.numeric(driver_info$Distance_Miles)
#dropDate
#Cleaning the date column.

driver_info$Deliverd_DateTime<-trimws(driver_info$Deliverd_DateTime)
driver_info$Deliverd_DateTime<-as.Date(driver_info$Deliverd_DateTime,format="%Y-%m-%d %H:%M:%S")
#b<-as.Date(driverjob_df$Delivery_Date,format="%Y-%m-%d")
#a[is.na(a)] <- b[!is.na(b)]
#driverjob_df$Delivery_Date<-a

#Standard pay amount
driver_info$Standard_Pay_Ammount<-trimws(driver_info$Standard_Pay_Ammount)
driver_info$Standard_Pay_Ammount<-as.numeric(driver_info$Standard_Pay_Ammount)

#VOC_SENT- this column indicates if feedback recieved after delivery.
driver_info$VOC_SENT<-NULL

driver_info$VOC_SATISFACTION<-as.numeric(driver_info$VOC_SATISFACTION)
colMeans(is.na(driver_info))
#Driver_Id                Drop_City               Drop_State           Distance_Miles        Deliverd_DateTime     Standard_Pay_Ammount Non_Standard_Pay_Ammount                Total_Pay         VOC_SATISFACTION 
#0.0000000000             0.0000000000             0.0000000000             0.0096602366             0.0002798404             0.0002798404             0.0001399202             0.0001399202             0.8194781419 
driver_info$VOC_SATISFACTION[is.na(driver_info$VOC_SATISFACTION)]<-2
#Cleaning all NA's in driver info df
driver_info<-na.omit(driver_info)
driver_info$VOC_SATISFACTION[driver_info$VOC_SATISFACTION==2]<-NA

#check is driver delivery date is before current date. else it is incorrect data
driver_info[-which(driver_info$Delivery_Date > as.Date('2018-06-07') ),]->driverjob_df1
nrow(driverjob_df1) #0 So all delivery dates are correct

#correlation matrix for numeric data
driver_info_cordf<-driver_info[,c("Distance_Miles","Standard_Pay_Ammount","Non_Standard_Pay_Ammount","Total_Pay")]
driver_info_correlation<- as.data.frame(round(cor(driver_info_cordf),2))
View(driver_info_correlation) #Standard_Pay_Ammount is highly correlated to Total_pay.So we are eliminating Standard_Pay_Ammount 


driver_info$Drop_State<-strtrim(driver_info$Drop_State,2)
driver_info$Drop_State[!driver_info$Drop_State %in% state.abb]<-"UNKNOWN"
unique(driver_info$Drop_State)

drivermaster_df$dr_state<-trimws(drivermaster_df$dr_state)
drivermaster_df$dr_state<-toupper(drivermaster_df$dr_state)
unique(drivermaster_df$dr_state)
drivermaster_df[which(drivermaster_df$dr_state==""),"dr_state"]<-"UNKNOWN"
#removing different driver Ids
setdiff(drivermaster_df$Driver_ID,driver_info$Driver_Id) -> driverDiff
drivermaster_df<-drivermaster_df[-which(drivermaster_df$Driver_ID %in% driverDiff),]
colnames(drivermaster_df)[1]<-"Driver_Id"


d<-drivermaster_df[,c("Driver_Id","dr_state")]
driver_info<-merge(driver_info,d,by="Driver_Id",all=F)
driver_info$DifferentStatesDeliverymatched<-ifelse(driver_info$dr_state==driver_info$Drop_State, 0, 1)


#create derived variables
#1.total jobs delivered by driver
#2.Total distance covered in delivering above jobs .
#3.latest delivery date of each driver- this is mainly used id driver is churned/not churned.Not for model building
#4.Total_Pay - total payment recieved
#5.Average customer statisfaction rating with driver
#6.Delivered in different state
driverjobupdated_df<-unique(transform(driver_info$Driver_Id, 
                                      total_jobs=ave(driver_info$Job_ID, driver_info$Driver_Id, FUN=length),
                                      Distance_Miles=ave(driver_info$Distance_Miles, driver_info$Driver_Id, FUN=sum),
                                      latest_delivery_date=ave(driver_info$Deliverd_DateTime,driver_info$Driver_Id,FUN=max),
                                      total_Payment=ave(driver_info$Total_Pay,driver_info$Driver_Id,FUN=sum),
                                      total_NonStandard_Payment=ave(driver_info$Non_Standard_Pay_Ammount,driver_info$Driver_Id,FUN=sum),
                                      totalDeliveriesinOtherStates=ave(driver_info$DifferentStatesDeliverymatched,driver_info$Driver_Id,FUN=sum),
                                      avg_voc_satisfaction=ave(driver_info$VOC_SATISFACTION,driver_info$Driver_Id,FUN=function(x) mean(x, na.rm=T))))

colnames(driverjobupdated_df)[1]<-"Driver_Id"
driverjobupdated_df -> driverinfo_df

sum(is.nan(driverjobupdated_df$avg_voc_satisfaction))
driverjobupdated_df$avg_voc_satisfaction[is.na(driverjobupdated_df$avg_voc_satisfaction)]<-0

#One more derived variable addition. Month of churn
driverjobupdated_df$Churnedmonth <- format(driverjobupdated_df$latest_delivery_date,"%m")
driverjobupdated_df$Churnedmonth<-as.factor(driverjobupdated_df$Churnedmonth)

currentDate <- as.Date('2018-06-07') 

churned <- matrix(nrow = nrow(driverjobupdated_df), ncol = 1)
churned<-as.data.frame(churned)
getchurneddriver <- function(lastDeliveryDate ){
  days <- currentDate - lastDeliveryDate
  for(i in 1:length(lastDeliveryDate)){
    if(days[i]>90){churned[i,1]=1}else{churned[i,1]=0}
  }
  return(churned)
}
churned<-getchurneddriver(driverjobupdated_df$latest_delivery_date)
colnames(churned)[1]<-"churned"
driverjobupdated_df$churend <- churned$churned
driverjobupdated_df$churend<-as.factor(driverjobupdated_df$churend)
driverjobupdated_df$latest_delivery_date<-NULL

#univarient analysis on driver_info dataframe
driverjobupdated_df[which(driverjobupdated_df$churend==1),]->churened_drivers
subset(driver_info, Driver_Id %in% churened_drivers$Driver_Id)->churneddriver_uni
churneddriver_uni$churned<-1

driverjobupdated_df[which(driverjobupdated_df$churend==0),]->notchurened_drivers
subset(driver_info, Driver_Id %in% notchurened_drivers$Driver_Id)->notchurneddriver_un
notchurneddriver_un$churned<-0
driver_univ<-rbind(churneddriver_uni,notchurneddriver_un)
driver_univ$churned<-as.factor(driver_univ$churned)

##### Categorical Variables ########
#1.Drop_State
driver_univ$Drop_State<-as.factor(driver_univ$Drop_State)
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


ggplot(churneddriver_uni, aes(x=reorder(Drop_State,Drop_State,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=1) +
  labs(title= "Bar Chart for Drop State") + xlab("Drop State") + ylab("driver churn count") + geom_text(stat='count',aes(label=..count..))
#MOnth of churn
ggplot(churened_drivers, aes(x=reorder(Churnedmonth,Churnedmonth,function(x)-length(x)))) + geom_bar(fill = "green", col= "red", alpha=1) +
  labs(title= "Bar Chart for Monthly churn") + xlab("Month") + ylab("driver churn") + geom_text(stat='count',aes(label=..count..))
##### NUmeric variables #############
#1.Non_Standard_Pay_Ammount
#BOX plot has couple of outliers,which will be treated
ggplot(churneddriver_uni, aes(x= "", y= churneddriver_uni$Non_Standard_Pay_Ammount)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + 
  scale_y_continuous(name = "Non_Standard_Pay_Ammount") + ggtitle("Boxplot of Non Standard Pay")

ggplot(churneddriver_uni, aes(x=Non_Standard_Pay_Ammount)) + geom_histogram(stat="bin", binwidth = 4, bins = 10, breaks = seq(18,58,by=4),
                                                                            col="red", fill="green",alpha=0.5) + labs(title="Histogram for Non_Standard_Pay_Ammount") + geom_text(stat='bin', binwidth = 4, bins = 10, breaks = seq(18,58,by=4), aes(label=..count..)) + scale_x_continuous(breaks = seq(18,58,by=4))

#BOX plot has couple of outliers,which will be treated
#2.Total pay
ggplot(churneddriver_uni, aes(x= "", y= churneddriver_uni$Total_Pay)) + geom_boxplot(fill = "blue", col="black", alpha = 0.5) + 
  scale_y_continuous(name = "Total pay") + ggtitle("Boxplot of Total Pay")
ggplot(churneddriver_uni, aes(x=Total_Pay)) + geom_histogram(stat="bin", binwidth = 4, bins = 10, breaks = seq(18,58,by=4),
                                                             col="red", fill="green",alpha=0.5) + labs(title="Histogram for Total payment") + geom_text(stat='bin', binwidth = 4, bins = 10, breaks = seq(18,58,by=4), aes(label=..count..)) + scale_x_continuous(breaks = seq(18,58,by=4))

#drivermaster df cleaning
sum(is.na(drivermaster_df$dr_city))
sum(is.na(drivermaster_df$dr_state))
sum(is.na(drivermaster_df$dr_contract))
sum(is.na(drivermaster_df$dr_paytype))

sum(is.na(drivermaster_df$dr_commission))
drivermaster_df$dr_city<-trimws(drivermaster_df$dr_city)
drivermaster_df$dr_paytype<-trimws(drivermaster_df$dr_paytype)

length(which(drivermaster_df$dr_wkbase=="NULL")) 
drivermaster_df$dr_wkbase<-NULL

#output variable no need
drivermaster_df$dr_onduty<-NULL
drivermaster_df$dr_contract <-as.factor(drivermaster_df$dr_contract)
drivermaster_df$dr_commission <-as.factor(drivermaster_df$dr_commission)
drivermaster_df$dr_paytype <-as.factor(drivermaster_df$dr_paytype)


summary(drivermaster_df$dr_paytype)
levels(drivermaster_df$dr_paytype)[1]<-'UNKNOWN'
drivermaster_df$dr_status <-as.factor(drivermaster_df$dr_status)

setdiff(drivermaster_df$Driver_Id,driverinfo_df$Driver_Id) #NULL
#fixing the unique column name in both dataframes for merging
colnames(drivermaster_df)[colnames(drivermaster_df) == "Driver_ID"] <- "Driver_Id"
combined_driver_df <- merge(drivermaster_df,driverjobupdated_df , by = "Driver_Id", all = F)

colMeans(is.na(combined_driver_df))
#Remove the missing values since percentage of number of missing values are very low
combined_driver_df<-na.omit(combined_driver_df)

#Bivariate analysis on combined data.
###CATEGORICAL DATA#####
#1.churn impact by contract type, dr_commission
plot_grid(ggplot(combined_driver_df, aes(x=dr_contract,fill=churend))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(combined_driver_df, aes(x=dr_commission,fill=churend))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme1, align = "h") 
#2.churn impact by dr_paytype,dr_status
plot_grid(ggplot(combined_driver_df, aes(x=dr_paytype,fill=churend))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(combined_driver_df, aes(x=dr_status,fill=churend))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme1, align = "h") 

###NUMERICAL DATA############
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(combined_driver_df, aes(x=churend,y=total_jobs, fill=churend))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(combined_driver_df, aes(x=churend,y=Distance_Miles, fill=churend))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(combined_driver_df, aes(x=churend,y=total_Payment, fill=churend))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(combined_driver_df, aes(x=churend,y=total_NonStandard_Payment, fill=churend))+ geom_boxplot(width=0.5)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(combined_driver_df, aes(x=churend,y=totalDeliveriesinOtherStates, fill=churend))+ geom_boxplot(width=0.5)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

ggplot(combined_driver_df, aes(x=totalDeliveriesinOtherStates)) +
  geom_histogram(stat="bin", binwidth = 4, bins = 10, breaks = seq(18,58,by=4),
                 col="red", fill="green",alpha=0.5) + labs(title="Histogram for totalDeliveriesinOtherStates") + 
  geom_text(stat='bin', binwidth = 4, bins = 10, breaks = seq(18,58,by=4), aes(label=..count..)) + scale_x_continuous(breaks = seq(18,58,by=4))

############COMPLETED Bi Variate#######
final_data<-combined_driver_df
final_data$dr_city<-NULL
final_data$dr_state<-NULL
final_data$Driver_Id<-NULL

################################################################
# Feature standardisation

# Normalising continuous features 

# creating a dataframe of numerical features
INTEGER_VARIABLES <- lapply(final_data, class) == "integer" | lapply(final_data, class) == "numeric"
churn_data_num<- final_data[, INTEGER_VARIABLES]

# Standardising all numerical features
churn_data_stand<- data.frame(sapply(churn_data_num, 
                                     function(x) scale(x)))
FACTOR_VARIABLES <- c("dr_contract","dr_commission","dr_paytype","dr_status","churend")

churnpercentage <- nrow(final_data[which(final_data$churend==1),])/nrow(final_data)
churnpercentage
# creating a dataframe of categorical features
churn_data_fact<- final_data[, FACTOR_VARIABLES]

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(churn_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =churn_data_fact))[,-1]))

# Final dataset
churn_final<- cbind(churn_data_stand,dummies) 
View(churn_final)

#Correlation matrix- from this actually we dont see much strong relation between any 2 independent variables.
churn_final_correlation<- as.data.frame(round(cor(churn_final),2))
View(churn_final_correlation)
########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(churn_final$churend, SplitRatio = 0.7)

train = churn_final[indices,]

test = churn_final[!(indices),]

########################################################################

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(churend ~ ., data = train, family = "binomial")
summary(model_1)
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.4754  -0.5962  -0.1324   0.4184   3.0107  

#Coefficients:
#                              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                  -15.25411  214.85953  -0.071  0.94340    
#total_jobs                    -0.41685    0.13731  -3.036  0.00240 ** 
#Distance_Miles                 0.16637    0.07222   2.304  0.02124 *  
#total_Payment                 -1.38019    0.16656  -8.287  < 2e-16 ***
#total_NonStandard_Payment      0.22018    0.07172   3.070  0.00214 ** 
#totalDeliveriesinOtherStates  -0.18921    0.07864  -2.406  0.01612 *  
#avg_voc_satisfaction          -0.01387    0.04868  -0.285  0.77572    
#dr_contract                   13.95829  214.86041   0.065  0.94820    
#dr_commission                 -0.60390    1.12046  -0.539  0.58990    
#dr_paytype.xCOMMISSION         0.06017    1.28373   0.047  0.96262    
#dr_paytype.xDAILY             -1.55344    0.68047  -2.283  0.02244 *  
#dr_paytype.xWEEKLY            -0.74865    0.72579  -1.031  0.30231    
#dr_status                      3.49219    0.10623  32.874  < 2e-16 ***

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
#Coefficients:
#                                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                  -15.26778  215.94209  -0.071 0.943634    
#total_jobs                    -0.42882    0.13453  -3.187 0.001435 ** 
#Distance_Miles                 0.16385    0.07188   2.279 0.022638 *  
#total_Payment                 -1.36957    0.16463  -8.319  < 2e-16 ***
#total_NonStandard_Payment      0.22029    0.07150   3.081 0.002063 ** 
#totalDeliveriesinOtherStates  -0.19153    0.07863  -2.436 0.014858 *  
#dr_contract                   13.42590  215.94209   0.062 0.950425    
#dr_paytype.xDAILY             -1.01360    0.27199  -3.727 0.000194 ***
#dr_status                      3.49501    0.10593  32.994  < 2e-16 ***
# Removing multicollinearity through VIF check
library(car)
vif(model_2)
#total_jobs               Distance_Miles                total_Payment    total_NonStandard_Payment    totalDeliveriesinOtherStates                  dr_contract 
#3.603758                     1.382624                     3.678822                     1.228426                     1.356144                       1.000000 
#dr_paytype.xDAILY                    dr_status 
#1.154475                              1.127332
#Excluding dr_contract 
model_3<- glm(formula = churend ~ total_jobs + Distance_Miles + total_Payment + avg_voc_satisfaction + total_NonStandard_Payment + totalDeliveriesinOtherStates
                +dr_paytype.xDAILY + dr_status ,
              family = "binomial", data = train) 
summary(model_3)
#Coefficients:
#                              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                  -1.85167    0.06844 -27.054  < 2e-16 ***
#total_jobs                   -0.44093    0.13529  -3.259 0.001117 ** 
#Distance_Miles                0.15644    0.07146   2.189 0.028584 *  
#total_Payment                -1.33491    0.16134  -8.274  < 2e-16 ***
#avg_voc_satisfaction         -0.01368    0.04861  -0.281 0.778370    
#total_NonStandard_Payment     0.21720    0.07151   3.037 0.002388 ** 
#totalDeliveriesinOtherStates -0.20224    0.07798  -2.594 0.009497 ** 
#dr_paytype.xDAILY            -0.96799    0.27281  -3.548 0.000388 ***
#dr_status                     3.50741    0.10591  33.116  < 2e-16 ***
vif(model_3)
#Excluding avg_voc_satisfaction 
model_4<- glm(formula = churend ~ total_jobs+Distance_Miles + total_Payment  + total_NonStandard_Payment+totalDeliveriesinOtherStates
               + dr_paytype.xDAILY + dr_status ,
              family = "binomial", data = train) 
summary(model_4)
#Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                    -1.85136    0.06843 -27.054  < 2e-16 ***
#  total_jobs                   -0.44826    0.13286  -3.374 0.000741 ***
#  Distance_Miles                0.15689    0.07145   2.196 0.028106 *  
#  total_Payment                -1.33082    0.16074  -8.279  < 2e-16 ***
#  total_NonStandard_Payment     0.21744    0.07132   3.049 0.002297 ** 
#  totalDeliveriesinOtherStates -0.20347    0.07794  -2.611 0.009035 ** 
#  dr_paytype.xDAILY            -0.97696    0.27090  -3.606 0.000311 ***
#  dr_status                     3.50659    0.10585  33.127  < 2e-16 ***
  ---
#excluding   Distance_Miles
model_5<- glm(formula = churend ~ total_jobs + total_Payment  + total_NonStandard_Payment+totalDeliveriesinOtherStates
                + dr_paytype.xDAILY + dr_status ,
                family = "binomial", data = train) 
summary(model_5)
#Coefficients:
#                                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                    -1.84655    0.06828 -27.045  < 2e-16 ***
#  total_jobs                   -0.40803    0.13062  -3.124 0.001785 ** 
#  total_Payment                -1.26616    0.15714  -8.057  7.8e-16 ***
#  total_NonStandard_Payment     0.23573    0.06884   3.424 0.000616 ***
#  totalDeliveriesinOtherStates -0.21302    0.07759  -2.745 0.006045 ** 
#  dr_paytype.xDAILY            -0.97141    0.27060  -3.590 0.000331 ***
#  dr_status                     3.49920    0.10560  33.137  < 2e-16 ***
vif(model_5)


model_6<- glm(formula = churend ~ total_jobs + total_Payment  + total_NonStandard_Payment
              + dr_paytype.xDAILY + dr_status ,
              family = "binomial", data = train) 
summary(model_6)
#Coefficients:
#                            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                 -1.84048    0.06825 -26.966  < 2e-16 ***
#  total_jobs                -0.55353    0.11916  -4.645 3.39e-06 ***
#  total_Payment             -1.20733    0.15211  -7.937 2.07e-15 ***
#  total_NonStandard_Payment  0.21979    0.06957   3.159  0.00158 ** 
#  dr_paytype.xDAILY         -1.14089    0.26246  -4.347 1.38e-05 ***
#  dr_status                  3.49580    0.10541  33.163  < 2e-16 ***
final_model<-model_6
### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test)
# Let's see the summary 

summary(test_pred)

# Let's use the probability cutoff of 50%.

test_pred_churn <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_churn <- factor(ifelse(test$churend==1,"Yes","No"))

test_conf <- confusionMatrix(test_pred_churn, test_actual_churn, positive = "Yes")
test_conf
#Reference

#Prediction  No Yes
#       No  923 192
#       Yes  66 587

#Accuracy : 0.8541 
#Sensitivity : 0.7548          
#Specificity : 0.9343 
#######################################################################

# Let's find out the optimal probalility cutoff 
# First let's create a function to find the accuracy, sensitivity and specificity
# for a given cutoff
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_churn, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
par(mar=c(2,2,2,2))
#Accuracy, Sensitivity and Specificity for difeerent thresholds
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

#cutoff


##################################################################################################
### Exporting Data for Excel Analysis (KS, Gain, Lift etc.) ######
test_cutoff_churn<- ifelse(test_pred_churn=="Yes",1,0)
test_actual_churn <- ifelse(test_actual_churn=="Yes",1,0)

myeval <- matrix(nrow = length(test_pred),ncol = 2)
myeval[,1] <- test_pred
myeval[,2] <- test_actual_churn
colnames(myeval) <- c("Predicted_Prob","Actual_Labels")
write.csv(myeval,"myeval.csv")


##################################################################################################
##################################################################################################

install.packages("ROCR")
library(ROCR)

pred_object_test<- prediction(test_cutoff_churn, test_actual_churn)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
######### Plot Receiver Operating Characteristics (ROC) Curve #####################

plot(performance_measures_test, type = "b", col = "red", lwd=1.5,
     main = "ROC Curve",
     ylab = "Sensitivity:TPR", 
     xlab = "(1 - Specificity):FPR")
abline(0,1, lty = 8, col = "grey", untf = T)
auc<-performance(pred_object_test,"auc")
auc.value <- unlist(auc@y.values)
text(0.8, 0.23, labels=sprintf("AUC: %0.3f", auc.value)) # 0.842

### KS -statistic - Test Data ######
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.68
# install.packages("InformationValue")
library(InformationValue)

ks_plot(test_actual_churn, test_cutoff_churn) # KS chart plot
####################################################################
# Lift & Gain Chart 

# Loading dplyr package
library(dplyr)
lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_churn, test_pred, groups = 10)
Attrition_decile

Gain <- c(0,Attrition_decile$Gain)
Deciles <- c(0,Attrition_decile$bucket)
#### Plot gain chart ####
plot(y=Gain,x=Deciles,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

Random_Gain <- seq(from=0,to=100,by=10)
lines(y=Random_Gain,x=Deciles,type ="l",lwd = 2, col="red")

Perfect_Gain <- vector(mode = "numeric", length = 11)
for (i in 2:11){Perfect_Gain[i] <- 100*min(1,129*(i-1)/209)}
lines(y=Perfect_Gain,x=Deciles,type ="l",lwd = 2, col="darkgreen")



legend("bottomright",col=c("darkgreen","black","red"),lwd =c(2,2,2,2),c("Perfect Model","Actual Model","Random Model"), cex = 0.7)

# plotting the lift chart
Lift <- Gain/Random_Gain
Random_Lift <- Random_Gain/Random_Gain

plot(y=Lift,x=Deciles,type ="l",ylim=c(0,3.5),lwd = 2,xlab="Bucket",ylab="Lift",main = "Lift Chart",ylim<-c())
lines(y=Random_Lift,x=Deciles,type ="l",lwd = 2, col="red")

legend("topright",col=c("black","red"),lwd =c(2,2,2),c("Actual Model","Random Model"), cex = 0.7)

##Ranking the Drivers (we take cutoff =0.35 from Accuracy Sensitivity Specificity graph).
#as per model payment ,dr_status and category dr_paytype.xDAILY as key variables
#churn_final
alldriver_pred = predict(final_model, type = "response", 
                         newdata = churn_final)
#alldriver_final_pred <- factor(ifelse(alldriver_pred >= 0.35, "Yes", "No"))
ranking_df<-churn_final
ranking_df$Driver_id<-combined_driver_df$Driver_Id
ranking_df$pred_churn<-alldriver_pred
ranking_df<-ranking_df[which(ranking_df$pred_churn>=0.35),]
#ranking_df$driverscore <- ranking_df$total_Payment*0.5+ranking_df$dr_status*0.3+ranking_df$Distance_Miles*0.2
ranking_df<-ranking_df[,c("Driver_id","pred_churn")]
#Descending order
ranking_df<-ranking_df[order(-ranking_df$pred_churn),]
View(ranking_df)