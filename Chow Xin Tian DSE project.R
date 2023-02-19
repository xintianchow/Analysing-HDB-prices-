rm(list = ls()) #clear everything 

#Packages for fitting decision trees:
#install.packages("tree")
#install.packages("rpart")
#install.packages("readxl")
#install.packages("leaps")


library(tree)
library(MASS)
library(rpart)
library(ROCR)
library(readxl)
library(leaps)
library(kknn)
library(corrplot)

#We will import some data using commands
#We set the working directory so we control where R looks for (note the unusual slash syntax):
#setwd("D:/data") #edit this for your desired path and uncomment

mydata = read_excel('HDB_data_2021_sample.xlsx')

##HDB dataset
data(mydata)
str(mydata)
mydata$resale_price = mydata$resale_price/1000
attach(mydata)
df = mydata
head(df)

sum(is.na(df)) ##check missing values
sum(duplicated(df)) ## check duplicated values
summary(df)
#View(df)

#Box plot for resale_prices
boxplot(df$resale_price,
        main = "Box plot of HDB resale prices",
        col = "wheat",
        horizontal = TRUE)
outlier = boxplot.stats(df$resale_price)$out
outlier
outlier_row = which(df$resale_price %in% c(outlier)) #remove outlier
df = df[-outlier_row, ]
attach(df)

#data splitting
ntrain=4692
set.seed(567834)
tr = sample(1:nrow(df),ntrain)  # draw ntrain observations from original data
train = df[tr,]   # Training sample
test = df[-tr,] 

names(df)
View(df)



#Corr plot for amenities
my_data <- df[, c(1,161,165,169,171,173,174,184,190,196,201,205,209,213,214,215,216,217,218,222,226,227)]
head(my_data, 6)
cor_matrix = round(cor(my_data), 2)#corr for distance
corrplot(cor_matrix, type = "upper", order = 'alphabet',
         tl.col = "black", tl.srt = 45, tl.cex = 0.5,   # 'options on text
         method = "number")   # also try method = "color" and "number"

#Corr plot for most influential determinants
my_data2 = df[, c(1,92,93,94,226)]
head(my_data, 6)
cor_matrix2 = round(cor(my_data2), 2)#corr for distance
corrplot(cor_matrix2, type = "upper", order = 'alphabet',
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,   # 'options on text
         method = "number")   # also try method = "color" and "number"




##linear regression
regfit=lm(formula=resale_price~.,data=train)
predlm=predict.lm(regfit, newdata=test)
mean((test$resale_price-predlm)^2)##1877.603
sqrt(mean((test$resale_price-predlm)^2))##43.3312
summary(regfit)
#adjused r sq = 0.9185


#Variable selection

regfit_without_months =lm(formula=resale_price~.-(month_Jan+month_Feb+month_Mar+month_Apr+month_May+month_Jun+month_Jul+month_Aug+month_Sep+month_Oct+month_Nov+month_Dec),data=train)
predlm2=predict.lm(regfit_without_months, newdata=test)
mean((test$resale_price-predlm2)^2)
summary(regfit_without_months)
#adjusted r sq = 0.9176

regfit_without_total_dwelling_units = lm(formula=resale_price~.-(total_dwelling_units),data=train)
predlm3=predict.lm(regfit_without_total_dwelling_units, newdata=test)
mean((test$resale_price-predlm3)^2)
summary(regfit_without_total_dwelling_units)
#adjusted r sq = 0.9185

regfit_Remaining_lease = lm(formula=resale_price~.-(Remaining_lease),data=train)
predlm4=predict.lm(regfit_Remaining_lease, newdata=test)
mean((test$resale_price-predlm4)^2)
summary(regfit_Remaining_lease)
##adjusted r sq = 0.8709

regfit_floor_area_sqm = lm(formula=resale_price~.-(floor_area_sqm),data=train)
predlm5=predict.lm(regfit_floor_area_sqm, newdata=test)
mean((test$resale_price-predlm5)^2)
summary(regfit_floor_area_sqm)
##adjusted r sq = 0.9052

regfit_max_floor_lvl = lm(formula=resale_price~.-(max_floor_lvl),data=train)
predlm6=predict.lm(regfit_max_floor_lvl, newdata=test)
mean((test$resale_price-predlm6)^2)
summary(regfit_max_floor_lvl)
##adjusted r sq = 0.9166

regfit_storey_range = lm(formula=resale_price~.-(storey_range_01.TO.03+storey_range_01.TO.05+storey_range_04.TO.06+storey_range_06.TO.10+storey_range_07.TO.09+storey_range_10.TO.12+storey_range_11.TO.15
                                                 +storey_range_13.TO.15+storey_range_16.TO.18+storey_range_16.TO.20+storey_range_19.TO.21+storey_range_21.TO.25+storey_range_22.TO.24+storey_range_25.TO.27
                                                 +storey_range_26.TO.30+storey_range_28.TO.30+storey_range_31.TO.33+storey_range_31.TO.35+storey_range_34.TO.36+storey_range_36.TO.40+storey_range_37.TO.39
                                                 +storey_range_40.TO.42+storey_range_43.TO.45+storey_range_46.TO.48+storey_range_49.TO.51),data=train)
predlm7=predict.lm(regfit_storey_range, newdata=test)
mean((test$resale_price-predlm7)^2)
summary(regfit_storey_range)
##adjusted r sq = 0.9059

regfit_room_sold= lm(formula=resale_price~.-(X1room_sold+X2room_sold+X3room_sold+X4room_sold+X5room_sold+exec_sold+multigen_sold+studio_apartment_sold),data=train)
predlm8=predict.lm(regfit_room_sold, newdata=test)
mean((test$resale_price-predlm8)^2)
summary(regfit_room_sold)
##adjusted r sq = 0.9183

regfit_room_rental= lm(formula=resale_price~.-(X1room_rental+X2room_rental+X3room_rental+other_room_rental),data=train)
predlm9=predict.lm(regfit_room_rental, newdata=test)
mean((test$resale_price-predlm9)^2)
summary(regfit_room_rental)
##adjusted r sq = 0.9185

regfit_mature= lm(formula=resale_price~.-(mature),data=train)
predlm10=predict.lm(regfit_mature, newdata=test)
mean((test$resale_price-predlm10)^2)
summary(regfit_mature)
##adjusted r sq = 0.9185

regfit_flattype = lm(formula=resale_price~.-(flat_type_1.ROOM+flat_type_2.ROOM+flat_type_3.ROOM+flat_type_4.ROOM+flat_type_5.ROOM+flat_type_EXECUTIVE+flat_type_MULTI.GENERATION),data=train)
predlm11=predict.lm(regfit_flattype, newdata=test)
mean((test$resale_price-predlm11)^2)
summary(regfit_flattype)
##adjusted r sq = 0.9171

regfit_flatmodel = lm(formula=resale_price~.-(flat_model_2.room+flat_model_adjoined.flat+flat_model_apartment+flat_model_dbss+flat_model_improved+flat_model_improved.maisonette+flat_model_maisonette+flat_model_model.a+flat_model_model.a.maisonette+flat_model_model.a2+flat_model_multi.generation+flat_model_new.generation+flat_model_premium.apartment+flat_model_premium.apartment.loft+flat_model_premium.maisonette+flat_model_simplified+flat_model_standard+flat_model_terrace+flat_model_type.s1+flat_model_type.s2),data=train)
predlm12=predict.lm(regfit_flatmodel, newdata=test)
mean((test$resale_price-predlm12)^2)
summary(regfit_flatmodel)
#adjusted r sq = 0.9021

regfit_hawker = lm(formula=resale_price~.-(Dist_nearest_GHawker+nearest_ghawker_no_of_cooked_food_stalls+nearest_ghawker_no_of_mkt_produce_stalls+nearest_ghawker_no_of_stalls),data=train)
predlm13=predict.lm(regfit_hawker, newdata=test)
mean((test$resale_price-predlm13)^2)
summary(regfit_hawker)
##adjusted r sq = 0.9177

regfit_mall = lm(formula=resale_price~.-(Dist_nearest_mall+no_malls_0.5km+no_malls_1km+no_malls_2km),data=train)
predlm14=predict.lm(regfit_mall, newdata=test)
mean((test$resale_price-predlm14)^2)
summary(regfit_mall)
##adjusted r sq = 0.9179

regfit_beach = lm(formula=resale_price~.-(Dist_nearest_beach
                                          +beach_within_2km),data=train)
predlm15=predict.lm(regfit_beach, newdata=test)
mean((test$resale_price-predlm15)^2)
summary(regfit_beach)
##adjusted r sq = 0.918

regfit_waterbody = lm(formula=resale_price~.-(Dist_nearest_waterbody+waterbody_within_2km),data=train)
predlm16=predict.lm(regfit_waterbody, newdata=test)
mean((test$resale_price-predlm16)^2)
summary(regfit_waterbody)
##adjusted r sq = 0.9183

regfit_CC= lm(formula=resale_price~.-(Dist_nearest_CC),data=train)
predlm17=predict.lm(regfit_CC, newdata=test)
mean((test$resale_price-predlm17)^2)
summary(regfit_CC)
##adjusted r sq= 0.9175

regfit_mrt= lm(formula=resale_price~.-(Dist_nearest_station+NSL+EWL+NEL+CCL+DTL+TEL+LRT+unique_no_mrt_0.5km+unique_no_mrt_1km),data=train)
predlm20=predict.lm(regfit_mrt, newdata=test)
mean((test$resale_price-predlm20)^2)
summary(regfit_mrt)
##adjusted r sq = 0.906

regfit_sch= lm(formula=resale_price~.-(Dist_nearest_primary_school+Nearest_primary_school_gender_BOYS..SCHOOL+Nearest_primary_school_gender_CO.ED.SCHOOL+Nearest_primary_school_gender_GIRLS..SCHOOL+no_primary_schools_1km+no_primary_schools_2km+Dist_nearest_GAI_primary_school+Nearest_GAI_primary_school_gender_BOYS..SCHOOL+Nearest_GAI_primary_school_gender_CO.ED.SCHOOL+Nearest_GAI_primary_school_gender_GIRLS..SCHOOL+no_GAI_primary_schools_1km+no_GAI_primary_schools_2km+Dist_nearest_G_primary_school+Nearest_G_primary_school_gender_CO.ED.SCHOOL+Nearest_G_primary_school_gender_GIRLS..SCHOOL+no_G_primary_schools_1km+no_G_primary_schools_2km+Dist_nearest_secondary_school
                                       +Nearest_secondary_school_gender_BOYS..SCHOOL+Nearest_secondary_school_gender_CO.ED.SCHOOL+Nearest_secondary_school_gender_GIRLS..SCHOOL+Dist_nearest_GAI_secondary_school+Nearest_GAI_secondary_school_gender_BOYS..SCHOOL+Nearest_GAI_secondary_school_gender_CO.ED.SCHOOL+Nearest_GAI_secondary_school_gender_GIRLS..SCHOOL+Dist_nearest_G_secondary_school+Nearest_G_secondary_school_gender_BOYS..SCHOOL+Nearest_G_secondary_school_gender_CO.ED.SCHOOL+Nearest_G_secondary_school_gender_GIRLS..SCHOOL+Dist_nearest_jc+Dist_nearest_GAI_jc+Dist_nearest_G_jc+Dist_nearest_polytechnic+Dist_nearest_university),data=train)
predlm21=predict.lm(regfit_sch, newdata=test)
mean((test$resale_price-predlm21)^2)
summary(regfit_sch)
##adjusted r sq = 0.9146

regfit_hospital= lm(formula=resale_price~.-(Dist_nearest_hospital+Nearest_hospital_ownership_Non.profit+Nearest_hospital_ownership_Private+Nearest_hospital_ownership_Public+Dist_nearest_A_hospital+Nearest_A_hospital_ownership_Non.profit+Nearest_A_hospital_ownership_Private+Nearest_A_hospital_ownership_Public),data=train)
predlm21=predict.lm(regfit_hospital, newdata=test)
mean((test$resale_price-predlm21)^2)
summary(regfit_hospital)
#adjusted r sq = 0.9184

regfit_CBD= lm(formula=resale_price~.-(Dist_CBD),data=train)
predlm22=predict.lm(regfit_CBD, newdata=test)
mean((test$resale_price-predlm22)^2)
summary(regfit_CBD)
#adjusted r sq = 0.918


regfit_ADF= lm(formula=resale_price~.-(Dist_nearest_ADF+ADF_within_0.5km+ADF_within_1km+ADF_within_2km),data=train)
predlm23=predict.lm(regfit_ADF, newdata=test)
mean((test$resale_price-predlm23)^2)
summary(regfit_ADF)
#adjusted r sq = 0.9173


#postal code
regfit_postalcode= lm(formula=resale_price~.-(postal_2digits_05+postal_2digits_08+postal_2digits_09+postal_2digits_10+postal_2digits_12+postal_2digits_13+postal_2digits_14+postal_2digits_15+postal_2digits_16+postal_2digits_18+postal_2digits_19+postal_2digits_20 + postal_2digits_21 + postal_2digits_26  + postal_2digits_27  + postal_2digits_30  + postal_2digits_31 + postal_2digits_32 
                                              + postal_2digits_33 + postal_2digits_35 +postal_2digits_36   + postal_2digits_37  +postal_2digits_38  + postal_2digits_39                                  
                                              
                                              
                                              +postal_2digits_40 +postal_2digits_41 + postal_2digits_42 + postal_2digits_43  +postal_2digits_44 + postal_2digits_46                                      
                                              
                                              
                                              +postal_2digits_47     +postal_2digits_50 + postal_2digits_51 + postal_2digits_52 + postal_2digits_53 + postal_2digits_54
                                              
                                              
                                              +postal_2digits_55   +postal_2digits_56 + postal_2digits_57 +postal_2digits_59 + postal_2digits_60  + postal_2digits_61                                   
                                              
                                              
                                              +postal_2digits_64 + postal_2digits_65  +postal_2digits_67 + postal_2digits_68  + postal_2digits_73  + postal_2digits_75                                  
                                              
                                              +postal_2digits_76  + postal_2digits_79 + postal_2digits_82 ),data=train)
predlm24=predict.lm(regfit_postalcode, newdata=test)
mean((test$resale_price-predlm24)^2)
summary(regfit_postalcode)
#adjusted r sq = 0.914


#town
regfit_town= lm(formula=resale_price~.-(town_ANG.MO.KIO + town_BEDOK + town_BISHAN + town_BUKIT.BATOK + town_BUKIT.MERAH + town_BUKIT.PANJANG  
                                        +town_BUKIT.TIMAH + town_CENTRAL.AREA + town_CHOA.CHU.KANG + town_CLEMENTI + town_GEYLANG + town_HOUGANG                                      
                                        +town_JURONG.EAST + town_JURONG.WEST + town_KALLANG.WHAMPOA + town_MARINE.PARADE + town_PASIR.RIS + town_PUNGGOL+ town_QUEENSTOWN + town_SEMBAWANG + town_SENGKANG + town_SERANGOON + town_TAMPINES + town_TOA.PAYOH + town_WOODLANDS + town_YISHUN),data=train)
predlm25=predict.lm(regfit_town, newdata=test)
mean((test$resale_price-predlm25)^2)
summary(regfit_town)
#adjusted r sq = 0.918


#linear regression base model
regfit3= lm(formula=resale_price~Remaining_lease + floor_area_sqm + max_floor_lvl + storey_range_01.TO.03+storey_range_01.TO.05+storey_range_04.TO.06+storey_range_06.TO.10+storey_range_07.TO.09+storey_range_10.TO.12+storey_range_11.TO.15
                                    +storey_range_13.TO.15+storey_range_16.TO.18+storey_range_16.TO.20+storey_range_19.TO.21+storey_range_21.TO.25+storey_range_22.TO.24+storey_range_25.TO.27
                                    +storey_range_26.TO.30+storey_range_28.TO.30+storey_range_31.TO.33+storey_range_31.TO.35+storey_range_34.TO.36+storey_range_36.TO.40+storey_range_37.TO.39
                                    +storey_range_40.TO.42+storey_range_43.TO.45+storey_range_46.TO.48+storey_range_49.TO.51 + flat_model_2.room+flat_model_adjoined.flat+flat_model_apartment+flat_model_dbss+flat_model_improved+flat_model_improved.maisonette
                                    +flat_model_maisonette+flat_model_model.a+flat_model_model.a.maisonette+flat_model_model.a2+flat_model_multi.generation+flat_model_new.generation+flat_model_premium.apartment+flat_model_premium.apartment.loft+flat_model_premium.maisonette+flat_model_simplified
                                    +flat_model_standard+flat_model_terrace+flat_model_type.s1+flat_model_type.s2 + Dist_CBD + postal_2digits_05+postal_2digits_08+postal_2digits_09+postal_2digits_10+postal_2digits_12+postal_2digits_13+postal_2digits_14+postal_2digits_15+postal_2digits_16+postal_2digits_18+postal_2digits_19+postal_2digits_20 + postal_2digits_21 + postal_2digits_26  + postal_2digits_27  + postal_2digits_30  + postal_2digits_31 + postal_2digits_32 
                                    + postal_2digits_33 + postal_2digits_35 +postal_2digits_36   + postal_2digits_37  +postal_2digits_38  + postal_2digits_39                                  
                                    
                                    
                                    +postal_2digits_40 +postal_2digits_41 + postal_2digits_42 + postal_2digits_43  +postal_2digits_44 + postal_2digits_46                                      
                                    
                                    
                                    +postal_2digits_47     +postal_2digits_50 + postal_2digits_51 + postal_2digits_52 + postal_2digits_53 + postal_2digits_54
                                    
                                    
                                    +postal_2digits_55   +postal_2digits_56 + postal_2digits_57 +postal_2digits_59 + postal_2digits_60  + postal_2digits_61                                   
                                    
                                    
                                    +postal_2digits_64 + postal_2digits_65  +postal_2digits_67 + postal_2digits_68  + postal_2digits_73  + postal_2digits_75                                  
                                    
                                    +postal_2digits_76  + postal_2digits_79 + postal_2digits_82,data=train) #postalcode, remaining lease, floor area, max floor level , storey range, mature, dist cbd, flat model
predlm33=predict.lm(regfit3, newdata=test)
mean((test$resale_price-predlm33)^2)##2556.294
sqrt(mean((test$resale_price-predlm33)^2))##50.5598
summary(regfit3)


##Tree with all variables

big.tree = rpart(resale_price~.,method="anova",data=train, minsplit=5, cp = .0005)
length(unique(big.tree$where))

#We now examine the CV plot, invoked by plotcp() function on the fitted tree:
#par(mfrow=c(1,1)) #back to one graph per window
plotcp(big.tree) #CV plot
bestcp=big.tree$cptable[which.min(big.tree$cptable[,"xerror"]),"CP"] #extract best cp value

#Finally, let's replot the best tree using the Fancy option:
windows()
best.tree = prune(big.tree,cp=bestcp) #get tree for best cp on CV
plot(best.tree,uniform=TRUE)
text(best.tree,digits=4,use.n=TRUE,fancy=FALSE,bg='lightblue') #note fancy=TRUE option would give a "nicer" look (see end of file for even nicer looking plot using rpart.plot)

treefit=predict(best.tree,newdata=test,type="vector") #prediction on test data
mean((test$resale_price-treefit)^2) #2329.977
sqrt(mean((test$resale_price-treefit)^2))#48.32698
dev.off()

#KNN 

boscv=train.kknn(resale_price~Remaining_lease + floor_area_sqm + max_floor_lvl + Dist_CBD ,data=test,kmax=100, kernel = "rectangular")

plot((1:100),boscv$MEAN.SQU, type="l", col = "blue", main="LOOCV MSE")
kbest=boscv$best.parameters$k
kbest#4

knnreg = kknn(resale_price~Remaining_lease + floor_area_sqm + max_floor_lvl + Dist_CBD,train,test,k=kbest,kernel = "rectangular")
#plot((1:kbest),knnreg$MEAN.SQU, type="l", col = "blue", main="LOOCV MSE")
knnmse=mean((test$resale_price-knnreg$fitted.values)^2)
knnmse#1987
sqrt(knnmse)#44.57593


#linear regression base model without floor area sqm
regfit4= lm(formula=resale_price~Remaining_lease + max_floor_lvl + storey_range_01.TO.03+storey_range_01.TO.05+storey_range_04.TO.06+storey_range_06.TO.10+storey_range_07.TO.09+storey_range_10.TO.12+storey_range_11.TO.15
            +storey_range_13.TO.15+storey_range_16.TO.18+storey_range_16.TO.20+storey_range_19.TO.21+storey_range_21.TO.25+storey_range_22.TO.24+storey_range_25.TO.27
            +storey_range_26.TO.30+storey_range_28.TO.30+storey_range_31.TO.33+storey_range_31.TO.35+storey_range_34.TO.36+storey_range_36.TO.40+storey_range_37.TO.39
            +storey_range_40.TO.42+storey_range_43.TO.45+storey_range_46.TO.48+storey_range_49.TO.51 + flat_model_2.room+flat_model_adjoined.flat+flat_model_apartment+flat_model_dbss+flat_model_improved+flat_model_improved.maisonette
            +flat_model_maisonette+flat_model_model.a+flat_model_model.a.maisonette+flat_model_model.a2+flat_model_multi.generation+flat_model_new.generation+flat_model_premium.apartment+flat_model_premium.apartment.loft+flat_model_premium.maisonette+flat_model_simplified
            +flat_model_standard+flat_model_terrace+flat_model_type.s1+flat_model_type.s2 + Dist_CBD + postal_2digits_05+postal_2digits_08+postal_2digits_09+postal_2digits_10+postal_2digits_12+postal_2digits_13+postal_2digits_14+postal_2digits_15+postal_2digits_16+postal_2digits_18+postal_2digits_19+postal_2digits_20 + postal_2digits_21 + postal_2digits_26  + postal_2digits_27  + postal_2digits_30  + postal_2digits_31 + postal_2digits_32 
            + postal_2digits_33 + postal_2digits_35 +postal_2digits_36   + postal_2digits_37  +postal_2digits_38  + postal_2digits_39                                  
            
            
            +postal_2digits_40 +postal_2digits_41 + postal_2digits_42 + postal_2digits_43  +postal_2digits_44 + postal_2digits_46                                      
            
            
            +postal_2digits_47     +postal_2digits_50 + postal_2digits_51 + postal_2digits_52 + postal_2digits_53 + postal_2digits_54
            
            
            +postal_2digits_55   +postal_2digits_56 + postal_2digits_57 +postal_2digits_59 + postal_2digits_60  + postal_2digits_61                                   
            
            
            +postal_2digits_64 + postal_2digits_65  +postal_2digits_67 + postal_2digits_68  + postal_2digits_73  + postal_2digits_75                                  
            
            +postal_2digits_76  + postal_2digits_79 + postal_2digits_82,data=train) #postalcode, remaining lease, floor area, max floor level , storey range, mature, dist cbd, flat model
predlm34=predict.lm(regfit4, newdata=test)
mean((test$resale_price-predlm34)^2)##8015.269
sqrt(mean((test$resale_price-predlm34)^2))##89.52803
summary(regfit4)

################################

##Tree with no floor area sqm

##################################

big.tree2 = rpart(resale_price~.-(floor_area_sqm),method="anova",data=train, minsplit=5,cp=.0005)
length(unique(big.tree2$where))

#We now examine the CV plot, invoked by plotcp() function on the fitted tree:
#par(mfrow=c(1,1)) #back to one graph per window
plotcp(big.tree2) #CV plot
bestcp2=big.tree2$cptable[which.min(big.tree2$cptable[,"xerror"]),"CP"] #extract best cp value

#Finally, let's replot the best tree using the Fancy option:
windows()
best.tree2 = prune(big.tree2,cp=bestcp2) #get tree for best cp on CV
plot(best.tree2,uniform=TRUE)
text(best.tree2,digits=4,use.n=TRUE,fancy=FALSE,bg='lightblue') #note fancy=TRUE option would give a "nicer" look (see end of file for even nicer looking plot using rpart.plot)

treefit2=predict(best.tree2,newdata=test,type="vector") #prediction on test data
mean((test$resale_price-treefit2)^2)#2632.147
sqrt(mean((test$resale_price-treefit2)^2))#51.30
dev.off()

##################################

#KNN without floor area sqm 

#####################################
boscv2=train.kknn(resale_price~Remaining_lease + max_floor_lvl + Dist_CBD ,data=test,kmax=100, kernel = "rectangular")

plot((1:100),boscv2$MEAN.SQU, type="l", col = "blue", main="LOOCV MSE")
kbest=boscv2$best.parameters$k
kbest#7

knnreg2 = kknn(resale_price~Remaining_lease + max_floor_lvl + Dist_CBD,train,test,k=kbest,kernel = "rectangular")
#plot((1:kbest),knnreg2$MEAN.SQU, type="l", col = "blue", main="LOOCV MSE")
knnmse2=mean((test$resale_price-knnreg2$fitted.values)^2)
knnmse2#9000.258
sqrt(knnmse2)#94.869







