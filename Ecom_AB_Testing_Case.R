#set up the environment & imported files
library(tidyverse)
test <- read_csv("Ecom_Test_Table.csv")
user <- read.csv("Ecom_User_table.csv")

#explored dataset. made sure all users who participated the test had users info.
head(test)
head(user)
test_result <- test %>% left_join(user,by="user_id")
test_result <-test_result %>% rename(membership_level=service)
head(test_result)
summary(test_result)
length(unique(test$user_id))
length(unique(test_result$user_id))
test_result %>% filter_all(any_vars(is.na(.)))
test_result$date <- as.Date(test_result$date)
test_result$test <- as.factor(test_result$test)


#checked the difference between control group and test group
test_result %>% group_by(test) %>% summarize(mean_purchase_amount=mean(purchase_amount))

#checked the difference by devices
test_result %>% group_by(device) %>% summarize(mean_purchase_amount=mean(purchase_amount))

#checked the difference by gender
test_result %>% group_by(gender) %>% summarize(mean_purchase_amount=mean(purchase_amount))

#checked the difference by country
test_result %>% group_by(country) %>% summarize(mean_purchase_amount=mean(purchase_amount))

#checked the difference by membership_level
test_result %>% group_by(membership_level) %>% summarize(mean_purchase_amount=mean(purchase_amount))

#checked the difference by test group & country
test_result %>% group_by(country,test) %>% summarize(mean_purchase_amount=mean(purchase_amount))

#conducted the t test. 
#assumed test group and control group both follow normal distribution, 
#Xtest ∼ N(μ1,σ21),  Xcontrol ∼ N(μ2,σ22)
#H0:μ1−μ2≤0 vs H1:μ1−μ2>0
#concluded that test group's purchase amount was significantly different from the control group's.
t.test(test_result[test_result$test==1,]$purchase_amount,test_result[test_result$test==0,]$purchase_amount,alternative="greater")

#conducted the AOV analysis to find out which factors had impact on the purchase amount.
#concluded that country, test, device,membership_level factors mattered.
aov_model <- aov(purchase_amount~ country + test+ device+gender+membership_level, test_result)
summary(aov_model)

#conducted the further AOV analysis to check if test vs other factors had impact on each others.
#found that test * country mattered.
interaction_model <- aov(purchase_amount~ country*test + device*test +membership_level*test, test_result)
summary(interaction_model)

#final aov model
final_model <- aov(purchase_amount~ country*test + device +membership_level, test_result)
summary(final_model)

#conducted the Tukey Test to find out the purchase_amount difference beteen test group and control group
TukeyHSD(final_model,"test")
TukeyHSD(final_model,"country")

#data visualization 
test_result_by_date <- test_result %>% group_by(date,test) %>% summarise(mean_purchase_amount=mean(purchase_amount))
test_result_by_date %>% ggplot(aes(date,mean_purchase_amount,color=test))+geom_point()+geom_line()+labs(title="Purchase amount by test group")+theme_bw()

test_result %>% ggplot(aes(purchase_amount,fill=test,color=test))+geom_density(alpha=0.3)+labs(title="Purchase Amount Density by test group")+theme_bw()

test_result %>% ggplot(aes(country,purchase_amount,color=test))+geom_boxplot()+labs(title="Purchase Amount Density by test group & country")+theme_bw()
test_result %>% ggplot(aes(device,purchase_amount,color=test))+geom_boxplot()+labs(title="Purchase Amount Density by test group & device")+theme_bw()
test_result %>% ggplot(aes(membership_level,purchase_amount,color=test))+geom_boxplot()+labs(title="Purchase Amount Density by test group & membership_level")+theme_bw()


