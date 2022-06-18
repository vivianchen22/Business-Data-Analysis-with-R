#setup environment & import data
library(tidyverse)
sales <- read_csv("Food_Company_Sales_Table.csv")
product <- read_csv("Food_Company_Product_Table.csv")
client <- read_csv("Food_Company_Client_Table.csv")

#inner join sales & product & client Tables
Sales_Data <- sales %>% inner_join(product, by="Product_ID") %>% inner_join(client,by="Client_ID")

#explore sales data & rename column & transform datatype & add price column
summary(Sales_Data)
Sales_Data <- Sales_Data %>% rename(Qty=Sales_Amount)
Sales_Data$Agency <- as.factor(Sales_Data$Agency)
Sales_Data$Product_ID <- as.factor(Sales_Data$Product_ID)
Sales_Data$Client_ID <- as.factor(Sales_Data$Client_ID)
Sales_Data <- Sales_Data %>% mutate(Price = Sales/Qty)

#Observe the relation between Qty & Price
#found that more products' price were below 200 & qty were below 500
Sales_Data %>% ggplot(aes(Price,Qty)) + geom_point(color="Orange",alpha=0.2) + labs(title="Price vs QTY")+theme_bw()

#Observe sales distribution by clients 
#found that LL's median was higher than others. FF,GG,KK performed better, too.
Sales_Data %>% ggplot(aes(factor(Client_Name),Sales)) + geom_boxplot(aes(color=Client_Name))+ labs(x="Client Name",title="Sales Distribution of Each Client")+theme_bw()

#Observe sales ranking by clients
#found that CC contributed the most. AA was the next. LL acutually didn't contirbuted a lot.
Sales_by_Client <-Sales_Data %>% group_by(Client_Name)%>%summarize(Total_Sales=sum(Sales))%>%arrange(-Total_Sales)
Sales_by_Client %>% ggplot(aes(Client_Name,Total_Sales,fill=Client_Name)) + geom_bar(stat='identity')+labs(title="Total Sales by Client") +theme_bw()

#Observe sales distribution by products
#found that sales from H varied a lot. and H's median was higher. 
Sales_Data %>% ggplot(aes(Product_Name,Sales))+geom_boxplot(aes(color=Product_Name))+labs(title="Sales Distribution of Each Product")+theme_bw()

#Observe sales ranking by products
#found that H contributed the most. and followed by D and P.
Sales_by_Product <- Sales_Data %>% group_by(Product_Name) %>% summarize(Total_Sales = sum(Sales))%>%arrange(-Total_Sales)
Sales_by_Product %>% ggplot(aes(Product_Name,Total_Sales,fill=Product_Name))+geom_bar(stat='identity')+labs(title="Total Sales by Product")+theme_bw()

#Observe sales ranking by clients & by products
Sales_by_Client_and_Product <-Sales_Data %>% group_by(Client_Name,Product_Name) %>% summarize(Total_Sales=sum(Sales))%>%arrange(-Total_Sales)
Sales_by_Client_and_Product %>% ggplot(aes(Product_Name,Total_Sales,fill=Product_Name))+geom_bar(stat='identity')+facet_wrap(~Client_Name)+labs(title="Total Sales by Client and Product")+theme_bw()

#Create the geom_rec plot
#(1) reformat the Sales_by_Client_and_Product plot.longdata to widedat 
Sales_Percentage <- Sales_by_Client_and_Product %>% spread(key=Product_Name,value=Total_Sales,fill=0)
#(2)calculate x percentage by client name
x_percentage <-c()
for (i in 1:nrow(Sales_Percentage)){
  x_percentage[i]<-sum(Sales_Percentage[i,2:ncol(Sales_Percentage)])/sum(Sales_Percentage[,2:ncol(Sales_Percentage)])
}
Sales_Percentage %>% mutate(x_max=cumsum(x_percentage)) 
Sales_Percentage$x_max <-cumsum(x_percentage)
Sales_Percentage$x_min <-cumsum(x_percentage)-x_percentage

#(3)reformat Sales_Percentage Table
Sales_Percentage <- Sales_Percentage %>% gather(key=Product_Name,value = Total_Sales,-c(Client_Name,x_max,x_min))
Sales_Percentage <-Sales_Percentage %>% group_by(Client_Name) %>% mutate(y_max=round(cumsum(Total_Sales)/sum(Total_Sales)*100)) %>% mutate(y_min=round(y_max-Total_Sales/sum(Total_Sales)*100))
#(4)calculate x_text&y_text position
Sales_Percentage <-Sales_Percentage %>% mutate(x_text=x_min+(x_max-x_min)/2,y_text=y_min+(y_max-y_min)/2)
#(5)build the chart
Sales_Percentage %>% ggplot(aes(xmin=x_min,xmax=x_max,ymin=y_min,ymax=y_max,fill=Product_Name))+geom_rect()+geom_text(aes(x=x_text,y=0,label=Client_Name))+geom_text(aes(x=x_text,y=y_text,label=ifelse(y_max-y_min!=0,paste(y_max-y_min,"%"),paste("")),size=1))+labs(x="Client_Name",y="Product_Name",title="Analysis by Client & Product")+theme_bw()
                                                                                                                      


