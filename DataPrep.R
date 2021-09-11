# Before data modeling/analysis, data must be cleaned/prepped.
# Common data preparation includes: data set consolidation, missing value treatment 
# and outlier analysis.

# Main ways to consolidate datasets are: Append and Merge

# Append: concatenate or stack datasets by row or column
# For example: Two datasets from car dealership (column names should be the same)
# car1 and car2 datasets

# Import data 
# Import car1 dataset
car_data1 <- read.csv(file.choose())
View(car_data1)

# Import car2 datasest
car_data2 <- read.csv(file.choose())
View(car_data2)

# Append datasets by row (stack the two datasets)
append_stack <- rbind(car_data1, car_data2) # rbind: row bind
View(append_stack)

# Append datasets by column (side by side)
append_sidebyside <- cbind(car_data1, car_data2) # cbind: column bind
View(append_sidebyside)

# Rename the Columns
colnames(append_sidebyside) <- c('CarName1', 
                                 'Sales1000s', 
                                 'CarName2', 
                                 'Sales1000s') # colnames: change column names
View(append_sidebyside) 

# Can export the dataset using write.csv command
write.csv(append_sidebyside, "/Users/cinsbox/Desktop/append_sidebyside")

# Merge: combine datasets with common values 
# Let's import another dataset, car3 dataset
car_data3 <- read.csv(file.choose())
View(car_data3)

# Simple merge 
merge_car1_car3 <- merge(car_data1,car_data3,by="CarName")
View(merge_car1_car3)

# Inner, Outer, Left and Right Joins (Think SQL)
# Import 2 new datasets: store1 and store 2

# Import store1 dataset
data_store1 <- read.csv(file.choose())
View (data_store1)

# Import store2 dataset
data_store2 <- read.csv(file.choose())
View(data_store2)

# Inner Join: intersection of two datasets (common information)
inner_store1_2 <- merge(x=data_store1,y=data_store2,by.x = "Cust_ID")
View(inner_store1_2)

# Outer Join: union of two datasets (all information)
outer_store1_2 <- merge(x=data_store1,y=data_store2,by="Cust_ID",all=T) # all = T for merge all
View(outer_store1_2)



# Left Join: take all of the store1 data that is in store2, store1 is left table
left_store1_2 <- merge(x=data_store1,y=data_store2,by="Cust_ID",all.x=T) # all from left or x table
View(left_store1_2)

# Right Join: take all of the store2 data that is in store 1, store 2 is right table
right_store1_2 <- merge(x=data_store1,y=data_store2,by="Cust_ID",all.y =T) 
View(right_store1_2)


# Missing Value(s): empty cell(s) or observation(s)
# Either delete them or replace with mean, medium or mode

# Import Car_sales dataset 
car_sales <- read.csv(file.choose())
View(car_sales)

# Check missing values
# sapply: loop through data in frame
# sum: count the # of missing (na) values
# na: missing values
sapply(car_sales, function(x) sum(is.na(x)))

# Rid of missing values #1: get rid of missing values (5% to 10% ok)
car_sales_new <- na.omit(car_sales) # omit: get rid of missing values
View(car_sales_new)
sapply(car_sales_new, function(x) sum(is.na(x)))

# Rid of missing values #2: rid of missing values by replacing with mean, median or mode
# Rid of missing values by mean

# Import house1 dataset
data_house1 <- read.csv(file.choose())
View(data_house1)

# Replace N/A with mean 
# To refer to a variable inside a table, use "TableName"$"VariableName" notation
# is na: denotes to look for N/A in the variable column
# mean is built-in function
# na.rm = TRUE is to replace all cells with N/A with mean
data_house1$sale_value_hundtho[is.na(data_house1$sale_value_hundtho)]<- mean((data_house1$sale_value_hundtho),na.rm=T)
View(data_house1)

# Import house1 dataset
data_house2 <- read.csv(file.choose())
View(data_house2)

# Replace with median
# is na: denotes to look for N/A in the variable column
# median is built-in function
# na.rm = TRUE is to replace all cells with N/A with median
data_house2$sale_value_hundtho[is.na(data_house2$sale_value_hundtho)]<- median((data_house2$sale_value_hundtho),na.rm=T)
View(data_house2)

# Replace with mode
# Mode can be used to do numberical or categorical variable replacement
# Example here uses categorical variable as the demonstration

# Import house2 dataset
data_house2 <- read.csv(file.choose())
View(data_house2)

# Create the function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
# Compute the mode using the function
v <- data_house2$`house_type`
mode <- getmode(v)
mode
# Replace the N/A in the dataset with mode
data_house2$`house_type`[is.na(data_house2$`house_type`)] <- mode
View(data_house2)

# Outlier Analysis
# Outlier: data point that differs significantly from others

# Import outliers1 dataset
data_outliers1 <- read.csv(file.choose())
View(data_outliers1)

# Boxplot using y variable
boxplot(data_outliers1$y)

# Inter-quartile (IQR)
summary(data_outliers1$y)

# Bench calculation 
bench <- 14.5 + 1.5*IQR(data_outliers1$y) 
bench

# Can either remove the outliers or replace the outliers with bench
# To replace with the bench
data_outliers1$y[data_outliers1$y > bench] <- bench
View(data_outliers1)






