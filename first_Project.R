# Importing related libraries
library(dplyr)
library(tidyverse)
library(lubridate)

# Importing dataset
olist_customers_dataset = read.csv("C:/Users/ÎÇáÏ/Desktop/Python File/Coding Dojo/R/chance week/brazilan e-commerce/olist_customers_dataset.csv")
olist_geolocation_dataset = read.csv("C:/Users/ÎÇáÏ/Desktop/Python File/Coding Dojo/R/chance week/brazilan e-commerce/olist_geolocation_dataset.csv")
olist_order_items_dataset = read.csv("C:/Users/ÎÇáÏ/Desktop/Python File/Coding Dojo/R/chance week/brazilan e-commerce/olist_order_items_dataset.csv")
olist_order_payments_dataset= read.csv("C:/Users/ÎÇáÏ/Desktop/Python File/Coding Dojo/R/chance week/brazilan e-commerce/olist_order_payments_dataset.csv")
olist_order_reviews_dataset= read.csv("C:/Users/ÎÇáÏ/Desktop/Python File/Coding Dojo/R/chance week/brazilan e-commerce/olist_order_reviews_dataset.csv")
olist_orders_dataset=read.csv("C:/Users/ÎÇáÏ/Desktop/Python File/Coding Dojo/R/chance week/brazilan e-commerce/olist_orders_dataset.csv")
olist_products_dataset=read.csv("C:/Users/ÎÇáÏ/Desktop/Python File/Coding Dojo/R/chance week/brazilan e-commerce/olist_products_dataset.csv")
olist_sellers_dataset= read.csv("C:/Users/ÎÇáÏ/Desktop/Python File/Coding Dojo/R/chance week/brazilan e-commerce/olist_sellers_dataset.csv")
product_category_name_translation = read.csv("C:/Users/ÎÇáÏ/Desktop/Python File/Coding Dojo/R/chance week/brazilan e-commerce/product_category_name_translation.csv")

# Deleting Null values from 'order_approved_at' & 'order_delivered_carried_date'
#columns 
olist_orders_dataset=subset( olist_orders_dataset,order_approved_at>''& order_delivered_carrier_date>'')

# Extracting orders with invalid date: These orders have illogical date which the
#delivered customer date is more than the order delivered carrier date, that 
#means the customer received the shipment before it has been shipped!
wrong_shipping_data=olist_orders_dataset[olist_orders_dataset$order_delivered_customer_date<olist_orders_dataset$order_delivered_carrier_date 
                                         & olist_orders_dataset$order_delivered_customer_date>"",]
# Exploring the wrong orders
View(wrong_shipping_data)

# The dataset after wrong shipping date being removed
olist_orders_dataset_corrected= (olist_orders_dataset
                                 [!(olist_orders_dataset$order_id) %in% 
                                     (wrong_shipping_data$order_id),])
# Exploring the dataset
View(olist_orders_dataset_corrected)

# Excluding Null values from the 'delivered_customer_date' column in order to
#measure the median of the shipping time to fill them into null values again
olist_orders_dataset_without_null= olist_orders_dataset_corrected[olist_orders_dataset_corrected$order_delivered_customer_date>'',]
df=olist_orders_dataset_without_null

# Calculating the median of the shipping time in Seconds
diffirent_time_in_second_after_correction=
  median(as.POSIXct(df$order_delivered_customer_date)-
           as.POSIXct(df$order_delivered_carrier_date))
diffirent_time_in_second_after_correction
# Calculating the median of the shipping time in Minutes
m_after_correction=diffirent_time_in_second_after_correction/60
# Calculating the median of the shipping time in Minutes with no string
m_after_correction=unlist(m_after_correction,'\\s+')[[1]]
m_after_correction
# Calculating the median of the shipping time in Hours
h_after_correction=m_after_correction/60
h_after_correction
# Calculating the median of the shipping time in Days to use it in replacing
#Null values at the column 
d_after_correction=(round(h_after_correction/24))
d_after_correction

# Finding the maximum shipping time in Seconds
max_time_after_correction=max(as.POSIXct(df$order_delivered_customer_date)-
                                as.POSIXct(df$order_delivered_carrier_date))
max_time_after_correction
# Finding the minimum shipping time in Seconds, this should be (0), otherwise
#it is a wrong value
min_time_after_correction=min(as.POSIXct(df$order_delivered_customer_date)-
                                as.POSIXct(df$order_delivered_carrier_date))
min_time_after_correction

# Filling the Null values in the 'order_delivered_customer_date' column with the
#suitable value, the method is that we measured the median of the shipping time
#value and then add it to the shipping date and time to anticipate the receiving
#date and time

for (i in 1:length(olist_orders_dataset_corrected$order_id)){
  if (olist_orders_dataset_corrected[i,7]==''){
    olist_orders_dataset_corrected[i,7]= 
      as.character.Date(ymd_hms(olist_orders_dataset_corrected[i,6])+days(d_after_correction))
  }
}

# Exploring the final cleaned dataset
View(olist_orders_dataset_corrected)


# Fixing olist_order_reviews_dataset

# In this dataset we have numbers of Null values in both 'review_comment_title'
#and 'review_comment_message' columns

# Filling Null values in both columns
olist_order_reviews_dataset$
  review_comment_title[olist_order_reviews_dataset$review_comment_title=='' 
                       | olist_order_reviews_dataset$review_comment_title==' ' ]= 'comment without title'
olist_order_reviews_dataset$
  review_comment_message[olist_order_reviews_dataset$review_comment_message==''
                         | olist_order_reviews_dataset$review_comment_message==' ' ]='comment without message'
# Exploring the dataset
View(olist_order_reviews_dataset)

#  Fixing olist_products_dataset 
olist_products_dataset=na.omit(olist_products_dataset)
View(olist_products_dataset)

