library(httr)
library(jsonlite)
library(dplyr)
  
#from raw to usable data:

#"content function" is having three conversion options: as raw, as parsed (list), as text. 


#################################
#############TESLA###############
#################################

my_url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=TSLA&outputsize=full&apikey=YN0U0MH7OZ0HPCRD")

my_raw_result <- httr::GET(my_url)
  #str(my_raw_result)

my_content <- httr::content(my_raw_result, as = 'text')
  #str(my_content)

my_content_from_json <- jsonlite::fromJSON(my_content)
  #dplyr::glimpse(my_content_from_json)

close_price_df <- my_content_from_json$`Time Series (Daily)`


#making the loop

values.close <- as.vector(0)

for (i in 1:length(close_price_df)) {
  values.close[i] <- as.numeric(close_price_df[[i]][[5]])
}

#making a vector, so we can sort on the dates

#ID = c(1:2682)


#combining ID and open/close
#values.close <- cbind(values.close, ID)


#making a df
values.close <- as.data.frame(values.close)

#ordering the values as descending
  #values.close <- values.close[order(-values.close$ID),]
  
  #plot(values.close$values.close, type = "l")


##################################
############BITCOIN###############
##################################

my_url_bitcoin <- paste0(
      "https://api.nomics.com/v1/exchange-rates/history?",
       "key=fe9a34407c27236211ba88d3327401ac&",
       "currency=BTC&",
       "start=2010-06-29T00%3A00%3A00Z&",
       "end=",
       paste0(Sys.Date()), #Get today as date
       "T00%3A00%3A00Z"
        )



my_raw_result_bitcoin <- httr::GET(my_url_bitcoin)
  #str(my_raw_result_bitcoin)


my_content_bitcoin <- httr::content(my_raw_result_bitcoin, as = 'text')
  #str(my_content_bitcoin)

my_content_from_json_bitcoin <- jsonlite::fromJSON(my_content_bitcoin)
  #dplyr::glimpse(my_content_from_json_bitcoin)


#configuring the dates

#Tesla
date_int <- as.vector(as.Date(row.names(t(t(close_price_df)))))
timestamp <- as.vector(row.names(t(t(close_price_df))))
timestamp_tesla <- as.data.frame(timestamp)


#Bitcoin

my_content_from_json_bitcoin$timestamp <- substr(my_content_from_json_bitcoin$timestamp, start=0, stop =10)
  #str(my_content_from_json_bitcoin$timestamp)



#MERGING

#TESLA NR TIMESTAMP = 2682
#BITCOIN NR TIMESTMAP 3452

merged_dates_btc <- merge(timestamp_tesla,my_content_from_json_bitcoin, by = "timestamp", all.x = TRUE) #We generete som NAs
merged_dates_btc <- na.omit(merged_dates_btc) #Removing NA's

merged_dates_tsla <- merge(x = my_content_from_json_bitcoin,y = cbind(timestamp,values.close), by = "timestamp",all.x = TRUE)
merged_dates_tsla <- na.omit(merged_dates_tsla) #Removing NA's

df <- setNames(as_tibble(cbind(merged_dates_btc,merged_dates_tsla$values.close)),nm = c("Date","BTC","TSLA"))

#Converting types
df$BTC <- as.numeric(df$BTC)
df$TSLA <- as.numeric(df$TSLA)
df$BTC <- round(x = df$BTC,digits = 3)


#Removing all but the dataframe
rm(list = setdiff(ls(),"df"))
