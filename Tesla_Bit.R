
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

str(my_raw_result)




my_content <- httr::content(my_raw_result, as = 'text')
str(my_content)

my_content_from_json <- jsonlite::fromJSON(my_content)
dplyr::glimpse(my_content_from_json)

close_price_df <- my_content_from_json$`Time Series (Daily)`




testets <- as.numeric(close_price_df[[1]][[1]])

#making the loop

values.open <- as.vector(0)
values.close <- as.vector(0)

for (i in 1:length(close_price_df)) {
  values.open[i] <- as.numeric(close_price_df[[i]][[1]])
  values.close[i] <- as.numeric(close_price_df[[i]][[5]])
}

#making a vector, so we can sort on the dates

ID = c(1:2682)


#combining ID and open/close
values.open <- cbind(values.open, ID)
values.close <- cbind(values.close, ID)


#making a df
values.open <- as.data.frame(values.open)
values.close <- as.data.frame(values.close)

#ordering the values as descending
values.open <- values.open[order(-values.open$ID),]
values.close <- values.close[order(-values.close$ID),]

plot(values.open$values.open, type = "l")
plot(values.close$values.close, type = "l")

max(values.open$values.open)



##################################
############BITCOIN###############
##################################



my_url_bitcoin <- paste0(
      "https://api.nomics.com/v1/exchange-rates/history?",
       "key=fe9a34407c27236211ba88d3327401ac&",
       "currency=BTC&",
       "start=2010-06-29T00%3A00%3A00Z&",
       "end=2021-02-24T00%3A00%3A00Z"
        )




my_raw_result_bitcoin <- httr::GET(my_url_bitcoin)

str(my_raw_result_bitcoin)


my_content_bitcoin <- httr::content(my_raw_result_bitcoin, as = 'text')
str(my_content_bitcoin)

my_content_from_json_bitcoin <- jsonlite::fromJSON(my_content_bitcoin)
dplyr::glimpse(my_content_from_json_bitcoin)


#configuring the dates

#Tesla
date_int <- as.vector(as.Date(row.names(t(t(close_price_df)))))
timestamp <- as.vector(row.names(t(t(close_price_df))))
timestamp_tesla <- as.data.frame(timestamp)


#Bitcoin

my_content_from_json_bitcoin$timestamp <- substr(my_content_from_json_bitcoin$timestamp, start=0, stop =10)
str(my_content_from_json_bitcoin$timestamp)



#MERGING

#TESLA NR TIMESTAMP = 2682
#BITCOIN NR TIMESTMAP 3452

merged_dates <- merge(timestamp_tesla,my_content_from_json_bitcoin, by = "timestamp", all.x = TRUE) #We generete som NAs

##### TO DO - removing NAs-----

  ### Must remove the nulls




#### Just a control of the two series' of time

#sorting

ID = c(1:2682)
timestamp_tesla_asc <- as.data.frame(cbind(timestamp_tesla,ID))
timestamp_tesla_asc <- timestamp_tesla_asc[order(-timestamp_tesla_asc$ID),]
timestamp_tesla_asc <- timestamp_tesla_asc[1]


timestamp_comparison <- cbind(timestamp_tesla_asc, merged_dates$timestamp, timestamp==merged_dates$timestamp)


##### Dataframes
# This is intended to contain the files for further analysis.


  #TSLA in ascending dates
  values.open <- as.vector(0)
  values.close <- as.vector(0)
  
  for (i in 1:length(close_price_df)) {
    values.open[i] <- as.numeric(close_price_df[[i]][[1]])
    values.close[i] <- as.numeric(close_price_df[[i]][[5]]) #Adjusted close price
  }

df <- setNames(as.data.frame(cbind(timestamp_tesla,values.close,merged_dates$rate)),nm = c("Timestamp","TSLA","BTC"))

## Note, we have a lot of NAs. There appear to be some days where information on the BTC price is missing. Hence we should deal with that