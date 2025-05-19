# apis-exercise
# For a research project about the role of the media, you need some data about
# news articles. Fortunately, the NY Times provides several APIs to provide the data
# you need.

# (If you prefer, you can also use an API of your choice (see also https://github.com/public-apis/public-apis).)
# https://www.fbi.gov/wanted/api

library(httr)
library(jsonlite)

response_1 <- GET("https://api.fbi.gov/wanted/v1/list")
data_1 <- content(response_1, as = "text")
json_data_1 <- fromJSON(data_1)

json_data_1$total
json_data_1$items$title[1]
json_data_1$items$title

names(json_data_1$items)


response <- GET("https://api.fbi.gov/wanted/v1/list", 
                query = list(sex = "female"))
data <- content(response, as = "text")
json_data <- fromJSON(data)


# 
# response <- GET("https://api.fbi.gov/wanted/v1/list", 
#                 query = list(page = 1))
# data <- content(response, as = "text")
# json_data <- fromJSON(data)
# d <- json_data$items
# df <- d
# 
# 
# response <- GET("https://api.fbi.gov/wanted/v1/list", 
#                 query = list(page = 2))
# data <- content(response, as = "text")
# json_data <- fromJSON(data)
# d <- json_data$items
# df <- rbind(df, d) 
# 
# 
# response <- GET("https://api.fbi.gov/wanted/v1/list",
#                 query = list(page = 13))
# data <- content(response, as = "text")
# json_data <- fromJSON(data)
# d <- json_data$items
# df <- rbind(df, d)


# 
# # Number of pages you want to retrieve
# num_pages <- 10
# 
# # Initialize empty list to store data from each page
# all_items <- list()
# 
# # Loop through pages
# for (i in 1:num_pages) {
#   cat("Fetching page", i, "\n")
#   response <- GET("https://api.fbi.gov/wanted/v1/list", query = list(page = i))
#   data <- content(response, as = "text")
#   json_data <- fromJSON(data)
#   all_items[[i]] <- json_data$items
#   Sys.sleep(3)
# }
# 
# # Combine all pages into a single data frame
# df <- do.call(rbind, lapply(all_items, as.data.frame))
# 
# # View result
# glimpse(df)



library(httr)
library(jsonlite)

num_pages <- 50
all_items <- list()

for (i in 1:num_pages) {
  cat("Fetching page", i, "\n")
  response <- GET("https://api.fbi.gov/wanted/v1/list", query = list(page = i))
  
  if (status_code(response) == 200) {
    content_type <- headers(response)[["content-type"]]
    
    if (grepl("application/json", content_type)) {
      data <- content(response, as = "text", encoding = "UTF-8")
      json_data <- fromJSON(data)
      all_items[[i]] <- json_data$items
    } else {
      cat("Non-JSON content received on page", i, "\n")
    }
  } else {
    cat("Request failed on page", i, "with status", status_code(response), "\n")
  }
  
  Sys.sleep(runif(1, 3, 6))  # avoid hammering the server
}

# Combine all successful results
df <- do.call(rbind, lapply(all_items, as.data.frame))

# df %>% 
#   mutate(crime_type = url %>% 
#            str_remove(fixed("https://www.fbi.gov/wanted/")),
#          crime_type = crime_type %>% 
#            str_extract(".*(?=\\/)")) %>% 
#   select(url, crime_type) %>% head()


df_fbi <- df %>% 
  mutate(crime_type = url %>% 
           str_remove(fixed("https://www.fbi.gov/wanted/")),
         crime_type = crime_type %>% str_remove_all("(?<=\\/).*"),
         crime_type = crime_type %>% str_remove_all(fixed("/")),
         .after = url) #%>% 
  # select(url, crime_type) %>% count(crime_type)


df_fbi %>% 
  ggplot(aes(reward_max, group = crime_type)) +
  geom_density()

# response <- GET("https://api.fbi.gov/wanted/v1/list", query = list(
#   field_offices = "miami"
# ))
# data <- content(response, as = "text")
# json_data <- fromJSON(data)
# 
# json_data$total
# print(json_data$items$title[1])
# json_data$items$title
# 
# # with paging
# response <- GET("https://api.fbi.gov/wanted/v1/list", query = list(
#   page = 2
# ))
# data <- content(response, as = "text")
# json_data <- fromJSON(data)
# 
# json_data$page
# json_data$items$title




# 1. Create a developer account at the NYT: "https://developer.nytimes.com/get-started"

# 2. Sign in, and go to "Apps". (This is in the top right corner.)

# 3. Create a new app, give it a name, and enable the "Article Search API".

# 4. Copy the API Key (don't share it with others ;).

# 5. Read the documentation for the Article Search API.

# 6. Replicate the example call with you API key.

# 7. Send the call to the API, and store it in an object.

# 8. Inspect the results. What types of data did the API give? How are they ordered?

# 9. Instead of "election", search for articles with "humboldt".

# 10. The API only returned data about ten articles. How many results are there in total? 
# Make a call to the API that gets you the data for results 11-20.
# (hint: This is explained in the documentation for the API.)

# 11. Force the results into a dataframe, using the function data.frame().

# 12. Extract only the variable for the publication date.

# 13. Use the function substr() to only get the year from this variable. 
# (hint: ?substr() shows you how to use the function)

# 14. Using the call from step 10, build a for loop for the first 3 pages of the results.
# Force the results into a dataframe, and bind the results for each call together to one
# dataframe. You can use the bind_rows() function from the dplyr package.
# Make sure to respect the rate limit of the API (hint: Sys.sleep()). You find info
# about that in the API documentation.

# Optional: Use this package to replicate the assignment: "https://github.com/mkearney/nytimes". 
# (hint: you can also provide the API key to the function using the argument "apikey = YOUR_API_KEY")
# How do your results differ?

