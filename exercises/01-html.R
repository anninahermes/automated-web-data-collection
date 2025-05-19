# html - exercise

library(tidyverse)
library(rvest)

# 1. Go to the website of the Parliament of Australia

# 2. Go to the list of all current members

# 3. Set the results per page to the maximum
# https://www.aph.gov.au/Senators_and_Members/Parliamentarian_Search_Results?q=&mem=1&sen=1&par=-1&gen=0&ps=96&st=1

# 4. Look at the source code

# 5. Find a css path for the names (and links) to all members on the first page (use the Selector Gadget, if necessary)

# 6. Load the page into R

# 7. Use the rvest package and the css path from above to extract all elements, and store them in an object.


url <- "https://www.aph.gov.au/Senators_and_Members/Parliamentarian_Search_Results?q=&mem=1&sen=1&par=-1&gen=0&ps=96&st=1"

page <- read_html(url)

page %>% 
  html_nodes("#main_0_content_0_snmSearchResults > div.search-filter-results.search-filter-results-snm.row > div .title a") %>% 
  html_attr("href")

# 8. Get the href attribute for all elements

v_href <- page %>% 
  html_nodes("#main_0_content_0_snmSearchResults > div.search-filter-results.search-filter-results-snm.row > div .title a") %>% 
  html_attr("href")

# 9. Get the text for all elements

v_names <- page %>% 
  html_nodes("#main_0_content_0_snmSearchResults > div.search-filter-results.search-filter-results-snm.row > div .title a") %>% 
  html_text()

# other aspects
# page %>% 
#   html_nodes("#main_0_content_0_snmSearchResults > div.search-filter-results.search-filter-results-snm.row > div > div > div.medium-push-2.medium-7.large-8.columns > dl") %>% 
#   html_text2()

# 10. Create a data.frame with two columns, the link/href from above and the the name/text from above. Store the data.frame in an object.

tibble(name = v_names,
       url = paste0("https://www.aph.gov.au", v_href))  
  
# 11. Try finding a CSS path for the party of the members on the page. (It might not be possible.)

t <- read_html("https://www.aph.gov.au/Senators_and_Members/Parliamentarian?MPID=R36")

t %>% 
  html_elements("#content > div > div > div > div:nth-child(3) > div.medium-7.columns > div > dl") %>% html_text2()


# 12. How would you add the party of members to your dataset?









