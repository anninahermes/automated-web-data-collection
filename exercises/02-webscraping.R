library(xml2)
xml2::write_html()

download.file(url, destfile)
read_html(destfile)


# webscraping-exercise

# For a project on responsiveness of elected politicians, you were asked to 
# create a dataset of the the responses of Members of the Berlin State 
# Parliament to citizen questions on abgeordnetenwatch.de.
# You can find the questions here: "https://www.abgeordnetenwatch.de/berlin/fragen-antworten"

# 1. Go to the website, how are the links to the full questions/responses organized?

# 2. Create a vector with links to all the pages with links to the questions/responses.

library(tidyverse)
library(rvest)
library(xml2)

url <- "https://www.abgeordnetenwatch.de/berlin/fragen-antworten"
page <- read_html(url)

# "https://www.abgeordnetenwatch.de/berlin/fragen-antworten?page=0"
# "https://www.abgeordnetenwatch.de/berlin/fragen-antworten?page=175"

links <- paste0("https://www.abgeordnetenwatch.de/berlin/fragen-antworten?page=", 0:175)

# 3. Create a folder where you will save the HTML files with the full questions/responses.

getwd()
if(!dir.exists("exercises/02-data")) dir.create("exercises/02-data")

# 4. Read the first page into R, and save it in the created folder.

t1 <- read_html(links[1])
filename <- str_c("exercises/02-data/", basename(links[1]), ".html")
if(!file.exists(filename)) write_html(t1, filename)

# 5. Build a for loop for the links to the first three pages. First define a filename that is different for each link.
# (hint: you can name the files "0.html", "1.html", etc. by extracting the page number from the link via regular expression).
# Read the html from the link, then write it to the filename.
# Make sure to pause R at the end of each iteration.


t_links <- links[1:3]
for (link in t_links) {
  filename <- str_c("exercises/02-data/", basename(link), ".html")
  if(file.exists(filename)) next
  # print(filename)
  myhtml <- read_html(link)
  write_html(myhtml, file = filename)
  Sys.sleep(1)
}

list.files("exercises/02-data")

# 6. Read the first html page (from the folder) and extract all links to the page for the full question/response on that page using a css path (hint: ".tile__question__teaser a").

page <- read_html("exercises/02-data/fragen-antworten?page=0.html")
p1_links <- page %>% 
  html_elements(".tile__question__teaser a") %>% 
  html_attr("href")

paste0("https://www.abgeordnetenwatch.de", p1_links)

# 7. Create a vector with the (full) file path to each file in your folder. (hint: The path has to include the folder name.)

files <- paste("exercises/02-data", list.files("exercises/02-data"), sep = "/")

# 8. Build a for loop for all files in your folder. Extract the links and add them all to a vector.

v_questions <- c()

for (file in files) {
  page <- read_html(file)
  page_links <- page %>% 
    html_elements(".tile__question__teaser a") %>% html_attr("href")
  v_questions <- c(v_questions, page_links)
}

v_questions

# 9. The links are only relative URLs, i.e. they are missing the domain. Add the domain using a stringr function.


# 10. Download the first three HTMLs with the full questions/responses. (hint: download.file(url, destfile)

v_questions <- paste0("https://www.abgeordnetenwatch.de", v_questions)


t_questions <- v_questions[1:3]

for (link in t_questions) {
  filename <- str_c("exercises/02-data/", basename(link), ".html")
  if(file.exists(filename)) next
  # print(filename)
  download.file(link, destfile = filename)
  Sys.sleep(1)
}

# 10. Download the first three HTMLs with the full questions/responses. (hint: download.file(url, destfile)

i <- 100
for (i in 0:2) {
  url <- paste0(base_url, "?page=", i)
  file_name <- paste0("abgeordneten_html/", i, ".html")
  download.file(url, destfile = file_name)
  Sys.sleep(2)  # Be polite
}
i

function(){
  i <- 200
}

# Load required packages
library(rvest)
library(xml2)
library(stringr)

# Base URL and page setup
base_url <- "https://www.abgeordnetenwatch.de/berlin/fragen-antworten"
pages <- paste0(base_url, "?page=", 0:2)

# Create folders for HTML files
dir.create("abgeordneten_html", showWarnings = FALSE)
dir.create("abgeordneten_qas", showWarnings = FALSE)

# Step 4–5: Download the first 3 list pages and save them
for (i in 0:2) {
  url <- paste0(base_url, "?page=", i)
  file_name <- paste0("abgeordneten_html/", i, ".html")
  download.file(url, destfile = file_name)
  Sys.sleep(2)  # Be polite
}

# Step 6: Read first list page and extract relative links to Q&A pages
html1 <- read_html("abgeordneten_html/0.html")
question_links <- html_elements(html1, ".tile__question__teaser a")
relative_urls <- html_attr(question_links, "href")

# Step 7: Vector of all saved HTML file paths
html_files <- list.files("abgeordneten_html", full.names = TRUE)

# Step 8: Extract all relative links from all pages
all_relative_urls <- c()

for (file in html_files) {
  page <- read_html(file)
  links <- html_elements(page, ".tile__question__teaser a")
  hrefs <- html_attr(links, "href")
  all_relative_urls <- c(all_relative_urls, hrefs)
}

# Step 9: Convert relative links to full URLs
base_domain <- "https://www.abgeordnetenwatch.de"
full_urls <- str_c(base_domain, all_relative_urls)

# Step 10: Download the first 3 Q&A pages
for (i in 1:3) {
  qa_url <- full_urls[i]
  file_name <- paste0("abgeordneten_qas/qa_", i, ".html")
  download.file(qa_url, destfile = file_name)
  Sys.sleep(2)  # Be polite
}
