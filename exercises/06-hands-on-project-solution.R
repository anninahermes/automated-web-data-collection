# Exercise 6: Hands-on Project (Solution)
# Scrape a list of URLs, save HTML, implement feedback, error handling, and logging

library(xml2)
library(tibble)

# Example URLs (replace with your own)
urls <- c("https://httpbin.org/html", "https://example.com/")

# Create output folder if it doesn't exist
if (!dir.exists("data/html")) dir.create("data/html", recursive = TRUE)

# Initialize log
log <- tibble(url = character(), file = character(), date = as.POSIXct(character()), status = character())

for (i in seq_along(urls)) {
  url <- urls[i]
  cat(sprintf("[%d/%d] Downloading: %s\n", i, length(urls), url))
  filename <- sprintf("data/html/page_%03d_%s.html", i, format(Sys.Date(), "%Y%m%d"))
  if (file.exists(filename)) {
    cat("File exists, skipping.\n")
    log <- add_row(log, url = url, file = filename, date = Sys.time(), status = "skipped")
    next
  }
  result <- tryCatch({
    html <- read_html(url)
    write_html(html, filename)
    log <- add_row(log, url = url, file = filename, date = Sys.time(), status = "success")
  }, error = function(e) {
    cat(sprintf("Error downloading %s: %s\n", url, e$message))
    log <- add_row(log, url = url, file = filename, date = Sys.time(), status = "error")
  })
}

# Summarize log
print(log)
cat(sprintf("Successes: %d, Errors: %d\n", sum(log$status == "success"), sum(log$status == "error"))) 