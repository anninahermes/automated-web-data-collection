library(httr)
library(jsonlite)
library(magick)

get_random_duck <- function() {
  # Make API request to get random duck
  response <- GET("https://random-d.uk/api/random")
  
  # Check if request was successful
  if (status_code(response) == 200) {
    # Parse JSON response
    duck_data <- fromJSON(rawToChar(response$content))
    
    # Get the image URL
    image_url <- duck_data$url
    
    # Read and display the image
    duck_image <- image_read(image_url)
    print(duck_image)
    
    # Return the URL for reference
    return(image_url)
  } else {
    stop("Failed to fetch duck image")
  }
}

# Example usage:
# get_random_duck() 