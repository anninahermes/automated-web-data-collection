Automated Web Data Collection

# Course Description

The internet is an essential source of data for social science research, providing access to vast amounts of text and structured information. This course introduces students to methods for automated web data collection, focusing on practical applications in political science and other social sciences. Students will learn web scraping techniques, work with APIs, and process various data formats. Ethical and legal considerations will also be discussed.

# Course Preparation

• HTML basics (https://r4ds.hadley.nz/webscraping.html#html-basics)
• CSS Diner (https://flukeout.github.io)
• Install Selector Gadget (https://selectorgadget.com/)
• 3 ideas for data collection projects (see below for examples)
• R basics (if needed)

# Course Outline Day 1

## Overview

## HTML and Web Structure

• Understanding HTML and CSS

• Extracting structured information from web pages (rvest)


## Web Scraping

• Introduction to rvest and other scraping tools in R

• Practical exercises in scraping political websites and news articles


## APIs and Data Formats (JSON, XML, CSV, etc.)

• Accessing structured data via APIs

• Handling different data formats in R

## Exercises and hands-on projects


# Course Outline Day 2

## Advanced Techniques 1

• Dealing with dynamic content (JavaScript, AJAX)

• RSelenium

• Understanding webpage backends

##  Advanced Techniques 2

• File management

• Scheduled scraping

• Handling scraping challenges (CAPTCHAs, rate limits, session IDs, proxies)

## Ethics and Legal Aspects of Web Data Collection

• Best practices

• Ethical considerations in data collection

• Legal frameworks (copyright, GDPR, terms of service, etc.)

## Outlook

• OCR, PDFs
• Regular expressions
• Images as data
• Text as data
• Firecrawl
• Geocoding and geodata
• Existing packages for scraping and accesing APIs

## Exercises and hands-on projects


# Textbooks

- Munzert, Simon, Christian Rubba, Peter Meißner, Dominic Nyhuis (2014). Automated Data Collection with R – A Practical Guide to Web Scraping and Text Mining. *John Wiley & Sons*, Chichester. [https://doi.org/10.1002/9781118834732](https://doi.org/10.1002/9781118834732)
- Grolemund, G., & Wickham, H. (2023). R for Data Science (2nd Edition). *O'Reilly Media*. [https://r4ds.hadley.nz/](https://r4ds.hadley.nz/)


# Scraping examples
## Easy
* Wikipedia (useful for networks etc.)
* Parties' press releases (Varies in difficulty)
* Polls (wahlrecht.de)
* Conference programs (EPSA, DVPW)
* Abgeordnetenwatch.de (questions and answers from candidates)
* German Lobby Register (can get very complex)
* Web Search Results (DuckDuckGo)
* News articles

## Medium
* Korean election results (backend, JSON)
* Parliamentary protocols (sometimes as documents or PDFs)
* US live election data from the New York Times (JSON backend)
* Polls (Politico JSON)
* Doctolib appointment availability (JSON)
* List of far-right demonstrations from parliamentary query (parsing PDFs and geocoding)
* Privatization of state owned companies (Treuhand) map (https://treuhandanstalt.online/karte/)

## Difficult
* German Members of Parliament (MPs) websites (parallel scraping/crawling)
* LinkedIn profiles (Python library) (https://pypi.org/project/linkedin-scraper/)
* Air quality sensor data worldwide (encrypted via JavaScript) (https://waqi.info/#/c/3.563/8.145/2.2z)
* Historic shapefiles for Danish parishes (https://dataforsyningen.dk/data/4840)


# Author
- **Cornelius Erfort**  
  Post-doctoral Researcher  
  University of Witten/Herdecke  
  [cornelius.erfort@uni-wh.de](mailto:cornelius.erfort@uni-wh.de)  



