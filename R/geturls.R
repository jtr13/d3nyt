library(tidyverse)
library(longurl)
library(rtweet)
library(lubridate)

# tweets <- get_timeline("nytgraphics", n = 8000)
# 
# shorturls <- tweets %>% flatten() %>% 
#   pull(urls_t.co) %>% na.omit() %>% unique()
# 
# 
#  longurls <- expand_urls(shorturls)
#  longurls %>% na.omit() %>% 
#    filter(str_detect(expanded_url, "interactive")) %>% 
#    pull(expanded_url) %>% 
#    str_remove("\\?.*$") %>% 
#    unique() %>% 
#    writeLines("longurls.txt")
# 
# longurls <- readLines("longurls.txt")
# knitr::kable(data.frame(longurls))
# 
# x <- unlist(tweets$urls_url) %>% na.omit() %>% unique()

# nyti.ms <- x[str_detect(x, "nyti.ms")] %>% expand_urls()
# writeLines(nyti.ms$expanded_url, "nyti.ms.txt")
expanded <- readLines("nyti.ms.txt") %>% 
  str_subset(".com/interactive") %>% 
  str_remove("\\?.*$") %>% 
  unique()

# doesn't work well

x <- head(expanded)
webshot::webshot(x)
fix <- function(file) {
  image <- image_read(file)
  info <- image_info(image)
  if (info$width < 1000) {
    cropped_image <- image_crop(image, "800x600+46+230")
  } else{
    cropped_image <- image_crop(image, "800x600+9050+230")
  }
  image_write(cropped_image, paste0(str_remove(file, ".png"), "-fix.png"))
}
files <- list.files(pattern = "webshot.*.png")
lapply(files, fix)

# trying thumbnails instead of webshot, but I need to find the right one

get_thumburl <- function(url) {
  page <- read_html(url)
  title <- page %>% html_node("h1") %>% html_text()
  images <- page %>% html_nodes("img")
  alt <- images %>% html_attr("alt")
  if (length(alt) == 0) return(NA)
  words_in_title <- unlist(str_split(title, " "))
  words_in_title <- words_in_title[nchar(words_in_title) > 4]
  count <- vector()
  for (i in seq_along(alt)) {
    words_in_alt <- unlist(str_split(alt[i], " "))
    words_in_alt <- words_in_alt[nchar(words_in_alt) > 4]
    count[i] <- sum(words_in_alt %in% words_in_title)
  }
  if (which.max(count) == 0) {
    return(0)
  } else
    return(images[[which.max(count)]] %>% html_attr("src"))
}

# sample csv

urls <- head(expanded)

date <- urls %>% 
  str_extract("20[/0-9]+[0-9]")
date[nchar(date)==4] <- paste0(date, "/01/01")

thumbnails <- unlist(lapply(urls, get_thumburl))


# categories

nyti.ms %>% str_remove_all("^.*[0-9]\\/") %>% str_remove_all("\\/.*$") %>% factor() %>% summary()
