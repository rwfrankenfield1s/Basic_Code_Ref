# loading the packages:
library(dplyr) # for pipes and the data_frame function
library(rvest) # webscraping
library(stringr) # to deal with strings and to clean up our data
# extracting the whole website
#google <- read_html("https://news.google.com/")
google <- read_html("https://news.google.com/topics/CAAqJggKIiBDQkFTRWdvSUwyMHZNRGx6TVdZU0FtVnVHZ0pWVXlnQVAB/sections/CAQiXENCQVNQd29JTDIwdk1EbHpNV1lTQW1WdUdnSlZVeUlQQ0FRYUN3b0pMMjB2TURsNU5IQnRLaG9LR0FvVVRVRlNTMFZVVTE5VFJVTlVTVTlPWDA1QlRVVWdBU2dBKioIAComCAoiIENCQVNFZ29JTDIwdk1EbHpNV1lTQW1WdUdnSlZVeWdBUAFQAQ?hl=en-US&gl=US&ceid=US%3Aen")

# extracting the com vehicles
# we pass the nodes in html_nodes and extract the text from the last one 
# we use stringr to delete strings that are not important
vehicle_all <- google %>% 
  html_nodes("div div div main c-wiz div div div article div div div") %>% 
  html_text() %>%
  str_subset("[^more_vert]") %>%
  str_subset("[^share]") %>%
  str_subset("[^bookmark_border]")

vehicle_all[1:10]

time_all <- google %>% html_nodes("div article div div time") %>% html_text()

time_all[1:10]


# extracting the headlines
# and using stringr for cleaning
headline_all <- google %>% html_nodes("article") %>% html_text("span") %>%
  str_split("(?<=[a-z0-9!?\\.])(?=[A-Z])")
# str_split("(?<=[a-z0-9αινσϊ!?\\.])(?=[A-Z])") # for Google News in Portuguese




headline_all <- sapply(headline_all, function(x) x[1]) # extract only the first elements

headline_all[1:10] 


# finding the smallest vector
min <- min(sapply(list(vehicle_all, time_all, headline_all), length))

# cutting
vehicle_all <- vehicle_all[1:min]
time_all <- time_all[1:min]
headline_all <- headline_all[1:min]
#And we have our final data frame:
  
df_news <- data_frame(vehicle_all, time_all, headline_all)

View(df_news)






