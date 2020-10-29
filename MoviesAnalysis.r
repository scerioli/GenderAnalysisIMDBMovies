#https://github.com/maazh/IMDB-Movie-Dataset-Analysis/blob/master/tmdb-movies.csv

library(data.table); library(rpart); library(bit64); library(httr); library(RCurl)
library(jsonlite);  library(fasttime)
library(knitr); library(rmarkdown); library(kableExtra)
library(devtools)
library(WikipediR)
library(RCurl)

#### 0. LOAD DATA ####
# Define path
setwd("~/Desktop/Kaggle/")

# Load data
#imdb <- fread("IMDB-Movie-Data.csv")
babyNames <- fread("babynames-clean.csv")
actorNames <- fread("ActorsNames.csv")
imdb <- fread("movies.csv", fill = TRUE)
# ------------------- #


#### 1. PREPARE DATASET ####

# Set names for data frame
setnames(babyNames, c("Name", "Gender"))
# Merge the files
allNames <- rbind(babyNames, actorNames)

# Rename columns with difficult names
# imdb[, Revenue_millions := `Revenue (Millions)`]
# imdb[, Runtime_minutes := `Runtime (Minutes)`]
# imdb[, `Revenue (Millions)` := NULL]
# imdb[, `Runtime (Minutes)` := NULL]

# Substitute corrupted names of actors
imdb$cast[[49]] <- "Jason Statham|Michael Angarano|Milo Ventimiglia|Dominik García-Lorido|Anne Heche"
imdb$cast[[54]] <- "Jennifer Lawrence|Bradley Cooper|Robert De Niro|Dascha Polanco|Edgar Ramírez"
imdb$cast[[55]] <- "Edgar Ramírez|Luke Bracey|Teresa Palmer|Delroy Lindo|Ray Winstone"
imdb$cast[[124]] <- "Channing Tatum|Matt Bomer|Joe Manganiello|Kevin Nash|Adam Rodriguez"
imdb$cast[[138]] <- "Reese Witherspoon|Sofía Vergara|Michael Mosley|John Carroll Lynch|Richard T. Jones"

# Separate the actors
singleActor <- strsplit(imdb$cast, "|", fixed = TRUE)

# Assign new columns to data table corresponding to the first three actors names
cols <- c("firstActor")
idx <- 1

for (actor in cols) {
  # Separate first names to assign the gender
  name <- paste0(actor, "Name")
  imdb[, (actor) := trimws(sapply(singleActor, "[", idx), which = "left")]
  # Take the first name only and add it to the data set
  imdb[, (name)  := sapply(strsplit(imdb[[(actor)]], " "), "[[", 1)]
  # Merge names and actors to categorize the gender of the actors
  gender <- paste0(actor, "Gender")
  imdb <- merge(allNames, imdb, by.x = "Name",  by.y = (name), all.y = T)
  imdb[, (gender) := Gender]
  # Remove useless columns
  imdb[, `:=` (Name = NULL, Gender = NULL)]
  
  idx <- idx + 1
}

# Normalize the data
imdbTotalMovies <- imdb[, .N, by = "Year"]
imdbTotalMovies <- imdbTotalMovies[order(Year)]
# Find ratios for girls and boys
cols_gender <- c("firstActorGender", "secondActorGender", "thirdActorGender")
imdbRatio <- data.table(Year = imdbTotalMovies$Year)

for (gender in cols_gender) {
  imdbGirls <- imdb[get(gender) == "girl", .N, by = "Year"]
  imdbGirls <- imdbGirls[, (gender) := "girl"]
  imdbGirls <- imdbGirls[order(Year)]

  imdbBoys <- imdb[get(gender) == "boy", .N, by = "Year"]
  imdbBoys <- imdbBoys[, (gender) := "boy"]
  imdbBoys <- imdbBoys[order(Year)]

  imdbRatioBoys <- imdbBoys[imdbTotalMovies, .(ratioBoys = N / i.N, Year), on = "Year"]
  imdbRatioGirls <- imdbGirls[imdbTotalMovies, .(ratioGirls = N / i.N, Year), on = "Year"]

  ratioBoysVar  <- paste0("ratioBoys_", sub("Gender", replacement = "", gender))
  ratioGirlsVar <-paste0("ratioGirls_", sub("Gender", replacement = "", gender))
  
  imdbRatio[imdbRatioBoys, (ratioBoysVar) := ratioBoys, on = "Year"]
  imdbRatio[imdbRatioGirls, (ratioGirlsVar) := i.ratioGirls, on = "Year"]
}

# Melting the data so it is possible to plot together
imdbRatioMelt <- melt(imdbRatio, id.vars = 1)
# ------------------------ #


#### 2. VISUALIZE THE DATA ####

# 2.1 Number of movies with boy vs girl actors aggregated
barplot1 <- ggplot(data = imdb, aes(x = firstActorGender)) + 
  geom_bar(aes(fill = firstActorGender), position = "dodge") + 
  xlab("Gender of first Actor") + ylab("Count") +
  ggtitle("First actors gender")

# 2.2 Number of movies with boy vs girl actors during the years
barplot1_2 <- ggplot(data = imdb, aes(x = factor(Year))) + 
  geom_bar(aes(fill = firstActorGender), position = "dodge") + 
  xlab("Year of production") + ylab("Count") +
  ggtitle("First actors gender over the years")

barplot2_2 <- ggplot(data = imdb, aes(x = factor(Year))) + 
  geom_bar(aes(fill = secondActorGender), position = "dodge") + 
  xlab("Year of production") + ylab("Count") +
  ggtitle("Second actors gender over the years")

barplot3_2 <- ggplot(data = imdb, aes(x = factor(Year))) + 
  geom_bar(aes(fill = thirdActorGender), position = "dodge") + 
  xlab("Year of production") + ylab("Count") +
  ggtitle("Third actors gender over the years")

# Put the plots together in one page
gridExtra::grid.arrange(barplot1_2, barplot2_2, barplot3_2, nrow = 2)

# 2.3 Number of movies with boy vs girl actors during the years normalized
# on number of movies for that year
barplot3 <- ggplot(data = imdbRatioMelt, aes(x = Year, y = value, 
                                             colour = variable, 
                                             group = variable)) + 
  geom_smooth(aes(fill = variable)) +
  xlab("Year of production") + ylab("Normalized Ratio") +
  ggtitle("Normalized ratio of female vs male actors in role of first, second,
          and third principal actor over the years")

# It would be nice to have 3 different plots for the first, second and third actors
my_formula <- y ~ x
ggplot(data = imdbRatioMelt, aes(x = Year, y = value, 
                             colour = variable, 
                             group = variable)) + 
  geom_smooth(method = "lm", aes(fill = variable), formula = my_formula) +
  geom_point(aes(fill = variable)) +
  xlab("Year of production") + ylab("Normalized Ratio")


# 2.3 Gender vs revenue
boxplot <- ggplot(data = imdb[!is.na(Revenue_millions)], 
                  aes(x = firstActorGender, y = Revenue_millions)) + geom_boxplot()


# ---------------------------------- #


#### 3. STATISTICAL ANALYSIS ####

# 3.1 Significance of the difference in numbers of movies with girls first actor 
# and boys first actor
dCohen_number <- (mean(imdbRatio$ratioBoys) - mean(imdbRatio$ratioGirls)) / 
  sqrt(sd(imdbRatio$ratioGirls)^2 + sd(imdbRatio$ratioBoys)^2)
# dCohen_number = 12.3214

# 3.2 Significance of the difference in revenue between movies with girls first
# actor and boys first actor
dCohen_revenue <- (mean(imdb[firstActorGender == "boy" & !is.na(Revenue_millions), Revenue_millions]) - 
                      mean(imdb[firstActorGender == "girl" & !is.na(Revenue_millions), Revenue_millions])) / 
                  sqrt(sd(imdb[firstActorGender == "boy" & !is.na(Revenue_millions), Revenue_millions])^2 + 
                         sd(imdb[firstActorGender == "girl" & !is.na(Revenue_millions), Revenue_millions])^2)
# dCohen_revenue = 0.2676383
# Classified as small (small = 0.2, medium = 0.5)

# ----------------------------- #


# Further ideas:
# - Analyse gender distribution of the directors
# - Genre vs gender
# Need to separate the genre inputs (currently many)
ggplot(data = imdb[Genre %like% "Romance"], aes(x = firstActorGender)) + 
  geom_bar(aes(fill = firstActorGender))




baseEndPoint <- "https://en.wikipedia.org/w/api.php?"
action <- "action=query"
origin <- "origin=*"
format <- "format=json"
generator <- "generator=search"
#prop <- "prop=revisions"
#title <- "title=Stanford%20University"
space <- "gsrnamespace=0"
limit <- "gsrlimit=5"
search <- "gsrsearch='Lists_of_actors'"



url <- paste0(baseEndPoint, 
              paste(action, origin, format, generator, search, 
                    sep = "&"))

response <- GET(url = url)
content <- content(response, as = "text", encoding = "utf-8")
result <- fromJSON(content)

wiki <- as.data.table(result)


top_editors_page <- "http://en.wikipedia.org/wiki/Wikipedia:List_of_Wikipedians_by_number_of_edits"
top_editors_table <- readHTMLTable(top_editors_page)
very_top_editors <- as.character(top_editors_table[[3]][1:5,]$User)

# setup connection to wikimedia project 
con <- wiki_con("en", project = c("wikipedia"))

# connect to API and get last 50 edits per user
user_data <- lapply(very_top_editors,  function(i) wiki_usercontribs(con, i) )
# and get information about the users (registration date, gender, editcount, etc)
user_info <- lapply(very_top_editors,  function(i) wiki_userinfo(con, i) )
