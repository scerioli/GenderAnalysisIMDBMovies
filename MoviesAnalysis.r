# ============================ #
#### Movies Gender Analysis ####
# ============================ #
# https://www.imdb.com/interfaces/
# 
# NOTE: This dataset is not up-to-date. The data seems to be reliable uo to
# 2017, and the next dates are only the already claimed to be released movies.
# 
# title.basics.tsv.gz - Contains the following information for titles:
#   - tconst (string) - alphanumeric unique identifier of the title
#   - titleType (string) – the type/format of the title (e.g. movie, short, 
#     tvseries, tvepisode, video, etc)
#   - primaryTitle (string) – the more popular title / the title used by the 
#     filmmakers on promotional materials at the point of release
#   - originalTitle (string) - original title, in the original language
#   - isAdult (boolean) - 0: non-adult title; 1: adult title
#   - startYear (YYYY) – represents the release year of a title. In the case of 
#     TV Series, it is the series start year
#   - endYear (YYYY) – TV Series end year. ‘\N’ for all other title types
#   - runtimeMinutes – primary runtime of the title, in minutes
#   - genres (string array) – includes up to three genres associated with the title
# title.principals.tsv.gz – Contains the principal cast/crew for titles
#   - tconst (string) - alphanumeric unique identifier of the title
#   - ordering (integer) – a number to uniquely identify rows for a given titleId
#   - nconst (string) - alphanumeric unique identifier of the name/person
#   - category (string) - the category of job that person was in
#   - job (string) - the specific job title if applicable, else '\N'
#   - characters (string) - the name of the character played if applicable, else '\N'
# name.basics.tsv.gz – Contains the following information for names:
#   - nconst (string) - alphanumeric unique identifier of the name/person
#   - primaryName (string)– name by which the person is most often credited
#   - birthYear – in YYYY format
#   - deathYear – in YYYY format if applicable, else '\N'
#   - primaryProfession (array of strings)– the top-3 professions of the person
#   - knownForTitles (array of tconsts) – titles the person is known for
# title.ratings.tsv.gz - Contains the IMDb rating and votes information for titles
#   - tconst (string) - alphanumeric unique identifier of the title
#   - averageRating - weighted average of all individual ratings
#   - numVotes - number of votes the title has received

# Analysis credits to Max Woolf
# https://minimaxir.com/2018/07/imdb-data-analysis/

# ---------------------- #
#### 0. LOAD THE DATA ####
# ---------------------- #

df_actors <- read_tsv("files/name.basics.tsv.gz", na = "\\N", quote = '') %>%
  filter(str_detect(primaryProfession, "actor|actress"))  %>%
  select(nconst, primaryName, birthYear)

df_principals <- read_tsv("files/title.principals.tsv.gz", na = "\\N", quote = '') %>%
  filter(str_detect(category, "actor|actress")) %>%
  select(tconst, ordering, nconst, category) %>%
  group_by(tconst) %>%
  # Select only first actress/actor
  filter(ordering == min(ordering))

df_ratings <- read_tsv("files/title.ratings.tsv.gz", na = "\\N", quote = '')

df_basics <- read_tsv("files/title.basics.tsv.gz", na = "\\N", quote = '',
                      col_types = cols(col_character(),
                                       col_character(),
                                       col_character(),
                                       col_character(),
                                       col_logical(),
                                       col_double(),
                                       col_double(),
                                       col_double(),
                                       col_character())) %>%
  select(tconst, primaryTitle, titleType, startYear, genres)


# ---------------------------- #
#### 1. PREPARE THE DATASET ####
# ---------------------------- #

### 1.1 Join the datasets ####
# Note: Using left_join, one does not need to specify the column on which to join
# Joining name of actors/actress to movies where they are the leading characters
df_principals <- df_principals %>% left_join(df_actors)
# Joining ratings to movie titles
df_basics <- df_ratings %>% left_join(df_basics)
# Joining name of leading characters with movie titles and year of release
dt_basics <- as.data.table(df_basics %>% left_join(df_principals))

### 1.2 Selection of data ####
# Quick check of how many movies do we have per year
nMovies <- dt_basics[!is.na(startYear), .N, by = "startYear"]

# Since the movies seems to be updated only up to 2017, we'll cut everything 
# after that date. Moreover, we are interested in the actors and actresses, so 
# I'll cut out also anything which has NA as a category
dt_basics <- dt_basics[startYear <= "2017"]
dt_basics <- dt_basics[!is.na(category)]

### 1.3 Prepare the ratios of the actors / actresses vs year ####
dt_actress <- dt_basics[category == "actress", .(nActress = .N), by = "startYear"]
dt_actor   <- dt_basics[category == "actor",   .(nActor = .N), by = "startYear"]
dt_numbers <- merge(dt_actor, dt_actress, by = "startYear")
dt_numbers <- dt_numbers[, total := nActress + nActor, by = "startYear"]
dt_ratios  <- dt_numbers[, .(ratioActress = nActress / total,
                             ratioActor = nActor / total,
                             startYear)]

### 1.4 Average rating of movies by year ####
df_ratings_movies <- dt_basics %>% filter(titleType == "movie", numVotes >= 10)
df_actors_ratings <- df_ratings_movies %>% group_by(startYear, category) %>% 
                      summarise(low_rating = quantile(averageRating, 0.25, na.rm = TRUE),
                                med_rating = quantile(averageRating, 0.50, na.rm = TRUE),
                                high_rating = quantile(averageRating, 0.75, na.rm = TRUE))

### 1.5 Select only highly-rated movies ####
df_best_movies <- dt_basics[averageRating >= 7.5 & titleType == "movie"]

# ---------------------- #
#### 2. PLOT THE DATA ####
# ---------------------- #

### 2.1 How many movies per year ####
plot(nMovies$startYear, nMovies$N)
abline(v = "2020")

### 2.2 Ratio of actresses and actors vs year ####
dt_ratios  <- melt(dt_ratios, id.vars = 3)
ggplot(data = dt_ratios, aes(x = startYear, y = value, fill = variable, color = variable)) +
  geom_line(aes(y = value))

### 2.3 Average rate of movies of actresses and actors vs year ####
ggplot(data = df_actors_ratings %>% filter(startYear >= 1920), aes(x = startYear, fill = category, color = category)) +
  geom_ribbon(aes(ymin = low_rating, ymax = high_rating), alpha = 0.2, color = NA) +
  geom_line(aes(y = med_rating)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")

### 2.4 Average rates of movies (among the ones rated >= 7.5) of actresses and actors vs year ####
ggplot(data = df_best_movies[startYear > 1950], aes(x = startYear, fill = category, color = category)) +
  stat_summary(aes(y = averageRating), fun = "mean", geom = "line") +
  stat_summary(aes(y = averageRating), fun.data = "mean_se", geom = "ribbon", alpha = 0.2, color = NA)


# Why do we see difference with the other dataset?
# We have high ranking movies which seem to be more skewed towards men.
# A gender vs genres and gender vs budget analysis would be great
# Doing it per genres seems easy
# Budget is not in the data. Web-scraping of wiki pages?