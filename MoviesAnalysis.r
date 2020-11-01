# ============================= #
##### Movies Gender Analysis ####
# ============================= #
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

# Analysis credits to Max Woolf
# https://minimaxir.com/2018/07/imdb-data-analysis/

# ---------------------- #
#### 0. LOAD THE DATA ####
# ---------------------- #

df_actors <- read_tsv("tmp/name.basics.tsv.gz", na = "\\N", quote = '') %>%
  filter(str_detect(primaryProfession, "actor|actress"))  %>%
  select(nconst, primaryName, birthYear)

df_principals <- read_tsv("tmp/title.principals.tsv.gz", na = "\\N", quote = '') %>%
  filter(str_detect(category, "actor|actress")) %>%
  select(tconst, ordering, nconst, category) %>%
  group_by(tconst) %>%
  # Select only first actress/actor
  filter(ordering == min(ordering))

df_basics <- read_tsv("tmp/title.basics.tsv.gz", na = "\\N", quote = '',
                      col_types = cols(col_character(),
                                       col_character(),
                                       col_character(),
                                       col_character(),
                                       col_logical(),
                                       col_double(),
                                       col_double(),
                                       col_double(),
                                       col_character())) %>%
  select(tconst, primaryTitle, startYear, genres)


# ------------------------- #
#### 1. JOIN THE DATASET ####
# ------------------------- #

# Note: Using left_join, one does not need to specify the column on which to join
# Joining name of actors/actress to movies where they are the leading characters
df_principals <- df_principals %>% left_join(df_actors)
# Joining name of leading characters with movie titles and year of release
dt_basics <- as.data.table(df_basics %>% left_join(df_principals))


# Quick check of how many movies do we have per year
nMovies <- dt_basics[!is.na(startYear), .N, by = "startYear"]
plot(nMovies$startYear, nMovies$N)
abline(v = "2020")

# Since the movies seems to be updated only up to 2017, we'll cut everything 
# after that date
dt_basics <- dt_basics[startYear <= "2017"]


# ---------------------- #
#### 2. PLOT THE DATA ####
# ---------------------- #
dt_actress <- dt_basics[category == "actress", .(nActress = .N), by = "startYear"]
dt_actor <- dt_basics[category == "actor",   .(nActor = .N), by = "startYear"]
dt_numbers <- merge(dt_actor, dt_actress, by = "startYear")
dt_numbers <- dt_numbers[, total := nActress + nActor, by = "startYear"]
dt_ratios <- dt_numbers[, .(ratioActress = nActress / total,
                                 ratioActor = nActor / total,
                            startYear)]
dt_ratios <- melt(dt_ratios, id.vars = 3)

ggplot(data = dt_ratios, aes(x = startYear, y = value, fill = variable, color = variable)) +
  geom_line(aes(y = value))


# Why do we see difference with the other dataset?
# We have high ranking movies which seem to be more skewed towards men.
# A gender vs genres and gender vs budget analysis would be great
# Doing it per genres seems easy
# Budget is not in the data. Web-scraping of wiki pages?