# GenderAnalysisIMDBMovies

https://www.imdb.com/interfaces/
 
NOTE: This dataset is not up-to-date. The data seems to be reliable uo to
2017, and the next dates are only the already claimed to be released movies.
 
#### title.basics.tsv.gz
Contains the following information for titles:
   - tconst (string) - alphanumeric unique identifier of the title
   - titleType (string) – the type/format of the title (e.g. movie, short, 
     tvseries, tvepisode, video, etc)
   - primaryTitle (string) – the more popular title / the title used by the 
     filmmakers on promotional materials at the point of release
   - originalTitle (string) - original title, in the original language
   - isAdult (boolean) - 0: non-adult title; 1: adult title
   - startYear (YYYY) – represents the release year of a title. In the case of 
     TV Series, it is the series start year
   - endYear (YYYY) – TV Series end year. ‘\N’ for all other title types
   - runtimeMinutes – primary runtime of the title, in minutes
   - genres (string array) – includes up to three genres associated with the title
#### title.principals.tsv.gz
Contains the principal cast/crew for titles
   - tconst (string) - alphanumeric unique identifier of the title
   - ordering (integer) – a number to uniquely identify rows for a given titleId
   - nconst (string) - alphanumeric unique identifier of the name/person
   - category (string) - the category of job that person was in
   - job (string) - the specific job title if applicable, else '\N'
   - characters (string) - the name of the character played if applicable, else '\N'
#### name.basics.tsv.gz 
Contains the following information for names:
   - nconst (string) - alphanumeric unique identifier of the name/person
   - primaryName (string)– name by which the person is most often credited
   - birthYear – in YYYY format
   - deathYear – in YYYY format if applicable, else '\N'
   - primaryProfession (array of strings)– the top-3 professions of the person
   - knownForTitles (array of tconsts) – titles the person is known for
#### title.ratings.tsv.gz
Contains the IMDb rating and votes information for titles
   - tconst (string) - alphanumeric unique identifier of the title
   - averageRating - weighted average of all individual ratings
   - numVotes - number of votes the title has received

### Analysis credits
Thanks to Max Woolf's blog for the very nice inputs!
https://minimaxir.com/2018/07/imdb-data-analysis/
