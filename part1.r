## -----------------------------------------------------------------------------
suppressMessages(library(tidyverse))
suppressMessages(library(readr))

suppressMessages(
survey <- read_csv(
  "https://raw.githubusercontent.com/introdsci/MusicSurvey/master/music-survey.csv"))

suppressMessages(
preferences <- read_csv(
  "https://raw.githubusercontent.com/introdsci/MusicSurvey/master/preferences-survey.csv"))




## -----------------------------------------------------------------------------
colnames(survey)[colnames(survey) == 
    "First, we are going to create a pseudonym for you to keep this survey anonymous (more or less). Which pseudonym generator would you prefer?"] <- "pseudonym_generator"

colnames(survey)[colnames(survey) == 
    "What is your pseudonym?"] <- "pseudonym"

colnames(survey)[colnames(survey) == 
    "Sex"] <- "sex"

colnames(survey)[colnames(survey) == 
    "Major"] <- "academic_major"

colnames(survey)[colnames(survey) == 
    "Which musical instruments/talents do you play? (Select all that apply)"] <- "instrument_list"

colnames(survey)[colnames(survey) == 
    "Year you were born (YYYY)"] <- "year_born"

colnames(survey)[colnames(survey) == 
    "Academic Year"] <- "academic_level"

colnames(survey)[colnames(survey) == 
    "YOB"] <- "year_born"

colnames(survey)[colnames(survey) == 
    "Artist"] <- "favorite_song_artist"

colnames(survey)[colnames(survey) == 
    "Song"] <- "favorite_song"

colnames(survey)[colnames(survey) == 
    "Link to song (on Youtube or Vimeo)"] <- "favorite_song_link"

colnames(survey)[colnames(survey) == 
    "Timestamp"] <- "time_submitted"


colnames(survey)



## -----------------------------------------------------------------------------
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))



## -----------------------------------------------------------------------------
Person <- tibble(time_submitted = survey$time_submitted, 
    pseudonym_generator = survey$pseudonym_generator, 
    pseudonym = survey$pseudonym, sex = survey$sex, 
    year_born = survey$year_born, academic_level = survey$academic_level, 
    academic_major = survey$academic_major, 
    favorite_instrument = survey$instrument_list)

Person$academic_level <- as.factor(Person$academic_level)
Person$academic_major <- as.factor(Person$academic_major)

# Standardize spelling of variables for
# major
levels(Person$academic_major)[levels(Person$academic_major) == 
    "Computer information systems"] <- "Computer Information Systems"

colnames(Person)

#------------------------------------------------------------------

FavoriteSong <- tibble(pseudonym = survey$pseudonym, 
    song_artist = survey$favorite_song_artist, 
    favorite_song = survey$favorite_song, 
    song_link = survey$favorite_song_link)

colnames(FavoriteSong)

#------------------------------------------------------------------
# This part was tricky. I manipulated the
# original dataframe preferences by using
# the gather function from Dplyr. This
# allowed me to lengthen data that was too
# wide by taking all the columns and
# making those rows under column name
# song_to_rate, then stored the ratings as
# keys called ratings.

preferences <- preferences %>% gather(song_to_rate, 
    rating, 3:45)

# using this new manipulated preferences
# dataframe I can easily map the columns
# to my ratings tibble

Ratings <- tibble(pseudonym = preferences$`What was your pseudonym?`, 
    song_to_rate = preferences$song_to_rate, 
    ratings = preferences$rating)

Ratings$song_to_rate <- as.factor(Ratings$song_to_rate)


## -----------------------------------------------------------------------------

Person$time_submitted <- as.POSIXlt(parse_datetime(Person$time_submitted, 
    format = "%m/%d/%y %H:%M"))




## -----------------------------------------------------------------------------

p <- ggplot(data=Person) + geom_bar(aes(academic_level))
p

p1 <- ggplot(data=Ratings,aes(ratings,fill=pseudonym)) + 
      geom_histogram(binwidth = .5,show.legend = FALSE)

p1


p2 <- ggplot(data=Ratings,aes(ratings,pseudonym)) + geom_boxplot() + theme_light()
p2





## -----------------------------------------------------------------------------

songs <- str_c(FavoriteSong$song_artist," ", FavoriteSong$favorite_song)
songs



test <- semi_join(FavoriteSong, Ratings, by=  )

test



