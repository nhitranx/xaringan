### This script is dedicated to scraping movies data from TMDB, divided into:
# 1. LOAD LIBRARIES & API KEY
# 2. FUNCTIONS OF GET REQUESTS
# 2.1. Get info of movie 
# 2.2. Get list of movies based on other queries

### 1. LOAD LIBRARIES & API KEY
library(httr)
library(tidyverse)
library(stringr)
library(jsonlite)

source(".Rprofile")
tmdb_key <- getOption("tmdb_key")

### 2. FUNCTIONS OF GET REQUESTS

### 2.1. Get info of movie

get_movie_details <- function(movie_id) {
  
  url <- paste0("https://api.themoviedb.org/3/movie/", {{ movie_id }})
  
  queryString <- list(
    append_to_response = "%20production_countries%2C%20release_date%2C%20genres%2C%20popularity%2C%20revenue%2C%20vote_average%2C%20vote_count%2C%20poster_path",
    language = "en-US"
  )
  
  response <- VERB("GET", url, query = queryString,
                   add_headers(
    Authorization = paste0('Bearer ', tmdb_key),
    `Content-Type` = "application/octet-stream",
    Accept = "application/json"
  ))
  
  movie_det <- fromJSON(content(response, "text"))
  
  # flatten() and as_tibble() both don't work with this nested structure, 
  # the flat tibble below and these conditional statements are suggested by chatGPT.
  genres <- if (!is.null(movie_det$genres)) {
    paste(sort(movie_det$genres$name), collapse = ", ")
  } else {
    NA_character_
  }
  
  production_countries <- if (!is.null(movie_det$production_countries)) {
    paste(sort(movie_det$production_countries$name), collapse = ", ")
  } else {
    NA_character_
  }
  
  spoken_languages <- if (!is.null(movie_det$spoken_languages)) {
    paste(sort(movie_det$spoken_languages$english_name), collapse = ", ")
  } else {
    NA_character_
  }
  
  production_companies <- if (!is.null(movie_det$production_companies)) {
    paste(sort(movie_det$production_companies$name), collapse = ", ")
  } else {
    NA_character_
  }
  
  # Build a flat tibble
  out <- tibble(
    id = movie_det$id,
    title = movie_det$title,
    release_date = movie_det$release_date,
    original_language = movie_det$original_language,
    runtime = movie_det$runtime,
    revenue = movie_det$revenue,
    popularity = movie_det$popularity,
    vote_average = movie_det$vote_average,
    vote_count = movie_det$vote_count,
    genres = genres,
    production_countries = production_countries,
    spoken_languages = spoken_languages,
    production_companies = production_companies,
    overview = movie_det$overview,
    status = movie_det$status,
    tagline = movie_det$tagline,
    imdb_id = movie_det$imdb_id,
    poster_path = movie_det$poster_path,
    homepage = movie_det$homepage,
    origin_country = movie_det$origin_country
  )
  
  return(out)
}


# keywords list of movie
#' get all keywords of movie by movie ID
get_keywords <- function(movie_id) {

  url <- paste0("https://api.themoviedb.org/3/movie/", 
                {{ movie_id }},
                "/keywords")
  
  response <- VERB("GET", url, add_headers(
    Authorization = paste0('Bearer ', tmdb_key),
    `Content-Type` = "application/octet-stream",
    Accept = "application/json"
  ))
  
  keywords_df <- fromJSON(content(response, "text")) %>%
    as_tibble() %>%
    flatten() %>% 
    rename(
      "key_id" = "keywords.id",
      "keyword" = "keywords.name"
    )
  
  return(keywords_df)
}

# reviews of movie
get_reviews <- function(movie_id) {

  url <- paste0("https://api.themoviedb.org/3/movie/",
                {{ movie_id }},
                "/reviews")  
  queryString <- list(
    language = "en-US",
    page = "1"
  )
  
  response <- VERB("GET", url, query = queryString, add_headers(
    Authorization = paste0('Bearer ', tmdb_key),
    `Content-Type` = "application/octet-stream",
    Accept = "application/json"
  ))
  
  reviews_df <- fromJSON(content(response, "text")) %>%
    as_tibble() %>%
    flatten()
  
  return(reviews_df)
}

### 2.2. Get list of movies based on other queries

# get keyword ID based on string
get_kw_id_by_kw <- function(keyword, pg=1) {
  url <- "https://api.themoviedb.org/3/search/keyword"
  
  queryString <- list(
    query = "environmentalism",
    page = as.character(pg)
  )
  
  keyword_info <- VERB("GET", url, query = queryString, add_headers('Authorization' = 'Bearer eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiIxYTFkODcxYTVhNGNhMWNiY2I1ZDQzNjU1MjE2NWJkOSIsIm5iZiI6MTcyMzU5NDM4OC43MDQsInN1YiI6IjY2YmJmNjk0YTY2MmI3OGRhYTgyM2VjNyIsInNjb3BlcyI6WyJhcGlfcmVhZCJdLCJ2ZXJzaW9uIjoxfQ.Ex63mIjMg4a48hK11ZnrKqEAlQmlrwsX-q-olOdQY70'), content_type("application/octet-stream"), accept("application/json"))
  kw_id <- fromJSON(content(keyword_info, "text")) %>%
    as_tibble() %>%
    flatten() %>% 
    pull(results.id)
  
  return(kw_id)
}

# get movies that has specific keyword
#' return movies title and IDs
get_movies_by_kwid <- function(key_id, pg=1) {
  url <- "https://api.themoviedb.org/3/discover/movie"
  
  queryString <- list(
    sort_by = "popularity.desc",
    include_adult = "true",
    include_video = "false",
    language = "en-US",
    page = as.character(pg),
    with_keywords = key_id
  )
  
  raw_movies <- VERB("GET", url, query = queryString, add_headers('Authorization' = 'Bearer eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiIxYTFkODcxYTVhNGNhMWNiY2I1ZDQzNjU1MjE2NWJkOSIsIm5iZiI6MTcyMzU5NDM4OC43MDQsInN1YiI6IjY2YmJmNjk0YTY2MmI3OGRhYTgyM2VjNyIsInNjb3BlcyI6WyJhcGlfcmVhZCJdLCJ2ZXJzaW9uIjoxfQ.Ex63mIjMg4a48hK11ZnrKqEAlQmlrwsX-q-olOdQY70'), content_type("application/octet-stream"), accept("application/json"))
  
  movies_lst <- fromJSON(content(raw_movies, "text")) %>%
    as_tibble() %>%
    flatten() %>% 
    rename("movie_id" = "results.id",
           "movie_title" = "results.title")
  
  # move important cols to front
  movies_lst <- movies_lst %>% 
    select(c(movie_id, movie_title), everything())
  
  return(movies_lst)
}

# loop to get all pages
get_all_pages <- function(get_tmdb_df, ...) {
  # First request, page 1
  first_df <- {{ get_tmdb_df(...) }}
  
  total_pages <- first_df$total_pages[1]
  # return if total pages is null
  if (is.null(total_pages) || length(total_pages) == 0 || is.na(total_pages)) {
    return(first_df)
  }
  
  all_data <- list(first_df)
  # Loop through remaining pages 
  if (total_pages > 1) {
    for (p in 2:total_pages) {
      df <- {{ get_tmdb_df(pg = p, ...) }}
      all_data[[p]] <- df
    }
  }
  
  return(bind_rows(all_data))
}
