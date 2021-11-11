## create decade from a year

create_decade <- function(df){

  df <- df %>%
    mutate(decade = start_date_year - start_date_year%%10)

  df

}
