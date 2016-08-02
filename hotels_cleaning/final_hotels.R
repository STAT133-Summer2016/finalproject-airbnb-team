la_df = read_csv("cleaned_la_hotel.csv")
ch_df = read_csv("cleaned_ch_hotel.csv")
ny_df = read_csv("cleaned_ny_hotel.csv")
sf_df = read_csv("cleaned_sf_hotel.csv")

cleaned_four_hotels = rbind(la_df, ch_df, ny_df, sf_df)

write_csv(cleaned_four_hotels, "cleaned_four_hotels.csv")
