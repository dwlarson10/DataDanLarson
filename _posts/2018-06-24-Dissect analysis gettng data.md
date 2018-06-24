---
layout: post
title:  "Analysis of Dissect Artists post 1: Getting Data"
tags:
- Goals
- Learning
- Data science
- sods18
---

#Analysis of Dissect Artists post 1: Getting Data


For the past year I have been completely obsessed with the [Podcast Dissect](https://dissectpodcast.com/). My obsession started with Cole's deep dive into Kanye West's 'Beautiful Dark Twisted Fantasy' and then into Kendrick Lamar's 'To Pimp a Butterfly.' Cole digs into the artist and the music and gives you an appreciation for the work that I personally didn't have before. After listening to the Kanye season, I played BDTF on repeat for nearly six months. At the time of me writing this, Dissect is digging into Frank Ocean's album 'Blond.' I thought I would dig into some of the tools available from Spotify and see if there are any statistical trends we can identify from the artists analyzed by Cole Cuchna.

### Data

These data were collected using the Spotify API. Spotify enables users to collect artist data down to the track level. For each track they have developed 15 characteristics. They are: danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration\_ms, time\_signature, key\_mode, track\_popularity. The data also includes album popularity, release date, and track name.

### spotifyr package

With many popular APIs there is often someone who has built a package or wrapper to use with R. The Spotify API is no different. There is a great R wrapper for the Spotify API called [spotifyr](https://github.com/charlie86/spotifyr) that has been developed by [charles86](https://github.com/charlie86) on github. With only a few lines of code I was able to get all the tracks from Frank Ocean, Kanye West, and Kendrick Lamar.

``` r
require(spotifyr)

# Sets the API Keys and authenticates app

Sys.setenv(SPOTIFY_CLIENT_ID = 'my_client_ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'my_client_secret')
access_token <- get_spotify_access_token()

#Extracts data for each artists, adds a variable for artists and binds them to a single data-frame.

FOdf <- get_artist_audio_features('Frank Ocean')
FOdf$artist <- 'Frank Ocean'
KWdf <- get_artist_audio_features('Kanye West')
KWdf$artist <- 'Kanye West'
KLdf <- get_artist_audio_features('Kendrick Lamar')
KLdf$artist <- 'Kendrick Lamar'
dissect <- rbind(FOdf,KLdf,KWdf)
```

### Starting an exploratory analysis

Now that we have the data we can start to explore it. The first thing that I like to do is just look at a summary of all the fields. While these summaries don't tell us much, they do help to give me a sense of what each data field might look like.

``` r
dissect%>%
  select(artist,track_popularity,danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,duration_ms,time_signature,key_mode)%>%
  summary()
```

    ##             artist    track_popularity  danceability        energy      
    ##  Frank Ocean   : 34   Min.   : 0.00    Min.   :0.2010   Min.   :0.0715  
    ##  Kanye West    :129   1st Qu.:49.00    1st Qu.:0.5032   1st Qu.:0.5000  
    ##  Kendrick Lamar: 97   Median :57.00    Median :0.5965   Median :0.6430  
    ##                       Mean   :52.16    Mean   :0.5918   Mean   :0.6105  
    ##                       3rd Qu.:65.00    3rd Qu.:0.6815   3rd Qu.:0.7610  
    ##                       Max.   :89.00    Max.   :0.9590   Max.   :0.9220  
    ##                                                                         
    ##       key        loudness          mode      speechiness    
    ##  C#     :53   Min.   :-21.487   major:141   Min.   :0.0285  
    ##  B      :30   1st Qu.: -8.959   minor:119   1st Qu.:0.0940  
    ##  G      :27   Median : -6.947               Median :0.2325  
    ##  F      :24   Mean   : -7.689               Mean   :0.2609  
    ##  F#     :22   3rd Qu.: -5.599               3rd Qu.:0.3578  
    ##  A#     :21   Max.   : -1.614               Max.   :0.9560  
    ##  (Other):83                                                 
    ##   acousticness      instrumentalness       liveness         valence      
    ##  Min.   :0.000259   Min.   :0.0000000   Min.   :0.0541   Min.   :0.0300  
    ##  1st Qu.:0.054125   1st Qu.:0.0000000   1st Qu.:0.1237   1st Qu.:0.2990  
    ##  Median :0.194500   Median :0.0000000   Median :0.1990   Median :0.4400  
    ##  Mean   :0.289753   Mean   :0.0216612   Mean   :0.2767   Mean   :0.4476  
    ##  3rd Qu.:0.442000   3rd Qu.:0.0000218   3rd Qu.:0.3762   3rd Qu.:0.5923  
    ##  Max.   :0.985000   Max.   :0.9740000   Max.   :0.9720   Max.   :0.9640  
    ##                                                                          
    ##      tempo         duration_ms     time_signature      key_mode  
    ##  Min.   : 48.34   Min.   : 19133   Min.   :1.000   C# major: 38  
    ##  1st Qu.: 85.81   1st Qu.:185610   1st Qu.:4.000   B minor : 24  
    ##  Median :102.10   Median :235504   Median :4.000   G major : 22  
    ##  Mean   :110.75   Mean   :235908   Mean   :3.981   A# minor: 15  
    ##  3rd Qu.:128.60   3rd Qu.:280088   3rd Qu.:4.000   C# minor: 15  
    ##  Max.   :193.02   Max.   :727107   Max.   :5.000   F minor : 15  
    ##                                                    (Other) :131

### Distribution of popularity of tracks by artists

The dependent variable that I am interested in for this analysis is the popularity of each track. What I hope to learn is for these artists which of the independent variables have the biggest impact on the success of the track. The box plots below show the distribution of popularity for each track by the three artists. The popularity of a track is measured between 0 and 100 and is based on the number of plays and the recency of those plays. Learn more about this metric and the other metrics collected from the API [here](https://developer.spotify.com/documentation/web-api/reference/tracks/get-track/)

I think the difference in track popularity by these three artists is very interesting. Frank Ocean's two albums are distinctively different, all of Kendrick Lamar's tracks appears to be overall evenly popular, and Kanye Wests tracks have the widest spread.

``` r
dissect%>%
  #filter(artist == 'Frank Ocean')%>%
  ggplot(aes(x=artist ,y=track_popularity))+
  geom_boxplot()+
  geom_jitter(aes(color=as.factor(dissected))) +
  theme(plot.subtitle = element_text(vjust = 1),
    plot.caption = element_text(vjust = 1),
    plot.title = element_text(size = 24),
    legend.position = "bottom", legend.direction = "horizontal") +labs(title = "Distribution of track popularity by artist",
    y = "Track Popularity", colour = NULL) + theme(panel.grid.major = element_line(size = 1),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12, colour = "gray4"),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA)) +labs(x = "Artist")
```

<p><img src="http://danlarson.io/static/img/track_plot-1.png" alt="Track plot" /></p>

In an attempt to better understand the popularity measure, I will use many of the other features available from the Spotify API. The ridgeplots below shows the differences in each variable by artist. You can see that for many of the variables there are little differences in the distribution by artists.

``` r
require(ggridges)

dissect%>%
  select(artist,album_name,danceability,energy,speechiness,acousticness,instrumentalness,liveness,valence)%>%
  gather(key='key',value='value',-album_name,-artist)%>%
  ggplot(aes(x=value,y=reorder(key,value),fill=artist))+
    geom_density_ridges(scale=1,alpha=.4)+
  theme_ridges()+
  ggtitle('Distribution of track features by artist') + theme(legend.position = "bottom",plot.caption = element_text(vjust = 1),
    plot.title = element_text(size = 24),  legend.direction = "horizontal") +labs(x = NULL, y = "Feature", fill = "Artist")
```

<p><img src="http://danlarson.io/static/img/ridgeplot1-1.png" alt="Ridge Plot" /></p>

For a slightly different view of the data above here is a box-plot of each variable with each data point jittered over the plot.

``` r
dissect%>%
  select(artist,album_name,danceability,energy,speechiness,acousticness,instrumentalness,liveness,valence)%>%
  gather(key='key',value='value',-album_name,-artist)%>%
  ggplot(aes(x=key,y=value))+
  geom_boxplot()+
  geom_jitter(aes(color=artist))+
   theme(plot.subtitle = element_text(vjust = 1),
    plot.caption = element_text(vjust = 1),
    plot.title = element_text(size = 24),
    legend.position = "bottom", legend.direction = "horizontal") +labs(title = "Distribution of track features by artist",
    y = "Metric", colour = NULL) + theme(panel.grid.major = element_line(size = 1),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12, colour = "gray4"),
    panel.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA)) +labs(x = "feature")
```
<p><img src="http://danlarson.io/static/img/track_features_box-1.png" alt="features box-plot" /></p>


``` r
dissect%>%
  #filter(artist == 'Frank Ocean')%>%
  ggplot(aes(x=tempo,y=artist,fill=artist))+
    geom_density_ridges2(alpha=.5,scale=6)+
  theme_ridges()+
  ggtitle('Tempo of tracks by artist')+ theme(legend.position = "none", legend.direction = "horizontal",plot.caption = element_text(vjust = 1),
    plot.title = element_text(size = 24) ) +labs(x = NULL, y = "", fill = "Artist")
```
<p><img src="http://danlarson.io/static/img/tempo_ridge-1.png" alt="tempo plot" /></p>

### Correlation Matrix

The last thing that I am going to do with these data this week is to look for correlations between the variables. The correlation plot below shows which variables are correlated, how correlated, and whether or not the correlation is positive or negative. Look at what correlates to the track popularity metric, we will want to keep an eye on liveness, duration, instrumentalness, and tempo.

``` r
require(corrplot)
require(ggcorrplot)

d2 <-dissect%>%
  select(tempo,duration_ms,danceability,energy,speechiness,acousticness,instrumentalness,liveness,valence,track_popularity)

corr<-cor(d2)

ggcorrplot(corr, hc.order = TRUE,
     outline.col = "white",
     type = "lower",method='circle',
     colors = c("#6D9EC1", "white", "#E46726")) +
  theme(plot.title = element_text(size = 16),
    panel.background = element_rect(fill = NA)) +
  labs(title = "Correlation matrix of the spotify track features")
```
<p><img src="http://danlarson.io/static/img/Correlation-1.png" alt="Correlation Plot" /></p>

### Next steps

Next week I will utilize a logistic regression technique to understand which of the metrics can help explain the popularity of each track.
