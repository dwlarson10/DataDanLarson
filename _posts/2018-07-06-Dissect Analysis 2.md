---
layout: post
title:  "Analysis of Dissect Artists post 2: Linear regression"
tags:
- Goals
- Learning
- Data science
- sods18
- regression
---

#### The project

In my [post last week](http://danlarson.io/Dissect-analysis-gettng-data.html) I explored the data for the three artists that have been analyzed by the Dissect Podcast. The initial analysis uses data available through the Spotify API. Before moving on to analyzing the actual lyrics of these artists, I am going to determine which of the features available through Spotify have the largest impact on track popularity.

Before building the model lets look at some plots. Last week, I read this article on [bee swarm plots](https://aghaynes.wordpress.com/2018/06/28/beeswarms-instead-of-histograms/) and thought I would give it a try. There are several packages available to build bee swarm plots but the package ggbeeswarm interacts with ggplot which is a huge plus for me. The bee swarm plot is another method of displaying distribution. Each dot represents a track and in instances where the dots are densely distributed a randomness spreads them out to create a shape similar to a violin plot. What I like about this plot is that each track is represented and you get to see a shape represented in the data. The first bee swarm shows the distribution of track popularity by artist. The shape of the points indicates if the track was on the album analyzed on the Dissect Podcast.

``` r
library(ggbeeswarm)
ggplot(dissect,aes(y=artist, x=track_popularity,color=artist,shape=factor(dissected))) +
  geom_quasirandom(groupOnX=FALSE) + theme(plot.subtitle = element_text(vjust = 1),
    plot.caption = element_text(vjust = 1),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13),
    plot.title = element_text(size = 22),
    panel.background = element_rect(fill = NA),
    legend.position = "none", legend.direction = "horizontal") +labs(title = "Distribution of track popularity by artist",
    x = "Track popularity", y = NULL, colour = "Artist",
    shape = "Dissected", caption = "Track popularity based on Spotify popularity metric")
```

<p><img src="http://danlarson.io/static/img/BeeswarmPopularity-1.png" alt="Beeswarm Track Popularity" /></p>


Okay... Okay... lets do one more bee swarm for fun. Here are the track features presented as a bee swarm plot. While I really like joy, plots as presented in the post last week, I think this plot really tells us a more interesting story about the different features.

``` r
require(ggridges)

dissect%>%
  select(artist,album_name,danceability,energy,speechiness,acousticness,instrumentalness,liveness,valence)%>%
  gather(key='key',value='value',-album_name,-artist)%>%
  ggplot(aes(x=value,y=reorder(key,value),color=key))+
  geom_quasirandom(groupOnX=FALSE)+
  ggtitle('Distribution of track features')  + theme(plot.subtitle = element_text(vjust = 1),
    plot.caption = element_text(vjust = 1),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 24),
    panel.background = element_rect(fill = NA),
    legend.position = "none") +labs(x = "Value", y = NULL, colour = NULL)
```

<p><img src="http://danlarson.io/static/img/BeeswarmTrackFeatures-1.png" alt="Beeswarm track features" /></p>

The next step is to fit a linear model to the data. At this point, I am only using the subset of the variables from spotify to determine track popularity. In the future we are going to collect additional data to see if we can improve our understanding of what makes a track popular.

Looking at the output, you can see that the model is significant and that it has an r-squared adjusted of .22. We can interpret this as the model can explain roughly 22% of the variance in track popularity of these artists. Looking at the coefficients we can determine that danceability and the metric we created of Dissected are the most significant. The estimates give as an estimation of the impact of each variable. For instance. as the danceability of a track goes up so will it's popularity. in contrast, more instrumental tracks will be less popular.

``` r
options(scipen=999)
fit <- lm(track_popularity ~
            danceability +
            energy +
            loudness +
            speechiness +
            acousticness +
            instrumentalness +
            liveness +
            valence +
            tempo+
            duration_ms+dissected,
          data=dissect)
summary(fit)
```

    ##
    ## Call:
    ## lm(formula = track_popularity ~ danceability + energy + loudness +
    ##     speechiness + acousticness + instrumentalness + liveness +
    ##     valence + tempo + duration_ms + dissected, data = dissect)
    ##
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max
    ## -58.212  -5.280   3.035  10.947  38.734
    ##
    ## Coefficients:
    ##                      Estimate   Std. Error t value   Pr(>|t|)    
    ## (Intercept)       36.74883195  13.39781346   2.743   0.006534 **
    ## danceability      32.69493122   9.35540820   3.495   0.000562 ***
    ## energy            -3.16565101   9.71602021  -0.326   0.744837    
    ## loudness           1.40318996   0.57331808   2.447   0.015080 *  
    ## speechiness        9.77079925   6.50827013   1.501   0.134553    
    ## acousticness       7.59052023   5.27337993   1.439   0.151297    
    ## instrumentalness -17.68863633  10.87018871  -1.627   0.104951    
    ## liveness         -17.81113245   5.93487767  -3.001   0.002965 **
    ## valence          -20.10177966   6.16897895  -3.259   0.001277 **
    ## tempo              0.11653480   0.03730598   3.124   0.001998 **
    ## duration_ms        0.00001199   0.00001258   0.953   0.341610    
    ## dissected         14.75021865   3.16983344   4.653 0.00000532 ***
    ## ---
    ## Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1
    ##
    ## Residual standard error: 18.13 on 248 degrees of freedom
    ## Multiple R-squared:  0.255,  Adjusted R-squared:  0.222
    ## F-statistic: 7.718 on 11 and 248 DF,  p-value: 0.00000000001845

The plot below shows the estimates and confidence intervals. You can see that danceability has the largest positive effect and valence and liveness have the largest negative effect on track popularity.

``` r
library(ggplot2)
require(broom)
td <- tidy(fit, conf.int = TRUE)
ggplot(td, aes(estimate, reorder(term,estimate), color = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))  + theme(plot.subtitle = element_text(vjust = 1),
    plot.caption = element_text(vjust = 1),
    panel.grid.major = element_line(colour = "gray94"),
    panel.grid.minor = element_line(linetype = "blank"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 24),
    panel.background = element_rect(fill = NA),
    legend.position = "none") +labs(title = "Term estimates for track popularity model",
    y = NULL, colour = NULL)
```
<p><img src="http://danlarson.io/static/img/TermEstimates-1.png" alt="TermEstimates" /></p>

#### Evaluating Model

The plots below can help us interpret how well our model is fitted. The first plot, residuals vs fitted, helps identify whether there is a non-linear relationship in the data. in other words, you hope that there is no distinct pattern in the residuals. In these data there may be a non-linear relationship that we need to explore.

The next plot, Normal Q-Q, is again used to determine how normally distributed the residuals are. With this plot we are hoping that the residuals fall on as close to a straight line as possible. In this instance there is something concerning in the data that cause the residuals to shift about one-third up the plot. .

Scale-Location allows us to make sure the residuals are spread along the range of predictors. The fact that there is a slightly negatively sloping line again suggests that there is something in our model that we need to explore.

Finally, the Residuals vs Leverage plot helps to identify any cases that may be outliers that impact the model. We are looking for cases in the upper or lower right hand corners of the plot. You can see that there are at least three cases that we should explore more closely.

``` r
library(ggfortify)
```

    ## Warning: package 'ggfortify' was built under R version 3.4.4

``` r
autoplot(fit,label.size = 3)+
  theme_bw()
```

<p><img src="http://danlarson.io/static/img/Evaluation-1.png" alt="Evaluation" /></p>

#### Next steps

The next steps with this analysis will be to understand the outliers and try to build a model that fits our data better. To get there we will add some additional variables that can help understand what makes a track popular for these three artists. I will be extracting the lyrics for each artists songs using the Rap Genius API and doing some text analysis to improve our understanding of these artists.
