This is an exercise showing what exciting research can be done with open software and open data. Here we are going to look at the pre-release of EVS 2018:

EVS (2018): European Values Study 2017: Integrated Dataset (EVS 2017). GESIS Data Archive, Cologne. ZA7500 Data file Version 1.0.0, <doi:10.4232/1.13090>

You can freely donwload the data here: <https://dbk.gesis.org/dbksearch/sdesc2.asp?no=7500&db=e&doi=10.4232/1.13090>

``` r
# Clean data --------------------------------------------------------------


# vars of interest
vars <- c("id_cocas", "country", "c_abrv",
          "age", "v225",
          "v39", "v75", "v102", "v133", "v142", "v171",
          "v184", "v203")


# select variables and code missing
evs_small <- evs %>% 
  select(vars) %>% # select variables
  mutate_all(funs(ifelse(. < 0, NA, .))) # code missing
```

``` r
# get full names in new variable
evs_small$cntry <- relabel(evs$country)


# get world map data
world <- map_data("world")


# summarize data at country level and clean vote var.
cntry_info <- evs_small %>% 
  group_by(cntry) %>% 
  select(v39:v203) %>%
  mutate(v171 = ifelse(v171 == 7 , NA, v171),
         v171 = 3 - v171) %>% #reverse
  summarize_all(mean, na.rm = T)
```

    ## Adding missing grouping variables: `cntry`

``` r
# join data
evs_map <- world %>% 
  left_join(cntry_info, by = c("region" = "cntry"))


theme_info <- 
  theme(axis.line = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme_tufte() 
```

``` r
p <- ggplot() + geom_polygon(data = evs_map, 
                        aes(x = long, 
                            y = lat, 
                            group = group,
                            fill = v39),
                        color = "white") + 
    xlim(-25, 47.0) + ylim(35, 70) +
   labs(fill = "Satisfied",
       title = "Distribution of satisfaction",
       subtitle = "(Dissatisfied = 1 ... 10 = Satisfied)") +
  scale_fill_gradientn(colors = c("#ff6138", "#00A388"), 
                       values = scales::rescale(c(min(cntry_info$v39),
                                  mean(cntry_info$v39), 
                                  max(cntry_info$v39)))) 


p + theme_info
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
ggsave("./output/evs_2017_satisfaction.png", dpi = 500)
```

    ## Saving 7 x 5 in image

``` r
p <- ggplot() + 
  geom_polygon(data = evs_map,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = v75),
               color = "white") +
  xlim(-25, 47.0) + ylim(35, 70) +
  labs(x = "", y = "", fill = "Disagree strongly",
       title = "Man's job is to earn money, woman's job is to look after home and family",
       subtitle = "(Strongly agree = 1 ... 4 = Disagree strongly)",
       caption = "Data: European Values Study 2017") +
  scale_fill_gradientn(colors = c("#ff6138", "#00A388"), 
                       values = scales::rescale(c(min(cntry_info$v75),
                                  mean(cntry_info$v75), 
                                  max(cntry_info$v75))))
                             

p + theme_info
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggsave("./output/evs_2017_man_job.png", dpi = 500)
```

    ## Saving 7 x 5 in image

``` r
p <- ggplot() + geom_polygon(data = evs_map, 
                        aes(x = long, 
                            y = lat, 
                            group = group,
                            fill = v102),
                        color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  labs(x = "", y = "", fill = "Right",
       title = "Position on left-right scale",
       subtitle = "(Left = 1 ... 10 = Right)",
       caption = "Data: European Values Study 2017") +
  scale_fill_gradientn(colors = c("#ff6138", "#00A388"), 
                       values = scales::rescale(c(min(cntry_info$v102),
                                  mean(cntry_info$v102), 
                                  max(cntry_info$v102)))) 


p + theme_info
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ggsave("./output/evs_2017_left_right.png", dpi = 500)
```

    ## Saving 7 x 5 in image

``` r
p <- ggplot() + geom_polygon(data = evs_map, 
                        aes(x = long, 
                            y = lat, 
                            group = group,
                            fill = v133),
                        color = "white") +
  xlim(-25, 47.0) + ylim(35, 70) +
  labs(x = "", y = "", fill = "Essential for democracy",
       title = "Governments tax the rich and subsidize the poor",
       subtitle = "(Against democracy = 0 ... 10 = Essential for democracy)",
       caption = "Data: European Values Study 2017") +
  scale_fill_gradientn(colors = c("#ff6138", "#00A388"), 
                       values = scales::rescale(c(min(cntry_info$v133),
                                  mean(cntry_info$v133), 
                                  max(cntry_info$v133)))) 


p + theme_info
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggsave("./output/evs_2017_tax.png", dpi = 500)
```

    ## Saving 7 x 5 in image

``` r
p <- ggplot() + geom_polygon(data = evs_map, 
                        aes(x = long, 
                            y = lat, 
                            group = group,
                            fill = v142),
                        color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  labs(x = "", y = "", fill = "Absolutely important",
       title = "Importance of democracy",
       subtitle = "(Not at all important = 0 ... 10 = Absolutely important)",
       caption = "Data: European Values Study 2017") +
  scale_fill_gradientn(colors = c("#ff6138", "#00A388"), 
                       values = scales::rescale(c(min(cntry_info$v142),
                                  mean(cntry_info$v142), 
                                  max(cntry_info$v142)))) 


p + theme_info
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ggsave("./output/evs_2017_democracy.png", dpi = 500)
```

    ## Saving 7 x 5 in image

``` r
p <- ggplot() + geom_polygon(data = evs_map, 
                        aes(x = long, 
                            y = lat, 
                            group = group,
                            fill = v171),
                        color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  labs(x = "", y = "", fill = "Always",
       title = "Usually vote at local elections",
       subtitle = "(Never = 1 ... 3 = Always)",
       caption = "Data: European Values Study 2017") +
  scale_fill_gradientn(colors = c("#ff6138", "#00A388"), 
                       values = scales::rescale(c(min(cntry_info$v171),
                                  mean(cntry_info$v171), 
                                  max(cntry_info$v171)))) 


p + theme_info
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggsave("./output/evs_2017_vote.png", dpi = 500)
```

    ## Saving 7 x 5 in image

``` r
p <- ggplot() + geom_polygon(data = evs_map, 
                        aes(x = long, 
                            y = lat, 
                            group = group,
                            fill = v184),
                        color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  labs(x = "", y = "", fill = "Very good",
       title = "Impact of immigrants on country development",
       subtitle = "(Very bad = 1 ... 5 = Very good)",
       caption = "Data: European Values Study 2017") +
  scale_fill_gradientn(colors = c("#ff6138", "#00A388"), 
                       values = scales::rescale(c(min(cntry_info$v184),
                                  mean(cntry_info$v184), 
                                  max(cntry_info$v184)))) 


p + theme_info
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggsave("./output/evs_2017_immigrants.png", dpi = 500)
```

    ## Saving 7 x 5 in image

``` r
p <- ggplot() + geom_polygon(data = evs_map, 
                        aes(x = long, 
                            y = lat, 
                            group = group,
                            fill = v203),
                        color = "white") + 
  xlim(-25, 47.0) + ylim(35, 70) +
  labs(x = "", y = "", fill = "Disagree strongly",
       title = "Environmental threats are exaggerated",
       subtitle = "(Strongly agree = 1 ... 5 = Disagree strongly)",
       caption = "Data: European Values Study 2017") +
  scale_fill_gradientn(colors = c("#ff6138", "#00A388"), 
                       values = scales::rescale(c(min(cntry_info$v203),
                                  mean(cntry_info$v203), 
                                  max(cntry_info$v203)))) 


p + theme_info
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
ggsave("./output/evs_2017_environment.png", dpi = 500)
```

    ## Saving 7 x 5 in image