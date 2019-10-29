Iteration and List Columns
================
Olya Besedina

``` r
set.seed(1)

l = list(vec_numeric = 5:8,
         mat         = matrix(1:8, 2, 4),
         vec_logical = c(TRUE, FALSE),
         summary     = summary(rnorm(1000)))
```

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
# pull out second thing on the list
l[[2]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

``` r
mean(l$vec_numeric)
```

    ## [1] 6.5

``` r
df = list(
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

df$a
```

    ##  [1] 4.134965 4.111932 2.129222 3.210732 3.069396 1.337351 3.810840
    ##  [8] 1.087654 1.753247 3.998154 2.459127 2.783624 1.378063 1.549036
    ## [15] 3.350910 2.825453 2.408572 1.665973 1.902701 5.036104

``` r
df[[2]]
```

    ##  [1] -1.63244797  3.87002606  3.92503200  3.81623040  1.47404380
    ##  [6] -6.26177962 -5.04751876  3.75695597 -6.54176756  2.63770049
    ## [11] -2.66769787 -1.99188007 -3.94784725 -1.15070568  4.38592421
    ## [16]  2.26866589 -1.16232074  4.35002762  8.28001867 -0.03184464

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

``` r
mean_and_sd(df[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12

``` r
mean_and_sd(df[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.416  4.08

``` r
mean_and_sd(df[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.191

``` r
mean_and_sd(df[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.43  1.18

create loops

``` r
output = vector("list", length = 4)
```

Create first loop

``` r
for (i in 1:4) {
  
  output[[i]] = mean_and_sd(df[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.416  4.08
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.191
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.43  1.18

Change for loop to map statement

``` r
output = map(df, mean_and_sd)

output_median = map (df, median)

output_summary = map(df, summary)

output = map_dbl(df, median)
output = map_dfr(df, mean_and_sd)
```

``` r
output = map(df, -mean_and_sd(.x))
```

Napoleon

``` r
library(rvest)

read_page_reviews = function(url) {
  
  h = read_html(url)
  
  title = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  text = h %>%
    html_nodes(".review-data:nth-child(5)") %>%
    html_text()
  
  data_frame(title, stars, text)
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

vec_url = str_c(url_base, 1:5)

read_page_reviews(vec_url[[1]])
```

    ## # A tibble: 10 x 3
    ##    title                     stars text                                    
    ##    <chr>                     <dbl> <chr>                                   
    ##  1 "Gotta watch it!\n      …     5 "Super fun cult film. A must-see! Funni…
    ##  2 "Great movie\n          …     5 "Love this movie.\n            "        
    ##  3 "Duh\n            "           5 "Best movie ever\n            "         
    ##  4 "Great video\n          …     5 "Product as described.  Great transacti…
    ##  5 "Give me some of your to…     5 "This movie will always be my favorite …
    ##  6 "Nostalgic\n            "     5 "One of the best nostalgic movies of my…
    ##  7 "Make you giggle type mo…     5 "I love, love, love this movie.  It mak…
    ##  8 "This movie is so stupid…     5 "No, really.  It's so stupid.  Your IQ …
    ##  9 "Hilarious\n            "     5 "Hilarious\n            "               
    ## 10 "Waste of money\n       …     1 "Terrible movie! Please don’t waste you…

``` r
read_page_reviews(vec_url[[2]])
```

    ## # A tibble: 10 x 3
    ##    title                         stars text                                
    ##    <chr>                         <dbl> <chr>                               
    ##  1 "Good movie\n            "        5 "Funny\n            "               
    ##  2 "A classic\n            "         5 "I like your sleeves. They're real …
    ##  3 "FRIKKEN SWEET MOVIE, GAWSH.…     5 "It’s Napolean Dynamite. It’s charm…
    ##  4 "You gonna eat the rest of y…     5 "One of my favorite movies ever.  Y…
    ##  5 "Tina you fat lard come get …     5 "It's a great movie\n            "  
    ##  6 "Great family movie\n       …     5 "My kids as well as the adults love…
    ##  7 "Teens love it\n            "     5 "Original and funny\n            "  
    ##  8 "Great\n            "             5 "Funny\n            "               
    ##  9 "Great Movie, Bad Packaging\…     4 "First off, the stick-on label on t…
    ## 10 "jeez napoleon\n            "     5 "gosh\n            "

``` r
read_page_reviews(vec_url[[3]])
```

    ## # A tibble: 10 x 3
    ##    title                       stars text                                  
    ##    <chr>                       <dbl> <chr>                                 
    ##  1 "👍\n            "              5 "👍\n            "                    
    ##  2 "A classic!\n            "      5 "A classic movie.  Hilarious!\n      …
    ##  3 "A must own\n            "      5 "Great movie\n            "           
    ##  4 "If you like 80s ...you mu…     5 "My all time favorite movie. I have w…
    ##  5 "🤘\n            "              5 "🤘\n            "                    
    ##  6 "Super Slow Mooovie...\n  …     1 "Too slow and too damn quiet... My gi…
    ##  7 "Awesome!\n            "        5 "Love this movie !\n            "     
    ##  8 "Very funny\n            "      4 "Very funny\n            "            
    ##  9 "Eat your food tina\n     …     5 "Cant go wrong\n            "         
    ## 10 "Dumb funny\n            "      5 "Dumb funny\n            "

``` r
read_page_reviews(vec_url[[4]])
```

    ## # A tibble: 10 x 3
    ##    title                           stars text                              
    ##    <chr>                           <dbl> <chr>                             
    ##  1 "Annoying! Not in a good way.\…     1 "I know that I am one of the very…
    ##  2 "Fun\n            "                 5 "Fun\n            "               
    ##  3 "such a great movie\n         …     5 "a true comedy classic\n         …
    ##  4 "Napoleon Dud\n            "        3 "Not impressed w/movie.\n        …
    ##  5 "Five stars\n            "          5 "Such a weird, awesome movie\n   …
    ##  6 "Fun!\n            "                5 "Great movie\n            "       
    ##  7 "Funny movie- bravo for Amazon…     5 "My son loves this movie, so I wa…
    ##  8 "Movie\n            "               5 "Movie\n            "             
    ##  9 "Funny movie, quotable lines\n…     5 "My kids quote this movie all the…
    ## 10 "Great for teenagers!\n       …     5 "My students loved this movie.\n …

``` r
read_page_reviews(vec_url[[5]])
```

    ## # A tibble: 10 x 3
    ##    title                        stars text                                 
    ##    <chr>                        <dbl> <chr>                                
    ##  1 "can't believe we fell for …     1 "a pretty lame movie--can't believe …
    ##  2 "shut up tina you fat lard.…     5 "i LOVE napoleon.\n            "     
    ##  3 "Laughter is the Best Medic…     5 "FAST SHIPPING! Love this Movie! Lau…
    ##  4 "New condition\n           …     5 "Classic for the kids to watch.\n   …
    ##  5 "Napoleon, give me some of …     5 "Cul\n            "                  
    ##  6 "Yes rent\n            "         5 "Always an amazing movie, classic!\n…
    ##  7 "Cult classic.\n           …     5 "I should’ve bought this movie a lon…
    ##  8 "DIDN'T WORK\n            "      1 "I paid for the rental, but it's not…
    ##  9 "I\n            "                5 "I love this movie! My kids love thi…
    ## 10 "Laugh out loud\n          …     5 "Introduced my grandsons to this mov…

``` r
#write a loop

output = vector("list", 5)

for (i in 1:5) {
  output[[i]] = read_page_reviews(vec_urls[[i]])
}
```

Write a loop

1.create empty output list 2.create a loop

Map

``` r
output = map(vec_urls, read_page_reviews)
```
