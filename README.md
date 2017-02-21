See documents/learn_purrr.Rmd for the full version 

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(purrr)
library(repurrrsive)
library(stringr)
library(ggthemes)
library(gapminder)
library(modelr)


theme_update(text =  element_text(family = 'Arial Narrow',
                                  size = 16))

```

The goal of this workshop and document is to get you familiar with the workings of the `purrr` package. There's of course a lot more than can be covered here, but hopefully this should get you familiar with

1. Using `purrr` as a replacement for `apply`

2. Using `purrr` to wrangle lists

3. Using `purrr` for data analysis

Full credit to Jenny Bryan's [excellent `purrr` tutorial](https://jennybc.github.io/purrr-tutorial/) for many of the ideas here, along with Hadley Wickham & Garret Grolemund's [R for Data Science](http://r4ds.had.co.nz). My goal is to walk you through some of the concepts outlined in these (much better) resources, and expand on some particular applications that have been useful to me.


## What is `purrr`?


`purrr` is a part of the tidyverse, taking on the tasks accomplished by the `apply` suite of functions in base R. It's great at applying operations across many dimensions of your data, improving your ability to keep even complex analyses "tidy". At it's simplest, it's just another way to use `apply`. At it's most complex, it allows you to easily move around in and manipulate multi-dimensional (and multi-type) data.

There are a whole suite of functions involved with `purrr`, but the goal of this tutorial is to get the fundamentals down so that you can start incorporating `purrr` into your own code and explore higher-level abilities on your own.

### Key verbs

- `map` is the workhorse of the `purrr` family. It is basically `apply`
- The basic syntax:

`map("Lists to apply function to","Function to apply across lists","Additional parameters")`

```{r}

map(mtcars, mean, na.rm = T)

```



`map` by default returns a list

`map_TYPE` returns an object of class TYPE

`map_lgl` returns logical objects

`map_df` returns data frames, etc.

Specifying type makes it easier to wrangle different types of outputs

`map` applies a function over one list.

`map2` applies a function over combinations of two lists in the form

`map2(list1, list2, function, ...)`

```{r}

map2_chr(c('one','two','red','blue'), c('fish'), paste)

```

Under earlier versions, `map3` was for 3 lists, etc.

Now, anything above two lists is handled by `pmap`

```
pmap(list(list1,list2,list3), function,...)

```

This is where things start to improve substantially on `apply`: instead of having to smash all the things you want to apply a function over into one data frame, you can easily pass multiple named inputs to a function

Let's start with the `got_chars` data from the `repurrrsive` package

```{r}
library(repurrrsive)
str(got_chars, list.len = 3)
```


Suppose we wanted to figure out how many aliases and alliances each character has, as well as where they were born. We can use `pmap` to apply a function over each of these attributes

```{r}
got_list <- got_chars %>%
  map(`[`, c('name','aliases','allegiances','born'))

got_list <-  got_chars %>% {
  list(
    name = map_chr(.,'name'),
    aliases = map(.,'aliases'),
    allegiances = map(.,'allegiances'),
    born = map_chr(.,'born')
  )
}

str(got_list, list.len = 3)

got_foo <- function(name, aliases, allegiances,born){

  paste(name, 'has', length(aliases), 'aliases and', length(allegiances),
        'allegiances, and was born in', born)

}

got_list %>%
  pmap_chr(got_foo) %>%
  head()

```


Getting used to using `map` is the foundation to working with `purrr`.

We'll now go into more detail on ways to use this in a practical way.


## Functional programming


`purrr` is designed to help with "functional programming", which you can take broadly as trying to use functions (preferably "pure" ones) to accomplish most of your complex and repetitive tasks (don't copy and paste more then 3 times - H. Wickham)

As a very quick reintroduction to functions:

  Functions in R take any number of named inputs, do operations on them, and returns the last thing produced inside the function (or whatever you specify using `return`)

    ```{r}

z <- 10

foo <- function(x,y) {

  z <- x + y

  return(z)
}

foo(2,3)

z
```

Notice that `z` wasn't affected by the `z` inside the function. Operations inside functions happen in the local environment of that function. "What happens in the function, stays in the function (except what you share on Facebook)"

Note though that functions can "see" objects in the global environment

```{r}

a <- 10

foo <- function(x,y) {

z <- x + y + a

return(z)
}

foo(2,3)


```

I strongly recommend you avoid relying on global variables inside functions: it can very easily cause unintended and sneaky behavior.

Functions can be as complicated as you want them to be, but a good rule is to try and make sure each function is good at doing *one* thing. That doesn't mean that that "one thing" can't be a complex series of operation, but the objective of the function should be to produce that one thing well. E.g., write one function to process your data, one function to run your model, one function to diagnose your model, etc.

You can also use "anonymous" functions in R/`purrr`. This is basically a shortcut for when you don't want to take up the space of writing and saving a whole function somewhere. You make anonymous functions with `~`

Say you want the coefficient of variation of each of the variables in `mtcars`


```{r}

foo <- function(x){

  sd(x) / mean(x)

}

map(mtcars, foo)

```

Can be accomplished using

```{r}

map(mtcars, ~ sd(.) / mean(.))

```

Notice the rather confusing `.` in there. `.` basically is a marker for whatever data is passed to a function/thing in R.

```{r}
mtcars %>%  {
  .$mpg
}
```

The `{` after the pipe tells R that the things inside the brackets don't take the data passed to it as its first argument

```{r}

x <- c(1,1,NA,NA)

y <-  c(1, NA, 1, NA)

z <-  data_frame(x = x, y = y)

myfoo <- function(x,y){


out <- (is.na(x) & is.na(y))

}


map(z, sum)

z %>%
mutate(both_na = map2_lgl(x,y,myfoo))

z %>%
mutate(both_na = map2_lgl(x,y, ~ is.na(.x) & is.na(.y)))

z %>%
mutate(both_na = pmap_lgl(list(x = x,y = y), myfoo))


```


## Wrangling lists

Let's get back to actually using `purrr` in practice.


Lists are powerful objects that allow you to store all kinds of information in one place.

They can also be a pain to deal with, since we are no longer in the nice 2-D structure of a traditional data frame.

`purrr` has all kinds of useful tools for helping you quickly and efficiently deal with parts of lists.

Let's start with the Game of Thrones database in the `repurrrsive` package.

The `str` function is a great way to get a first glimpse at a list's structure

```{r}

str(got_chars, list.len =  3)

```

For those of you who prefer a more interactive approach, you can also use the `jsonedit` function in the `listviewer` package


```{r}

listviewer::jsonedit(got_chars)

```


So, how do we start poking around in this database?

Suppose we wanted only the first 5 characters named

```{r}

got_chars[1:5]

```

Now, suppose that we jut want to look at the name of the first 5 characters

```{r}

got_chars[1:5] %>%
  map_chr('name')

```

If you're like me, the numeric indexing of each of the entries is currently driving you nuts: I'd rather have each element in the list be named by the character it refers to

```{r}

got_chars[1:5] %>%
  set_names(map_chr(.,'name')) %>%
  listviewer::jsonedit()

```

Much better!

  Now, suppose that I want more than just the names

```{r}

got_chars[1:5] %>%
  set_names(map_chr(.,'name')) %>%
  map(c('name','allegiances')) %>%
  listviewer::jsonedit()

```

Huh, why didn't that work? `purrr` has some built in helpers for simple operations, but as things get more complicated you need to specify functions

```{r}
got_chars[1:5] %>%
set_names(map_chr(., 'name')) %>%
map(`[`, c('name', 'allegiances')) %>%
listviewer::jsonedit()
```

Now, let's say that I want to get all the Lanisters, so I can see which people to hate.

This is where a lot of the power of `purrr` starts to come in, allowing you to easily apply functions across nested layers of a list

```{r}
got_chars %>%
  set_names(map_chr(.,'name')) %>%
  map(`[`,c('name','allegiances')) %>%
  keep(~str_detect(.$name, 'Lannister')) %>%
  listviewer::jsonedit()
```

Now, suppose that we want anyone who's allied with the Starks

```{r}
got_chars[1:4] %>%
set_names(map_chr(.,'name')) %>%
map(`[`,c('name','allegiances')) %>%
map(~str_detect(.$allegiances, 'Stark'))
```

Hmmm, that doesn't look good, what's up with Will? What happens if I try and use `keep` (list `filter`) here?

```{r, eval = F}

got_chars %>%
set_names(map_chr(.,'name')) %>%
map(`[`,c('name','allegiances')) %>%
keep(~str_detect(.$allegiances, 'Stark'))

```

Can fix that with a bit of a hack. There's almost certainly a better way, but this just shows that things get a little more complicated when you're trying to apply functions across list objects; things like dimensions, types, NULLs, can cause problems. If I'm trying something new, I'll usually try and develop the methods on a subset of the list that I know is "ideal", make sure it works there, and then try the operation on progressively more complicated lists. That allows me to separate errors in my functions vs. problems reading in "odd" data types.

```{r}

got_chars %>%
set_names(map_chr(.,'name')) %>%
map(`[`,c('name','allegiances')) %>%
keep(~ifelse(length(.$allegiances) > 0, str_detect(.$allegiances, 'Stark'),FALSE)) %>%
listviewer::jsonedit()

```
As Cersei likes to remind us, anyone who's not a Lannister is an enemy to the Lannisters. Let's look at all the POV characters that aren't allied to the Lannisters

```{r}
got_chars %>%
  set_names(map_chr(.,'name'))  %>%
  map(`[`,c('name','allegiances')) %>%
  discard(~ifelse(length(.$allegiances) > 0, str_detect(.$allegiances, 'Lannister'),FALSE)) %>%
  listviewer::jsonedit()

```

Things obviously get a lot more complicated than this, but hopefully that gives you an idea of how to manipulate lists using `purrr`

The last thing we might want to go over is converting to and from data frames and lists. Life is obviously easier with data frames, and a lot of the time we can massage aspects of lists that we care about into data frames (especially using list columns).

```{r}

got_chars %>%
  set_names(map_chr(.,'name'))  %>%
  map(`[`,c('name','allegiances')) %>%
  listviewer::jsonedit()

```


Basic loops



How to move around in lists effectively

- Manipulating independent lists

## Analysis with `purrr` and `modelr`

So far, `purrr` has basically helped us use tidy operations on lists. That's nice, but its real power comes in helping with analysis. Let's look at the gapminder data set

```{r}

DT::datatable(gapminder)

```


```{r, echo = F}

gapminder::gapminder %>%
  ggplot(aes(year,lifeExp, color = country)) +
  geom_line(show.legend = F) +
  facet_wrap(~continent) +
  labs(title = 'Life expectancy across continents')

```

Now, suppose we want to build up a model trying to predict life expectancy as a function of covariates, starting with a simple one: life expectancy as a function of population and per capita GDP

```{r}

gapminder <- gapminder %>%
  set_names( colnames(.) %>% tolower())

life_mod <- lm(lifeexp ~ pop + gdppercap, data = gapminder)

```


```{r, results = 'asis'}

stargazer::stargazer(life_mod, type = 'html')

```

Not bad for a simple model, but how do we know if this is the model we want to use? Let's use AIC to compare a few different model structures (note, this is not an endorsement for AIC mining!)

```{r}

m1 <- 'lifeexp ~ pop + gdppercap'

m2 <- 'lifeexp ~ pop + gdppercap + continent + year'

m3 <- 'lifeexp ~ pop + gdppercap + country + year'

m4 <- 'lifeexp ~ pop + gdppercap + year*country'

```

Now, since this is a simple three model example, we could just use a loop, or even copy and paste a few times. But, let's see how we can use `purrr` to help us do some diagnostics on these models.

Let's start by getting our models and data into a data frame, using list-columns

```{r}

model_frame <- data_frame(model = c(m1, m2, m3,m4)) %>%
mutate(model = map(model, as.formula))

```

First, let's use purrr to convert each of these character strings into a model

```{r}

model_frame <- data_frame(model_name = c('simple','medium','more', 'woah'),model = list("simple" = m1, 'medium' = m2, 'more' = m3, 'woah' = m4)) %>%
  mutate(model = map(model, as.formula))

model_frame
```

```{r, eval = F}

model_frame <- model_frame %>%
  mutate(fit = lm(model, data = gapminder))

```

Hmmm, why didn't that work? `mutate` by itself doesn't know how to evaluate this, but `map` can help us out

```{r}
model_frame <- model_frame %>%
  mutate(fit = map(model, ~lm(., data = gapminder)))

model_frame
```

We're now going to start integrating some methods from the `modelr` package to diagnose our regression

```{r}
model_frame <- model_frame %>%
mutate(r2 = map_dbl(fit, ~rsquare(., data = gapminder)),
aic = map_dbl(fit, ~AIC(.)))

model_frame

```

So, AIC tells us that our über complicated model is still the most parsimonious. Let's dig into this a bit further, by explicitly testing the out of sample predictive ability of each of the models. "Overfit" models are commonly really good at describing the data that they are fit to, but perform poorly out of sample.


We'll start by using the `modelr` package to create a bunch of training-test combination data sets

```{r}


validate <- gapminder %>%
crossv_mc(20, test = 0.25)

test_data <- list(test_training = list(validate), model = model_frame$model)


test_data <- cross_d(test_data) %>%
unnest(test_training, .drop = F, .id = 'model_number') %>%
mutate(model_number = as.numeric(model_number)) %>%
left_join(data_frame(model_number = c(1:4), model_name = c('simple','medium', 'more','woah')), by = 'model_number')

test_data
```

In a few lines of code, we now have "tidy" cross validation routine across multiple models, not bad.

```{r}

test_data <- test_data %>%
mutate(fit = map2(model, train, ~lm(.x, data = .y))) %>%
mutate(root_mean_sq_error = map2_dbl(fit, test, rmse))

test_data
```

```{r}

test_data %>%
ggplot(aes(root_mean_sq_error, fill = model_name)) +
geom_density(alpha = 0.75) +
labs(x = 'Root Mean Squared Error', title = 'Cross-validated distribution of RMSE')


```

## Miscellaneos `purrr`

That's a broad tour of the key features of `purrr`. Here's a few more examples of miscellaneous things you can do with `purrr`

### Check on factors

Factors can creep into your data, especially if you're reading in .csv's using base R. There's lot's of ways to solve this, but you can use `purrr` to efficiecntly check for factors, and convert them to characters in your dataframe.

```{r}

gapminder

```

Yep, look at that, country and continent are both factors. Useful for regression, but a little dangerous to have in your raw data.

We can use `purrr` to find all the factors in our data

```{r}

gapminder %>%
map(is.factor)

```
And to convert each column that is a factor into a character

```{r}

factor_foo <- function(x){

if (class(x) == 'factor'){

y <- as.character(x)

} else {
y <-  x

}

}

gapminder %>%
map_df(factor_foo)

```

## Center and Scale

One problem with our earlier regression: the scales of the different variables are widlly our of proportion

```{r}

summary(gapminder %>% select(year,lifeexp, pop, gdppercap))

```

This can can make your regression a little hard to interpret. Hhow do you think about the intercept in this model for example? Or, how do you compare the relative magnitude of coefficients? An increase of 1 year life expectancy is a much bigger shift and an increase in population of 1.

You can "center and scale" you data to resolve this problem, generally by

$$ x^{centered, scaled} = \frac{(x - mean(x))} {2\sigma_{x}}  $$
See Gelman & Hill 2007 for a good explanation of this.

Centering and scaling doesn't change the statistics of the model (significance etc.), just helps with interpretation in OLS. But, if you're doing maximum likelihood or Bayesian statistics, centering and scaling can dramatically improve the ability of the model to converge.

Suppose then that we want to center and scale all of the continuous variables in our data

```{r}

center_scale <- function(x, xname, omit_names = '') {

if (is.numeric(x) & !all(unique(x) %in% c(1, 0)) &
!xname %in% omit_names) {
x <- (x - mean(x)) / (2* sd(x))

}

return(x)

}

gapminder %>%
map2_df(
colnames(.),
center_scale
)

```

Looks good, except for the year thing. We're treating year as a continuos variable in our model. Suppose though we want to treat it as a factor, so that there doesn't have to be one slope for year. Remember that we can pass additional parameters through `map`

```{r}

gapminder %>%
map2_df(
colnames(.),
center_scale,
omit_names = 'year'
)

```

## Save all plots

Suppose you've got a large project and want to save (or print) all the plots. This often leads to a lot of copy and pasting of save commands, etc.

Here's another solution, using `walk`

I usually tag all my ggplot objects that I want to save with `_plot`

```{r}

life_v_money_plot <- gapminder %>%
ggplot(aes(gdppercap, lifeexp)) +
geom_abline(aes(slope = 1, intercept = 0))  +
geom_point() +
geom_smooth(method = 'lm')

life_v_money_plot

```

```{r}
life_v_time_plot <- gapminder %>%
ggplot(aes(year, lifeexp)) +
geom_point() +
geom_smooth(method = 'lm')

```

Suppose I want to save both of these plots?

```{r}

plot_files <- (ls()[ str_detect(ls(), '_plot')])

plot_foo <- function(x){

ggsave(paste0(x,'.pdf'), get(x), device = cairo_pdf)

}

walk(plot_files, plot_foo)

```


## Partial

I just really like this one. Suppose you've got something that you are copy and pasting a lot, like getting the upper and lower CI of something

```{r}

gapminder %>%
  summarise(
    mean_gdp = mean(gdppercap),
    lower_gdp = quantile(gdppercap, 0.25),
    upper_gdp = quantile(gdppercap, 0.75),
    mean_life = mean(lifeexp),
    lower_life = quantile(lifeexp, 0.25),
    upper_life = quantile(lifeexp, 0.75)
  )

```

Works, and in this case not hard, but still annoying to retype!

```{r}

lower = partial(quantile, probs = 0.25)

upper = partial(quantile, probs = 0.75)

gapminder %>%
  summarise(
    mean_gdp = mean(gdppercap),
    lower_gdp = lower(gdppercap),
    upper_gdp = upper(gdppercap),
    mean_life = mean(lifeexp),
    lower_life = lower(lifeexp),
    upper_life = upper(lifeexp)
  )

```
