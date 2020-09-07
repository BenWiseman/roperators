---
title: "Make your R code nicer with roperators"
output: 
  prettydoc::html_pretty:
    toc: true
    readme: true
    theme: "cayman"
vignette: >
  %\VignetteIndexEntry{Make your R code nicer with roperators}
  %\VignetteEngine{knitr::knitr}
  \usepackage[utf8]{inputenc}
---





## A Package to Make R a Little Nicer

Vignette will usually be updated here first:
</br>
https://www.happylittlescripts.com/2018/09/make-your-r-code-nicer-with-roperators.html
</br>

When I first started with R, there were a few things that bothered me greatly. 
While I can't change dynamic typing, it is possible to do things such as:

1) String addition, subtraction, multiplication, and division
</br>
2) In-place modifiers (*à la* `+=`)
</br>
3) Direct assignments to only NA or regex-matched elements
</br>
4) Comparison operators for between, floating point equality, and more
</br>
5) Extra logical operators to make code more consistent
</br>
6) Make nicer (shorter) conversion functions (`int()` as opposed to `as.integer()`)
</br>
7) Simple checks for usability (e.g `is.bad_for_calcs()`)

The above functionality, I'd found myself manually adding into my R projects to 
clean up the code. Then me an my colleges thought: 'that all might actually be useful
as a package.' So now it's a package on CRAN: `roperators` (pronounced 'rop-er-ators, not r-operators)

To help introduce you to `roperators`, I put together some use cases where it'll make your life easier. 

## String Arithmetic


One of the most common criticisms lobbed at R by Python people (and their wretched, 
non-curly-brace-using ilk) is the lack of string arithmetic. In a world without `roperators` 
one simply had to deny reality and insist that using a paste function doesn't look any worse 
than simply using + to concatenate words.

Happily, using `roperators`, you can now do this:


```r
require(roperators)
my_string <- 'using infix (%) operators ' %+% 'R can do simple string addition'
print(my_string)
```

```
## [1] "using infix (%) operators R can do simple string addition"
```

You can also use `%-%` to delete bits of text like so:


```r
my_string %-% 'R can do simple string addition'
```

```
## [1] "using infix (%) operators "
```

If ever need to use string multiplication (like some kind of barbarian), you can use `%s*%` (`%*%` was taken already)


```r
my_a <- 'a'
my_a %s*% 3
```

```
##     a 
## "aaa"
```

```r
# If a is an unnamed vector, the original value is saved as the element name(s)
# just to make it easier to undo by my_a <- names(my_a)
```

And, something you can't do in Python: **string division**


```r
# How many times does the letter a appear in the string
'an apple a day keeps the malignant spirit of Steve Jobs at bay' %s/% 'a' 
```

```
## a 
## 8
```

String division also works with regular expressions (it is case sensitive):


```r
# How many times is Steve Jobs or apple mentioned?
'an apple a day keeps the malignant spirit of Steve Jobs at bay' %s/% 'Steve Jobs|apple' 
```

```
## Steve Jobs|apple 
##                2
```



## In-Place Modifiers (*à la* `+=`)


The lack of operators like += that you'd find in other languages is another
common criticism of R. Happily, you have `roperators`. 

Now, at the risk of sounding like one of those infomercials where people struggle with 
clearly trivial tasks, how many times do you end up doing something like this:

```r
iris_data$Sepal.Length <- iris_data$Sepal.Length + 1
```
Or worse...

```r
iris_data$Sepal.Length[iris_data$Species == 'setosa'] <- iris_data$Sepal.Length[iris_data$Species == 'setosa'] + 1
# ...which may not even fit on the page. 
```

Without `roperators` the trivial code above makes me envy the blind. After all, 
you're only adding 1 to some values. So, using the *greatest-best* package formally called
`roperators`: 


```r
iris_data$Sepal.Length %+=% 1
```
Or

```r
iris_data$Sepal.Length[iris_data$Species == 'setosa'] %+=% 1
# ...which is ike a breath of fresh air
```

The current in-place modifiers included in `roperators` are:


* `%+=%`, `%-=%`             - Add to and subtract from a variable. Also works on character strings
* `%*=%`, `%/=%`, and `%^=%` - Multiply, divide, and exponentiation a variable.
* `%root=%` and `%log=%`     - Transform a variable by the nth root or log
* `%regex=%`                 - Apply a regular expression to text

The last two are similar depending on whether you want to modify the text or replace it outright. 
Note that they both take two values `c(pattern, replacement)`:

```r
x <- c("a1b", "b1", "c", "d0")
# Replace digits with the letter x
x %regex=% c("\\d+", "i")
 # x is now c("aib", "bi", "c", "di")
print(x)
```

```
## [1] "aib" "bi"  "c"   "di"
```



## Replace Missing Values or Regex matches Directly


The last in-place modifiers are `%na<-%` which works as you'd expect and `%regex<-%` 
which is hopefully intuitive enough. This is useful for all those 
times you'd otherwise need to do something clunky like `df$column[is.na(df$column)] <- 0`


```r
x <- c(NA, 1, 2, 3)
x %na<-% 0
print(x)
```

```
## [1] 0 1 2 3
```
And to replace by regex... (as opposed to modifying with `%regex=%`) 

```r
x <- c("aib", "bi", "c", "di")
x %regex<-% c('i', '[redacted]')
print(x)
```

```
## [1] "[redacted]" "[redacted]" "c"          "[redacted]"
```

## Make More Comparisons and Logical Operators Great Again


This category of `roperators` is an answer to all those who cry out for help when
what should be simple logical statements are either inconsistent looking or, 
such as the case with floating point equality, god-awful looking.

### When `1 == NA` should be `FALSE` 


First up: `if(a == b)` when `a` and `b` are both `NA`. I get it, an `NA` doesn't
technically equal another `NA`, however most of the time they, for all intents and purposes, 
are the same. The solution is simple: 


```r
x <- c(NA, 'foo', 'foo', NA)
y <- c(NA, 'foo', 'bar', 'bar')

x %==% y
```

```
## [1]  TRUE  TRUE FALSE FALSE
```

As opposed to: 


```r
x == y
```

```
## [1]    NA  TRUE FALSE    NA
```

Think about how many `if` statements you've had break due to a lack of missing-value equality capability. 
You can also use `%<=%` and `%>=%` to handle missing values instead of `<=` and `>=`


### When `(0.1 + 0.1 + 0.1) == 0.3` should be `TRUE` (i.e. almost always)


The floating point trap is a particular kind of mongrel. Innocent young statistics students
are seldom warned about it, and so they go about, using `==` thinking that it'll keep working
even when a decimal place is present when in reality, is doesn't always. 

Don't believe me? Oh, my sweet summer child, try this and despair:


```r
(0.1 * 3) == 0.3  # FALSE
(0.1 * 5) == 0.5  # TRUE
(0.1 * 7) == 0.7  # FALSE
(0.1 * 11) == 1.1 # TRUE

(0.1 * 3) >= 0.3 # TRUE
(0.1 * 3) <= 0.3 # FALSE
```
If you're feeling panicked about your old scripts, well, I guess you should be. 
</br>

Happily, you now have `roperators`


```r
(0.1 * 3)  %~=% 0.3  # TRUE
(0.1 * 5)  %~=% 0.5  # TRUE
(0.1 * 7)  %~=% 0.7  # TRUE
(0.1 * 11) %~=% 1.1  # TRUE

(0.1 * 3) %>~% 0.3 #TRUE
(0.1 * 3) %<~% 0.3 #TRUE
```

You could use something like `isTRUE(all.equal(0.1 * 3, 0.3))` but that looks disgusting. 

```r
isTRUE(all.equal(0.1 * 3, 0.3))  # TRUE
isTRUE(all.equal(0.1 * 5, 0.5))  # TRUE
isTRUE(all.equal(0.1 * 7, 0.7))  # TRUE
isTRUE(all.equal(0.1 * 11, 1.1))  # TRUE


isTRUE(all.equal(0.1 * 3, 0.3)) | ((0.1 * 3) > 0.3)
isTRUE(all.equal(0.1 * 3, 0.3)) | ((0.1 * 3) < 0.3)
# I feel dirty even typing that as an example. 
```

If you have any sense of style, just use `%~=%` instead. 

### When `x` is between `a` and `b`

This is a simple shortcut with two variants for end-exclusive and end-inclusive 
between. you just need to feed in `c(lower_bound, upper_bound)`


```r
5 %><% c(1, 10)  # TRUE
```

```
## [1] TRUE
```

```r
1 %><% c(1, 10)  # FALSE
```

```
## [1] FALSE
```

```r
1 %>=<% c(1, 10) # TRUE
```

```
## [1] TRUE
```

```r
# note that due to my simple mindedness, at the time of writing, 5 %><% c(10, 1) is FALSE
```

Note that `%>=<%` doesn't support NA equality testing. If you want a variant that does that in a future version, just let me know. 

### When you need something else

The last set of logical operators are not in, exclusive or, and all-or-nothing. 

**Not In** `%ni%` was made because it's just easier to read than negating an in statement.
For example:

```r
!1 %in% c(2,3,4)
```

```
## [1] TRUE
```
Which reads "not 1 in [2, 3, 4]?" which just looks wrong. So, we appropriated from the snake-like language:


```r
1 %ni% c(2,3,4)
```

```
## [1] TRUE
```
Which now reads: "1 not in [2, 3, 4]?" That's just better looking. 


**Exclusive Or** exists in base R as a function, which makes it look inconsistent, for example:

```r
if((a|b) & xor(y, z))
```
I know it's finicky, but the `roperators` way is a touch more consistent:

```r
if((a|b) & (y %xor% z))
```
That way both expressions are using an operator rather than one or statement using an operator while the other uses a function. 


**All or Nothing** is for those occasions when you want `a` and `b` to either both be `TRUE` or both be `FALSE` - for two logical variables it's probably easier to use `a == b`, but for expressions it can be cleaner:


```r
if((a*2 == b+2) %aon% (x^2 == y*10))
# Compared to 
if((a*2 == b+2) == (x^2 == y*10)) 
# which takes my brain a little bit more time to read
```

But, like I said, that's me personally being finicky. 


## Shorten type conversions

Fair warning: this part of `roperators` will, I'm sure, be the source of a lot of hate-mail.

### Numeric to factor

One of the ugliest things I see in R code is the infamous `x <- as.numeric(as.character(x))`
when trying to turn a factor with numeric labels (most of which are the fault of dynamic typing) 
into a number. I can still recall the rage I felt the first time a factor was converted into its levels
rather than its labels when using `as.numeric()`. 


The simple solution is just a shorthand: `x <- f.as.numeric(x)` - just chuck an f in front of it and be done with it. 

### Shorten `as.charater` and friends. 

I'll give this one to PyPeople, R's conversion syntax is cumbersome. That's why `roperators` includes:

* `chr()` short for `as.character()`
* `num()` short for `as.numeric()`
* `int()` short for `as.integer()`
* `dbl()` short for `as.double()`
* `chr()` short for `as.character()` (if only `str()` wasn't already taken)
* `bool()` short for `as.logical()` 

Now things like this:


```r
x <- c('TRUE', 'FALSE', 'TRUE', 'TRUE')

percent_true <- paste0(sum(as.integer(as.logical(x))) / length(x)*100, '%')
print(percent_true)
```

```
## [1] "75%"
```

Can be done like this:


```r
percent_true <- (sum(int(bool(x))) / length(x) * 100) %+% '%'
print(percent_true)
```

```
## [1] "75%"
```

Which is arguably easier on the eyes, especially for people who grew up in other programming languages. 


## Add more type checks

Sometimes you just want to know that everything is going to be okay. Rather than running
multiple checks. If you wanted to be sure something was going to work in R, you could do something like
this: 


```r
if(is.atomic(x) & (length(x) >= 1) & !is.na(x) & !is_nan(x) & !is.na(as.numeric(x)) & !is.factor(x) & !is.infinite(x) ){
  ...
}
```

...Which is fine if you're happy with people thinking you're a maniac, 
Or you could just use `roperators` like so:


```r
if(!is.bad_for_calcs(x)){
  ...
}
```

And as a convenience function there's also `any_bad_for_calcs()` to save you from 
`any(is.bad_for_calcs((x))` because we're nice like that. 

Beyond that, you'll also find:

* `is.scalar()`
* `is.irregular_list()`
* `is.bad_for_indexing()`

To help with basic checks, and for those times when something should either be a certain class or `NULL`:

* `is.scalar_or_null()`
* `is.numeric_or_null()`
* `is.character_or_null()`
* `is.logical_or_null()`
* `is.df_or_null()`
* `is.list_or_null()`
* `is.atomic_nan()` (I didn't want to put it all by itself)


## Bonus - Use `roperators` with `magrittr`

Some of you may have been thinking: “oh, so it’s like using `magrittr` to write cleaner code?” And, yes, it’s kind of the same idea - making R coding a bit nicer around the edges. Also like `magrittr`, it’s a tiny, self contained package so you can easily use it in production code. 
I use `roperators` and `magrittr` together religously and you should too. After all, why make things unpleasant for yourself when you don’t need to? Just think about how much more clean and tidy your R code could be if you used string arithmetic operators, in-place modifiers, direct replacement for missing values, better logical operators (especially for NA handling and floating point equality), terse conversions, simplified checks, and magrittr pipes!
