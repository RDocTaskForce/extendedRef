
<!-- README.md is generated from README.Rmd. Please edit that file -->
extendedRef - Extended Reference Classes
========================================

[![Build Status](https://travis-ci.org/RDocTaskForce/extendedRef.svg?branch=master)](https://travis-ci.org/RDocTaskForce/extendedRef) [![codecov.io](https://codecov.io/github/RDocTaskForce/extendedRef/coverage.svg?branch=master)](https://codecov.io/github/RDocTaskForce/extendedRef?branch=master)

The goal of staticRef is to add static and private variable and methods support for R reference classes.

Installation
------------

You can install the released version of extendedRef from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("extendedRef")
```

Example
-------

To setup an extended reference class use the `setExtendedRefClass()` function. For this example we will create a registered counter class that tracks all the counters created and the individual counts.

``` r
counter <- 
    setExtendedRefClass('registeredCounter',
        # fields define the publicly accessible fields, same as setRefClass
        fields = c(name= 'character'),
        
        # private are the variables that are not accessible via the `$` operator
        private = c(count = 'integer'),
        
        # staic  specifies static variables common to all variables.
        static = c( counters = 'list' ),
        
        # static.const specifies constants for the class that cannot be changed.
        # since these do not change they are specified once by a list with the 
        # values they should take.
        static.const  = list( max = 3L),
        # for illustration, we are artificially setting a maximum of 3 counters 
        
        # Methods are specified with lists of functions.
        # static methods
        static.methods = list( how_many_counters = function()length(counters)
                             , add_counter = function(counter){
                                 x <- list(counter)
                                 names(x) <- counter$name
                                 counters <<- c(counters, x)
                             }),

        # private methods
        private.methods = list(private_initialize = function(){
            # The public initialize, cannot use or alter private variables,
            # the `private_initialize` function serves this role. 
            count <<- 0L

            # The private initialize can also access the static variables,
            if (how_many_counters() < max) add_counter(.self) else
                stop("Cannot create another counter!")
        }),

        # methods as with setRefClass defines the public methods.
        methods = list( increment = function()count <<- count + 1L
                      , get_count = function()count
                      ),
        # note that the public function alters a 'private' variable.
        )
```

This class may not be very useful but illustrates the concepts.

As before the `counter` object is a object generator function that creates objects from the exteneded reference class 'registeredCounter'. However, the generator object also provides a method for accessing the static variables and methods.

``` r
counter$static$how_many_counters()
#> [1] 0
counter$static$max
#> [1] 3
```

``` r
first <- counter(name = 'first')
```

We now have `first` as a counter and we have one counter registered.

``` r
first$get_count()
#> [1] 0
counter$static$how_many_counters()
#> [1] 1
```

Counter is a reference class so you can do all the things that you would do with a reference class

``` r
first$increment()
first$get_count()
#> [1] 1
```

``` r
also.first <- first
also.first$get_count()
#> [1] 1
```

Note that one thing you cannot do is access the private variable `count`.

``` r
first$count
#> Error in envRefInferField(x, what, getClass(class(x)), selfEnv): 'count' is not a valid field or method name for reference class "registeredCounter"
```

For the static variables all object have access to them from their internal methods, but not publicly.

``` r
first$how_many_counters()
#> Error in envRefInferField(x, what, getClass(class(x)), selfEnv): 'how_many_counters' is not a valid field or method name for reference class "registeredCounter"
```

Let's create a few more counters.

``` r
second <- counter(name = 'second')
third <- counter(name = 'third')
fourth <- counter(name = 'fourth')
#> Error in private.methods$private_initialize(): Cannot create another counter!
```

We cannot add the fourth counter because we specified that we can only have 3 total.

``` r
counter$static$how_many_counters()
#> [1] 3
```

Since the counters are registered we can retrieve them from the registry.

``` r
counter$static$counters[['first']]$get_count()
#> [1] 1
```

The static variables are tied to the stored definition so there is always only one copy.

``` r
validate_that(identical(counter$def, getClass('registeredCounter')))
#> [1] TRUE
```
