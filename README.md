# Erl

[ ![Codeship Status for sporto/erl](https://codeship.com/projects/74c708d0-6c07-0133-ba44-0e105eb8924a/status?branch=master)](https://codeship.com/projects/115393)

A URL parsing and construction library for ELM

## `parse` a url string

```elm
-- Given a url string
url = "http://sam:pass@api.example.com:3000/products/kids?color=red&age=10#toys/1"

Erl.parse url 

-- Returns a Erl.Url record:

{ protocol = "http"
, host = "api.example.com"
, port_ = Just 3000
, pathname = "/products/kids"
, hash = "#toys/1"
, query = "?color=red&age=10"
}
```

See MDN for more details (https://developer.mozilla.org/en-US/docs/Web/API/Location). Note that in MDN `query` is called `search`.

## `toString`

```elm
-- Given a Erl.Url record (url):

Erl.toString url 

-- Returns the url as string:

"http://www.foo.com:2000/users/1?k=2&q=1#a/b"
```

## Query parsing

There isn't a unique standard for query parsing. Many frameworks / libraries do this in different ways. E.g. an array might be `a[]=1&a[]=2` or `a=1&a=2`.

This version of Erl doesn't have an opinionated way of parsing the query anymore. Instead you will get the raw string e.g. `?a=1&b=2`. There are other good Elm libraries to parse queries.

## Documentation

[Documentation at package.elm-lang.org](http://package.elm-lang.org/packages/sporto/erl/latest/Erl)

## Test

```
yarn install
npm test
```

## [Changelog](https://github.com/sporto/erl/blob/master/changelog.md)