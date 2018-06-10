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
, query = [ ( "color", "red" ), ( "age", "10") ]
, hash = "#toys/1"
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

There are many ways to parse query strings. E.g. an array might be `a[]=1&a[]=2` or `a=1&a=2` depending on the web framework or library.

Erl parses the query into a `List (String, String)`. This is a bit more useful than just a string, but not as opinionated as other libraries.

## Documentation

[Documentation at package.elm-lang.org](http://package.elm-lang.org/packages/sporto/erl/latest/Erl)

## Test

```
yarn install
npm test
```

## [Changelog](https://github.com/sporto/erl/blob/master/changelog.md)
