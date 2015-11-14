# Erl

A URL parsing and construction library for ELM

## `parse` a url string

```elm
-- Given a url string
url = "http://sam:pass@api.example.com:3000/products/kids#toys/1?color=red&age=10"

Erl.parse url 

-- Returns a Erl.Url record:

{
  protocol: "http",
  username: "sam",
  password: "pass",
  host: ["api", "example", "com"],
  port': 3000,
  path: ["products", "kids"],
  fragment: ["toys", "1"],
  query: Dict {
    "color": "red",
    "age": "10"
  }
}
```

## Construct

### `new`

```elm
-- Create a Erl.Url record:

url = 
  Erl.new
```

### Mutations

```elm
-- Modify the protocol
{ url | protocol <- "https" }

-- Modify host
{url | host <- ["api", "acme", "com"] }

-- Modify port
{ url | port' <- 9000 }

-- Modify path
{url | path <- ["users", "1"] }

-- Modify fragment (hash)
{url | fragment <- ["!", "user", "1"] }

-- Clear query string
Erl.clearQuery url

-- Set query string key/value
Erl.setQuery "key" "value" url

-- Unset query string key/value
Erl.unsetQuery "key" url
```

### `toString`

```elm
-- Given a Erl.Url record (url):

Erl.toString url 

-- Returns the url as string:

"http://www.foo.com:2000/users/1#a/b?k=2&q=1"
```

## Documentation

[Documentation at package.elm-lang.org](http://package.elm-lang.org/packages/sporto/erl/latest/Erl)

## Test

```
npm i
npm test
```

## Todo

- Username
- Password
- Convenience methods for updating parts
- Methods for adding to query / replacing / removing / cleaning
