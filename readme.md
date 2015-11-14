# Erl

A URL parsing and construction library for ELM

## Parse

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

## toString

```elm
-- Given a Erl.Url record (url)

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
- Create empty Url
- Encode / Decode query
- Convenience methods for updating parts
- Methods for adding to query / replacing / removing / cleaning
