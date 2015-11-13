# Erl

[ ![Codeship Status for sporto/erl](https://codeship.com/projects/74c708d0-6c07-0133-ba44-0e105eb8924a/status?branch=master)](https://codeship.com/projects/115393)

A URL parsing library for ELM

[Documentation at package.elm-lang.org](http://package.elm-lang.org/packages/sporto/erl/latest/Erl)

## Parse

Given a url like:

```elm
url = http://sam:pass@api.example.com:3000/products/kids#toys/1?color=red&age=10

Erl.parse url 
```

Returns:
```elm
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


## Test

```
npm i
npm test
```

## Todo

- Username
- Password
- Create empty Url
- Convenience methods for updating parts
- Methods for adding to query / replacing / removing / cleaning
