# Erl

A URL parsing library for ELM

## Parse

Given a url like:

```
http://sam:pass@api.example.com:3000/products/kids#toys/1?color=red&age=10
```

This will be parsed into an Elm record:

```
{
  protocol: "http",
  username: "sam",
  password: 'pass',
  domain: ["api", "example", "com"],
  port': 3000,
  path: ["products", "kids"],
  hash: ["toys", "1"],
  query: Dict {
    "color": "red",
    "age": "10"
  }
}
```

## Construct

## Test

```
elm reactor
```

Open `http://localhost:8000/ErlTest.elm`

## Todo

- Username
- Password
- Port
- Domain
- Path
- Construction
