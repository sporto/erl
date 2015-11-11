# Erl

A URL parsing library for ELM

## Parse

Given a url like:

```
http://example.com:3000/products/kids#toys/1?color=red&age=10
```

This will be parsed into an Elm record:

```
{
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
