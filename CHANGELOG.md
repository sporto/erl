# Changelog

## 13.0.1

- Do not include port 443 (default) when protocol is https

## 13.0.1

- Fix parsing bugs see https://github.com/sporto/erl/pull/20

## 13.0.0

- Added `toAbsoluteString`
- `queryToString` takes an url again as in v11
- Added new module Erl.Query to handle the query

## 12.0.0

- `queryToString` takes url.query instead of url

### 11.1.1

- Extract Host without a TLD

### 11.1.0

- Added `getQueryValuesForKey`

## 11.0.0

- Change the query to `List (String, String)`. This allows the query string to hold duplicate values, which is valid.

### 10.0.2

- Upgrade to Elm 0.18

### 10.0.1

- `extractPort` attempts to add a default port if missing in the url

## 10.0.0

- Added `hasLeadingSlash`

## 9.0.0

- Added `hasTrailingSlash`

## 8.0.0

- `hash` is a string, not a list anymore

### 7.3.0

- Added `hashToString`

### 7.2.0

- Added `queryToString`

### 7.1.0

- Added `appendPathSegments`

## 7.0.0

- Hash goes after query as per https://url.spec.whatwg.org/

## 6.0.0

- `setQuery` replaces the whole query, Added `addQuery` and `removeQuery`

### 5.0.1

- `setQuery` removes the key when passed an empty value

## 5.0.0

- Renamed `fragment` to `hash` to aling with the MDN documentation better