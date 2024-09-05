# Servant client pagination experiment

[SO answer](https://stackoverflow.com/a/78954717/1364288).

```
servant-pagination-experiment$ cabal run
Resolving dependencies...
Response: [1,2,3,4,5,6,7,8,9]
```

Each group of three numbers in the response comes from a different HTTP request.