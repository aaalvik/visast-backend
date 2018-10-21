To build, do: 

``` 
stack build
```

To run the app, do:

``` bash
stack exec example-servant-minimal
```

Then you can query the server like this:

``` bash
curl localhost:3000/item

curl -X POST -d '{"name":"Add", "children":[{"name":"Leaf", "children":[]}]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:3000/step
```
