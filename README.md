# dj

An OTP library for decoding JSON, that doesn't stop at decoding JSON.

The main idea behind `dj` is to combine the decoding, validation, and conversion of JSON data. Let's look at an example. Given the following JSON:

```javascript
{
  "foo": 42,
  "date": "2001-01-01",
  "baz": "quux",
  "scores": [1, 2, 3]
}
```

This code:

```erlang
dj:decode(Json, [ dj:object([{labels, atom}])
                , dj_maps:is_key(foo)
                , dj_maps:value_isa(foo, dj_int:is_pos())
                , dj_maps:put_default(bar, -23)
                , dj_maps:value_isa(bar, dj_int:is_neg())
                , dj_maps:is_key(date)
                , dj_maps:value_isa(date, dj_datetime:is_full_date(rfc3339))
                , dj_maps:update_with(date, dj_datetime:full_date_to_tuple(rfc3339))
                , dj_maps:is_key(baz)
                , dj_maps:value_isa(baz, dj:one_of([<<"quux">>, <<"quuux">>]))
                , dj_maps:update_with(baz, dj:to_atom())
                , dj_maps:is_key(scores)
                , dj_maps:value_isa(scores, dj:list_of(dj_int:is_pos()))
                ]
  )
```

Will return:

```erlang
{ok, #{ foo => 42
      , bar => -23
      , date => {2001, 1, 1}
      , baz => quux
      , scores => [1, 2, 3]
      }
}
```
