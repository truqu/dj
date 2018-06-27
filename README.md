# dj

An OTP library for decoding JSON, that doesn't stop at decoding JSON.

The main idea behind `dj` is to combine the decoding, validation, and conversion
of JSON data. Let's look at an example. Given the following JSON:

```json
{
  "foo": 42,
  "date": "2001-01-01",
  "baz": "quux",
  "scores": [1, 2, 3]
}
```

This code:

```erlang
Decoder = dj:to_map(#{ foo => dj:field(foo, dj:pos_integer())
                     , bar => dj:one_of([ dj:field(bar, dj:neg_integer())
                                        , dj:succeed(-23)
                                        ])
                     , date => dj:field(date, dj:full_date_tuple(rfc3339))
                     , baz => dj:field(baz, dj:atom([quux, quuux]))
                     , scores => dj:field(scores, dj:list(dj:pos_integer()))
                     }),
dj:decode(Json, Decoder).
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
