-module(dj_test).

-include_lib("eunit/include/eunit.hrl").

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

decode_object_test() ->
  Dec = dj:to_map(#{foo => dj:field(foo, dj:binary())}),
  %% All ok
  Json1 = <<"{\"foo\": \"bar\"}">>,
  M = #{foo => <<"bar">>},
  {ok, M} = dj:decode(Json1, Dec),
  %% Missing field
  Json2 = <<"{}">>,
  ErrMissing = {missing_field, foo, #{}},
  {error, ErrMissing} = dj:decode(Json2, Dec),
  %% Expected map
  Json3 = <<"123">>,
  WantMap = {unexpected_type, map, 123},
  {error, WantMap} = dj:decode(Json3, Dec),
  %% Expected binary
  Json4 = <<"{\"foo\": []}">>,
  WantBinary = {unexpected_type, binary, []},
  InField = {in_field, foo, WantBinary},
  {error, InField} = dj:decode(Json4, Dec),
  %% Done
  ok.

decode_list_test() ->
  Dec = dj:list(dj:binary()),
  %% All ok
  Json1 = <<"[\"foo\", \"bar\", \"baz\"]">>,
  L = [<<"foo">>, <<"bar">>, <<"baz">>],
  {ok, L} = dj:decode(Json1, Dec),
  %% Not a list
  Json2 = <<"{}">>,
  WantList = {unexpected_type, list, #{}},
  {error, WantList} = dj:decode(Json2, Dec),
  %% Not a binary, idx 1
  Json3 = <<"[\"foo\", 123, \"bar\", \"baz\"]">>,
  WantBinary = {unexpected_type, binary, 123},
  AtIndex = {at_index, 1, WantBinary},
  {error, AtIndex} = dj:decode(Json3, Dec),
  %% Done
  ok.

decode_version_test() ->
  Version = dj:mapn( fun (X, Y, Z) -> {X, Y, Z} end
                    , [ dj:index(0, dj:integer())
                      , dj:index(1, dj:integer())
                      , dj:index(2, dj:integer())
                      ]
                    ),
  Dec = dj:field(version, Version),
  %% All ok
  Json1 = <<"{\"version\": [1, 2, 3]}">>,
  {ok, {1, 2, 3}} = dj:decode(Json1, Dec),
  %% Done
  ok.

decode_one_of_test() ->
  Accept = dj:one_of([ dj:integer()
                      , dj:exactly(foobar, dj:atom())
                      ]),
  Dec = dj:list(Accept),
  %% All ok
  Json1 = <<"[123, \"foobar\"]">>,
  Exp1 = [123, foobar],
  {ok, Exp1} = dj:decode(Json1, Dec),
  %% Done
  ok.

decode_at_test() ->
  Dec = dj:at([foo, bar], dj:binary()),
  Json = <<"{\"foo\": {\"bar\": \"baz\"}}">>,
  {ok, <<"baz">>} = dj:decode(Json, Dec),
  ok.

decode_one_of_as_default_test() ->
  Dec = dj:list(dj:one_of([ dj:existing_atom()
                            , dj:succeed(foobar)
                            ])),
  Json = <<"[\"abcde\", \"foo\", \"fghi\", \"bar\"]">>,
  Exp = [foobar, foo, foobar, bar],
  {ok, Exp} = dj:decode(Json, Dec),
  ok.

decode_email_test() ->
  {ok, _} = dj:decode(<<"\"ilias@truqu.com\"">>, dj:email()),
  {error, _} = dj:decode(<<"\"foo@bar\"">>, dj:email()),
  ok.


decode_full_object_test() ->
  Json = << "{\"foo\": 42, \"date\": \"2001-01-01\", "
          , "\"baz\": \"quux\", \"scores\": [1,2,3]"
          , "}"
         >>,
  Dec = dj:to_map(#{ foo => dj:field(foo, dj:integer())
                    , bar => dj:one_of([ dj:field(bar, dj:integer())
                                        , dj:succeed(-23)
                                        ])
                    , date => dj:field(date, dj:full_date_tuple(rfc3339))
                    , baz => dj:field(baz, dj:existing_atom())
                    , scores => dj:field(scores, dj:list(dj:integer()))
                    , hard_set => dj:succeed(<<"mine">>)
                    }),
  M = #{ foo => 42
       , bar => -23
       , date => {2001, 1, 1}
       , baz => quux
       , scores => [1, 2, 3]
       , hard_set => <<"mine">>
       },
  {ok, M} = dj:decode(Json, Dec),
  ok.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
