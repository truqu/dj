-module(dj_test).

-include_lib("eunit/include/eunit.hrl").

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

invalid_json_test() ->
  {error, [{invalid_json, <<>>}]} = dj:decode(<<>>, dj:null()),
  ok.

decode_object_test() ->
  Dec = dj:to_map(#{foo => dj:field(foo, dj:binary())}),
  %% All ok
  Json1 = <<"{\"foo\": \"bar\"}">>,
  M = #{foo => <<"bar">>},
  {ok, M} = dj:decode(Json1, Dec),
  %% Missing field
  Json2 = <<"{}">>,
  ErrMissing = {missing_field, foo, #{}},
  {error, [ErrMissing]} = dj:decode(Json2, Dec),
  %% Expected map
  Json3 = <<"123">>,
  WantMap = {unexpected_type, map, 123},
  {error, [WantMap]} = dj:decode(Json3, Dec),
  %% Expected binary
  Json4 = <<"{\"foo\": []}">>,
  WantBinary = {unexpected_type, binary, []},
  InField = {in_field, foo, [WantBinary]},
  {error, [InField]} = dj:decode(Json4, Dec),
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
  {error, [WantList]} = dj:decode(Json2, Dec),
  %% Not a binary, idx 1
  Json3 = <<"[\"foo\", 123, \"bar\", \"baz\"]">>,
  WantBinary = {unexpected_type, binary, 123},
  AtIndex = {at_index, 1, [WantBinary]},
  {error, [AtIndex]} = dj:decode(Json3, Dec),
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

integer_test() ->
  {ok, 123} = dj:decode(<<"123">>, dj:integer()),
  {error, [{unexpected_type, integer, 0.1}]} =
    dj:decode(<<"0.1">>, dj:integer()),
  ok.

pos_integer_test() ->
  {ok, 123} = dj:decode(<<"123">>, dj:pos_integer()),
  {error, [{unexpected_type, pos_integer, 0}]} =
    dj:decode(<<"0">>, dj:pos_integer()),
  {error, [{unexpected_type, pos_integer, -1}]} =
    dj:decode(<<"-1">>, dj:pos_integer()),
  ok.

neg_integer_test() ->
  {ok, -12} = dj:decode(<<"-12">>, dj:neg_integer()),
  {error, [{unexpected_type, neg_integer, 0}]} =
    dj:decode(<<"0">>, dj:neg_integer()),
  {error, [{unexpected_type, neg_integer, 1}]} =
    dj:decode(<<"1">>, dj:neg_integer()),
  ok.

non_neg_integer_test() ->
  {ok, 123} = dj:decode(<<"123">>, dj:non_neg_integer()),
  {ok, 0} = dj:decode(<<"0">>, dj:non_neg_integer()),
  {error, [{unexpected_type, non_neg_integer, -1}]} =
    dj:decode(<<"-1">>, dj:non_neg_integer()),
  ok.

float_test() ->
  {ok, 0.1} = dj:decode(<<"0.1">>, dj:float()),
  {ok, 123.0} = dj:decode(<<"123">>, dj:float()),
  {error, [{unexpected_type, float, <<>>}]} = dj:decode(<<"\"\"">>, dj:float()),
  ok.

null_test() ->
  {ok, foo} = dj:decode(<<"null">>, dj:null(foo)),
  {error, [{unexpected_type, null, true}]} = dj:decode(<<"true">>, dj:null()),
  ok.

boolean_test() ->
  {ok, true} = dj:decode(<<"true">>, dj:boolean()),
  {ok, false} = dj:decode(<<"false">>, dj:boolean()),
  {error, [{unexpected_type, boolean, null}]} =
    dj:decode(<<"null">>, dj:boolean()),
  ok.

atom_test() ->
  %% atom/0
  {ok, foo} = dj:decode(<<"\"foo\"">>, dj:atom()),
  {ok, null} = dj:decode(<<"null">>, dj:atom()),
  {ok, true} = dj:decode(<<"true">>, dj:atom()),
  {ok, false} = dj:decode(<<"false">>, dj:atom()),
  {error, [{custom, {not_an_atom, 123}}]} = dj:decode(<<"123">>, dj:atom()),
  %% atom/1
  {ok, foo} = dj:decode(<<"\"foo\"">>, dj:atom([foo, bar])),
  {error, [ {not_exactly, foo, true}
          , {not_exactly, bar, true}
          ]
  } = dj:decode(<<"true">>, dj:atom([foo, bar])),
  ok.

existing_atom_test() ->
  %% existing_atom/0
  {ok, foo} = dj:decode(<<"\"foo\"">>, dj:existing_atom()),
  {error, [{custom, {not_an_atom, <<"not an existing atom">>}}]} =
    dj:decode(<<"\"not an existing atom\"">>, dj:existing_atom()),
  {error, [{custom, {not_an_atom, []}}]} =
    dj:decode(<<"[]">>, dj:existing_atom()),
  %% existing_atom/1
  {ok, foo} = dj:decode(<<"\"foo\"">>, dj:existing_atom([foo])),
  ok.

full_date_tuple_test() ->
  Dec = dj:full_date_tuple(rfc3339),
  {ok, {2018, 6, 27}} = dj:decode(<<"\"2018-06-27\"">>, Dec),
  {error, [{custom, {malformed_date, <<"foo">>}}]} =
    dj:decode(<<"\"foo\"">>, Dec),
  {error, [{custom, {invalid_date, {2018, 2, 31}}}]} =
    dj:decode(<<"\"2018-02-31\"">>, Dec),
  ok.

prop_test() ->
  {ok, {foo, <<"hello">>}} =
    dj:decode(<<"{\"foo\": \"hello\"}">>, dj:prop(foo, dj:binary())),
  ok.

prop_list_test() ->
  Dec = dj:prop_list([ {foo, dj:binary()}
                     , {bar, dj:integer()}
                     ]),
  Json = << "{ \"foo\": \"hello\""
          , ", \"bar\": 123"
          , "}"
         >>,
  Exp = [{foo, <<"hello">>}, {bar, 123}],
  {ok, Exp} = dj:decode(Json, Dec),
  ok.

index_test() ->
  {ok, 123} = dj:decode(<<"[true, 123, false]">>, dj:index(1, dj:integer())),
  {error, [{missing_index, 1, []}]} =
    dj:decode(<<"[]">>, dj:index(1, dj:null())),
  {error, [{at_index, 0, [{unexpected_type, null, true}]}]} =
    dj:decode(<<"[true]">>, dj:index(0, dj:null())),
  {error, [{unexpected_type, list, #{}}]} =
    dj:decode(<<"{}">>, dj:index(0, dj:null())),
  ok.

list_test() ->
  Dec = dj:list(dj:integer()),
  {ok, [1, 2, 3]} = dj:decode(<<"[1, 2, 3]">>, Dec),
  {error, [{at_index, 2, [{unexpected_type, integer, true}]}]} =
    dj:decode(<<"[1, 2, true]">>, Dec),
  {error, [ {at_index, 0, [{unexpected_type, integer, true}]}
          , {at_index, 1, [{unexpected_type, integer, false}]}
          , {at_index, 3, [{unexpected_type, integer, null}]}
          ]
  } = dj:decode(<<"[true, false, 12, null]">>, Dec),
  ok.

multiple_errors_test() ->
  Dec = dj:to_map(#{ foo => dj:field(foo, dj:pos_integer())
                   , bar => dj:field(bar, dj:neg_integer())
                   , baz => dj:succeed(<<"hi">>)
                   , test => dj:at([a, b], dj:binary())
                   }),
  JsonOk = << "{ \"foo\": 123"
            , ", \"bar\": -20"
            , ", \"a\": {\"b\": \"hello\"}"
            , "}"
           >>,
  {ok, #{ foo := 123
        , bar := -20
        , baz := <<"hi">>
        , test := <<"hello">>
        }
  } = dj:decode(JsonOk, Dec),
  JsonBad = << "{ \"foo\": -20"
             , ", \"a\": {\"b\": null}"
             , "}"
            >>,
  Errors = [ {missing_field, bar, #{foo => -20, a => #{b => null}}}
           , {in_field, foo, [{unexpected_type, pos_integer, -20}]}
           , {in_field, a, [{in_field, b, [{unexpected_type, binary, null}]}]}
           ],
  {error, Errors} = dj:decode(JsonBad, Dec),
  ok.

uuid_test() ->
  ValidUUID = <<"2e2d65cb-437e-4abf-a7e8-7d97ad9d37a8">>,
  InvalidUUID = <<"42c1f4f4-a81c-11e7-abc4-cec278b6b50a">>,
  {ok, ValidUUID} = dj:decode(jsx:encode(ValidUUID), dj:uuid(v4)),
  {error, [{custom, {invalid_uuid, v4, InvalidUUID}}]} =
    dj:decode(jsx:encode(InvalidUUID), dj:uuid(v4)),
  ok.

set_test() ->
  Exp = sets:from_list([1, 2, 7, 99]),
  {ok, Exp} = dj:decode(<<"[1, 2, 7, 99]">>, dj:set(dj:integer())),
  ok.

nonempty_list_test() ->
  {ok, [foo]} = dj:decode(<<"[null]">>, dj:nonempty_list(dj:null(foo))),
  {error, [{unexpected_type, nonempty_list, []}]} =
    dj:decode(<<"[]">>, dj:nonempty_list(dj:null(foo))),
  ok.

throwing_decoders_are_not_caught_test() ->
  Dec = dj:map(fun (_) -> throw(foobar) end, dj:null()),
  ?assertThrow(foobar, dj:decode(<<"null">>, Dec)).

mapn_arity_mismatch_test() ->
  Dec = dj:mapn( fun (X, Y) -> {X, Y, 0} end
               , [ dj:field(major, dj:pos_integer())
                 , dj:field(minor, dj:non_neg_integer())
                 , dj:field(patch, dj:non_neg_integer())
                 ]
               ),
  Json = << "{ \"major\": 123"
          , ", \"minor\": 66"
          , ", \"patch\": 0"
          , "}">>,
  {error, [{custom, {arity_mismatch, 3, 2}}]} = dj:decode(Json, Dec),
  ok.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
