%% @doc {@module} allows writing composable decoders, combining decoding,
%% transformation and validation.
-module(dj).

-export([ %% Primitives
          binary/0
        , integer/0
        , pos_integer/0
        , neg_integer/0
        , non_neg_integer/0
        , float/0
        , null/0
        , null/1
        , boolean/0
        , value/0
          %% Convenience decoders
        , atom/0
        , atom/1
        , existing_atom/0
        , existing_atom/1
        , email/0
        , full_date_tuple/1
        , uuid/1
        , integer/2
        , nullable/1
        , nullable/2
          %% Objects and maps
        , field/2
        , optional_field/3
        , at/2
        , prop/2
        , prop_list/1
        , to_map/1
          %% Lists
        , list/1
        , nonempty_list/1
        , index/2
        , sequence/1
        , set/1
          %% Manipulating decoders
        , map/2
        , chain/2
        , fail/1
        , succeed/1
          %% Fancy decoders
        , mapn/2
        , exactly/2
        , one_of/1
          %% Running a decoder
        , decode/2
        , decode/3
        ]).

%%%-----------------------------------------------------------------------------
%%% Types
%%%-----------------------------------------------------------------------------

-type decoder(T) :: fun ((jsx:json_term()) -> result(T, errors())).
%% A `decoder(T)' is an opaque datastructure that represents a composable
%% decoder. After composing a decoder for your JSON and final datastructure, you
%% can run it with {@link decode/2} or {@link decode/3}.
%%
%% Consider `decoder(T)' an opaque datastructure. It is only exposed due to
%% dialyzer limitations.

-type result(V, E) :: {ok, V} | {error, E}.
%% Running a decoder results in a `result', indicating either success (and
%% providing the decoded data) or failure, with a nonempty list of errors.

-type error() :: {unexpected_type, ExpectedType :: type(), jsx:json_term()}
               | {missing_field, field(), jsx:json_term()}
               | {missing_index, non_neg_integer(), jsx:json_term()}
               | {in_field, field(), [error(), ...]}
               | {at_index, non_neg_integer(), [error(), ...]}
               | {custom, any()}
               | {invalid_json, any()}.
%% `error()' is a (resursive) structure that gives detailed information about
%% what went wrong, and where.
%%
%% Most primitive decoders may fail with an `unexpected_type' error when the
%% JSON value is not the expected type. Those errors contain both the expected
%% <em>type</em> and the actual <em>value</em>.
%%
%% When decoders like {@link field/2} or {@link index/2} are used,
%% `missing_field' and `missing_index' may be used when the provided field or
%% index was not found in the JSON data.
%%
%% Errors that occur in the context of a field or index, for example when using
%% {@link field/2} or {@link list/1}, are nested in `in_field' or `at_index'
%% which allows tracing the failure path.
%%
%% Convenience and extended decoders using {@link fail/1} will wrap their errors
%% in `custom' to indicate a higher level failure.
%%
%% If the actual JSON cannot be parsed by jsx, an `invalid_json' error is
%% produced.

-type errors() :: [error(), ...].
%% A nonempty list of {@link error/0}s

-type type() :: binary
              | integer
              | pos_integer
              | neg_integer
              | non_neg_integer
              | null
              | boolean
              | float
              | map
              | list
              | nonempty_list.
%% These may appear in `unexpected_type' errors.

-type field() :: atom() | binary().
%% Depending on the options passed to {@link decoder/3}, a field-name may be
%% either an `atom()', a `binary()' or a mix of both. When using {@link
%% decoder/2}, field-names are always atoms. Note that those atoms must already
%% exist, or decoding will fail.

-export_type([ decoder/1
             , result/2
             , error/0
             , errors/0
             , type/0
             , field/0
             ]).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------


%% @doc Decodes a JSON string as a `binary()'.
%%
%% ```
%%    {ok, <<"Hi there">>} = dj:decode(<<"\"Hi there\"">>, dj:binary()).
%% '''
%%
%% If the specified JSON is not a string (it might, for example, be an integer),
%% this will return an error indicating that an unexpected type was found -
%% including the actual value that was found instead. This value will be a
%% `jsx:json_term()'.
%%
%% ```
%%    {error, {dj_errors, [{unexpected_type, binary, 123}]}} =
%%        dj:decode(<<"123">>, dj:binary()).
%% '''
-spec binary() -> decoder(binary()).
binary() ->
  fun (Json) when is_binary(Json) -> {ok, Json};
      (Json)                      -> unexpected_type_error(binary, Json)
  end.

%% @doc Decodes a JSON integer as an `integer()'
%%
%% ```
%%    {ok, 123} = dj:decode(<<"123">>, dj:integer()).
%%    {error, {dj_errors, [{unexpected_type, integer, true}]}} =
%%        dj:decode(<<"true">>, dj:integer()).
%% '''
%%
%% @see float/0
%% @see pos_integer/0
%% @see neg_integer/0
%% @see non_neg_integer/0
%% @see integer/2
-spec integer() -> decoder(integer()).
integer() ->
  fun (Json) when is_integer(Json) -> {ok, Json};
      (Json)                       -> unexpected_type_error(integer, Json)
  end.

%% @doc Decodes a strictly positive JSON integer as a `pos_integer()'
%%
%% @see float/0
%% @see integer/0
%% @see neg_integer/0
%% @see non_neg_integer/0
-spec pos_integer() -> decoder(pos_integer()).
pos_integer() ->
  fun (Json) when is_integer(Json), Json > 0 ->
      {ok, Json};
      (Json) ->
      unexpected_type_error(pos_integer, Json)
  end.

%% @doc Decodes a negative JSON integer as a `neg_integer()'
%%
%% @see float/0
%% @see pos_integer/0
%% @see integer/0
%% @see non_neg_integer/0
-spec neg_integer() -> decoder(neg_integer()).
neg_integer() ->
  fun (Json) when is_integer(Json), Json < 0 ->
      {ok, Json};
      (Json) ->
      unexpected_type_error(neg_integer, Json)
  end.

%% @doc Decodes a positive JSON integer as a `non_neg_integer()'
%%
%% @see float/0
%% @see pos_integer/0
%% @see neg_integer/0
%% @see integer/0
-spec non_neg_integer() -> decoder(non_neg_integer()).
non_neg_integer() ->
  fun (Json) when is_integer(Json), Json >= 0 ->
      {ok, Json};
      (Json) ->
      unexpected_type_error(non_neg_integer, Json)
  end.

%% @doc Decodes a JSON number as a `float()'
%%
%% Note that JSON does not have a separate floating point type. As such,
%% integers in JSON will be cast to floats by this function.
%%
%% ```
%%    {ok, 123.0} = dj:decode(<<"123">>, dj:float()).
%% '''
%%
%% @see integer/0
%% @see pos_integer/0
%% @see neg_integer/0
%% @see non_neg_integer/0
-spec float() -> decoder(float()).
float() ->
  fun (Json) when is_float(Json)   -> {ok, Json};
      (Json) when is_integer(Json) -> {ok, float(Json)};
      (Json)                       -> unexpected_type_error(float, Json)
  end.

%% @equiv null(null)
-spec null() -> decoder(null).
null() ->
  null(null).

%% @doc Decodes `null' into an arbitrary value.
%%
%% This can be used to convert `null' to a specific value, like `undefined' or a
%% default value that makes sense for your application.
%%
%% ```
%%    {ok, foo} = dj:decode(<<"null">>, dj:null(foo)).
%% '''
%%
%% @see null/0
-spec null(V) -> decoder(V).
null(V) ->
  fun (null) -> {ok, V};
      (Json) -> unexpected_type_error(null, Json)
  end.

%% @doc Decodes a JSON `true' or `false' to a `boolean()'
%%
%% ```
%%    {ok, true} = dj:decode(<<"true">>, dj:boolean()).
%%    {ok, false} = dj:decode(<<"false">>, dj:boolean()).
%%    {error, _} = dj:decode(<<"null">>, dj:boolean()).
%% '''
-spec boolean() -> decoder(boolean()).
boolean() ->
  fun (Json) when is_boolean(Json) -> {ok, Json};
      (Json)                       -> unexpected_type_error(boolean, Json)
  end.

%% @doc Extracts a raw `jsx:json_term()'
-spec value() -> decoder(jsx:json_term()).
value() ->
  fun (X) -> {ok, X} end.

%% @doc Decodes a JSON value to an `atom()'
%%
%% If the JSON value is `true', `false' or `null', this returns an erlang atom
%% `true', `false' or `null'. If the JSON value is a string,
%% `binary_to_atom(Json, utf8)' is used to turn it into an atom.
%%
%% <strong>NOTE</strong>: Be careful with this function, as it may be used to
%% force erlang to create many, many new atoms, to the point of running out of
%% memory.
%%
%% @see existing_atom/0
%% @see atom/1
-spec atom() -> decoder(atom()).
atom() ->
  chain( value()
       , fun (Json) when is_atom(Json)   -> succeed(Json);
             (Json) when is_binary(Json) -> succeed(binary_to_atom(Json, utf8));
             (Json)                      -> fail({not_an_atom, Json})
         end
       ).

%% @doc Decodes a JSON value to one of a set of predefined atoms
%%
%% This is a safer alternative to `atom()', as it not only allows whitelisting
%% allowed values, but can also prevent creating new atoms.
-spec atom([atom(), ...]) -> decoder(atom()).
atom(Allowed) ->
  one_of(lists:map(fun (A) -> exactly(A, existing_atom()) end, Allowed)).

%% @doc Decodes a JSON value to an existing atom
%%
%% Note that Erlang, in some cases, may optimize atoms away. For example, if an
%% atom is only every used in an `atom_to_binary(some_atom)' call, the
%% `some_atom' atom may not "exist".
%%
%% @see atom/1
-spec existing_atom() -> decoder(atom()).
existing_atom() ->
  AtomizeOrFail =
    fun(Json) ->
        try succeed(binary_to_existing_atom(Json, utf8))
        catch error:badarg ->
            fail({not_an_atom, Json})
        end
    end,
  chain ( value()
        , fun (Json) when is_atom(Json)   -> succeed(Json);
              (Json) when is_binary(Json) -> AtomizeOrFail(Json);
              (Json)                      -> fail({not_an_atom, Json})
          end
        ).

%% @equiv atom(Allowed)
-spec existing_atom([atom(), ...]) -> decoder(atom()).
existing_atom(Allowed) ->
  atom(Allowed).

%% @doc Decodes a JSON string as a `binary()' if and only if it looks like an
%% email address.
%%
%% ```
%%    {ok, <<"ilias@truqu.com">>} =
%%        dj:decode(<<"\"ilias@truqu.com\"">>, dj:email()).
%% '''
%%
%% If the specified JSON is not a string, this will fail with an
%% `unexpected_type' error (expecing a `binary'). If the specified JSON is a
%% string but does not look like an email address, this will fail with a custom
%% `not_an_email' error.
%%
%% ```
%%    E = {dj_errors, [{custom, {not_an_email, <<"foo@bar">>}}]},
%%    {error, E} = dj:decode(<<"\"foo@bar\"">>, dj:email()).
%% '''
-spec email() -> decoder(binary()).
email() ->
  chain( map(fun string:lowercase/1, binary())
       , fun (V) ->
             case re:run( V
                        , <<"^[^@\s]+@([^.@\s]{2,}\.){1,}[a-z]{2,}$">>
                        , [{capture, none}]
                        ) of
               match -> succeed(V);
               _     -> fail({not_an_email, V})
             end
         end
       ).

-spec full_date_tuple(rfc3339) -> decoder(calendar:date()).
full_date_tuple(rfc3339) ->
  RE =  <<"^([0-9]+)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$">>,
  ToDateTuple =
    fun (B) ->
        case re:run(B, RE, [{capture, all_but_first, binary}]) of
          {match, [Y, M, D]} ->
            succeed({ erlang:binary_to_integer(Y)
                    , erlang:binary_to_integer(M)
                    , erlang:binary_to_integer(D)
                    });
          _ ->
            fail({malformed_date, B})
        end
    end,
  ValidateDateTuple =
    fun (Date = {Y, M, D}) ->
        case calendar:valid_date(Y, M, D) of
          true -> succeed(Date);
          false -> fail({invalid_date, Date})
        end
    end,
  chain( binary()
       , [ ToDateTuple
         , ValidateDateTuple
         ]
       ).

%% @doc Decode a UUIDv4 from a JSON string
-spec uuid(v4) -> decoder(binary()).
uuid(v4) ->
  Re = <<"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>,
  UuidFromBinary =
    fun (B) ->
        case re:run(B, Re) of
          {match, _} -> succeed(B);
          _          -> fail({invalid_uuid, v4, B})
        end
    end,
  chain(binary(), UuidFromBinary).

%% @doc Decode a bounded integer from JSON
%%
%% Occasionally, you may want to decode a JSON integer only when it sits between
%% certain bounds. Both the upper and lower bound are inclusive.
%%
%% ```
%%    -spec score() -> dj:decoder(1..10)
%%    score() ->
%%        dj:integer(1, 10).
%%
%%    {ok, 5} = dj:decode(<<"5">>, score()).
%%
%%    E = {dj_errors, [{custom, {integer_out_of_bounds, 1, 10, 0}}]},
%%    {error, E} = dj:decode(<<"0", score()).
%% '''
%%
%% @see integer/0
-spec integer(Min :: integer(), Max :: integer()) -> decoder(integer()).
integer(Min, Max) when Max < Min ->
  integer(Max, Min);
integer(Min, Max) ->
  CheckBounds =
    fun (Int) when Int >= Min andalso Int =< Max -> succeed(Int);
        (Int) -> fail({integer_out_of_bounds, Min, Max, Int})
    end,
  chain(integer(), CheckBounds).

%% @doc Decode a nullable value
%%
%% Sometimes, we explicitly want to allow `null'. In such a case, making that
%% clear by wrapping a decoder with `nullable/1' can help readability.
%%
%% ```
%%    -spec score() -> dj:decoder(1..10)
%%    score() ->
%%        dj:integer(1, 10).
%%
%%    {ok, 5} = dj:decode(<<"5">>, nullable(score())).
%%    {ok, null} = dj:decode(<<"null">>, nullable(score())).
%%
%%    E = {dj_error, [ {unexpected_type, integer, true}
%%                   , {unexpected_type, null, true}
%%                   ]},
%%    {error, E} = dj:decode(<<"true", nullable(score())).
%% '''
%%
%% @equiv nullable(Decoder, null)
-spec nullable(decoder(T)) -> decoder(T | null).
nullable(Decoder) ->
  nullable(Decoder, null).

%% @doc Decode a nullable value
%%
%% Sometimes, we explicitly want to allow `null' but use a default value. In
%% such a case, making that clear by wrapping a decoder with `nullable/2' can
%% help readability.
%%
%% ```
%%    -spec score() -> dj:decoder(1..10)
%%    score() ->
%%        dj:integer(1, 10).
%%
%%    {ok, 4} = dj:decode(<<"4">>, nullable(score(), 5)).
%%    {ok, 5} = dj:decode(<<"null">>, nullable(score(), 5)).
%%
%%    E = {dj_error, [ {unexpected_type, integer, true}
%%                   , {unexpected_type, null, true}
%%                   ]},
%%    {error, E} = dj:decode(<<"true", nullable(score(), 5)).
%% '''
%%
%% @equiv nullable(Decoder, null)

-spec nullable(decoder(T), V) -> decoder(T | V).
nullable(Decoder, Default) ->
  one_of([Decoder, null(Default)]).

%% @doc Instruct a decoder to match a value in a given field
%%
%% ```
%%    Dec = dj:field(foo, dj:binary()),
%%    {ok, <<"bar">>} = dj:decode(<<"{\"foo\": \"bar\"}">>, Dec),
%%
%%    Error = {unexpected_type, binary, null},
%%    InField = {in_field, foo, Error},
%%    {error, {dj_errors, [InField]}} = dj:decode(<<"{\"foo\": null}">>, Dec),
%%
%%    Missing = {missing_field, foo, #{}},
%%    {error, {dj_erros, [Missing]}} = dj:decode(<<"{}">>, Dec).
%% '''
%%
%% @see at/2
%% @see prop/2
%% @see to_map/1
%% @see prop_list/1
-spec field(field(), decoder(T)) -> decoder(T).
field(Field, Decoder) ->
  fun (#{Field := Value}) -> in_field(Field, Decoder(Value));
      (M) when is_map(M)  -> missing_field_error(Field, M);
      (Json)              -> unexpected_type_error(map, Json)
  end.

%% @doc Decodes a field or uses a default value when the field is missing
%%
%% Note that if the field is present but malformed according to the decoder,
%% decoding will fail. If we're not working in the context of a map/object,
%% decoding also fails.
%%
%% ```
%%    Dec = dj:optional_field(foo, dj:binary(), <<"default">>),
%%
%%    {ok, <<"bar">>} = dj:decode(<<"{\"foo\": \"bar\"}">>, Dec),
%%    {ok, <<"default">>} = dj:decode(<<"{}">>, Dec),
%%
%%    Error = {unexpected_type, binary, null},
%%    InField = {in_field, foo, [Error]},
%%    {error, {dj_error, [InField]}} = dj:decode(<<"{\"foo\": null}">>, Dec),
%%
%%    NotMap = {unexpected_type, map, <<"foo">>},
%%    {error, {dj_error, [NotMap]}} = dj:decode(<<"\"foo\"">>, Dec).
%% '''
-spec optional_field(field(), decoder(T), V) -> decoder(T | V).
optional_field(Field, Decoder, Default) ->
  fun (#{Field := Value}) -> in_field(Field, Decoder(Value));
      (M) when is_map(M)  -> {ok, Default};
      (Json)              -> unexpected_type_error(map, Json)
  end.

%% @doc Instruct a decode to match a value in a nested path
%%
%% ```
%%    {ok, null} = dj:decode(<<"null">>, dj:at([], dj:null())).
%%
%%    Dec = dj:at([foo, bar], dj:decode(dj:integer())),
%%    Json = <<"{\"foo\": {\"bar\": 123}}">>,
%%    {ok, 123} = dj:decode(Json, Dec).
%% '''
%%
%% @see field/2
%% @see prop/2
%% @see to_map/1
%% @see prop_list/1
-spec at([field()], decoder(T)) -> decoder(T).
at(Path, Decoder) ->
  lists:foldr(fun field/2, Decoder, Path).

%% @doc Decode a field as a property
%%
%% Similar to {@link field/2} but adds the fieldname to the decoded entry.
%% Convenient for decoding into a proplist.
%%
%% @see field/2
%% @see prop_list/1
-spec prop(field(), decoder(T)) -> decoder({field(), T}).
prop(Field, Decoder) ->
  map(fun (V) -> {Field, V} end, field(Field, Decoder)).

%% @doc Decode a proplist by matching fields on a JSON object
-spec prop_list([{field(), decoder(T)}]) -> decoder([{field(), T}]).
prop_list(Spec) ->
  sequence(lists:map(fun ({F, D}) -> prop(F, D) end, Spec)).

%% @doc Decode arbitrary JSON into a `map()'
%%
%% ```
%%    Dec = dj:to_map(#{ x => dj:index(0, dj:integer())
%%                     , y => dj:index(1, dj:integer())
%%                     , z => dj:index(2, dj:integer())
%%                     }),
%%    Json = <<"[1, 6, 2]">>,
%%    {ok, #{x := 1, y := 6, z := 2}} = dj:decode(Json, Dec).
%% '''
%%
%% @see field/2
-spec to_map(MapSpec) -> decoder(MapResult) when
    MapSpec :: #{Key := decoder(T)} | [{Key, decoder(T)}],
    MapResult :: #{Key := T}.
to_map(Spec) when is_list(Spec) ->
  Decoders = lists:map(fun({K, Decoder}) ->
                           map(fun (V) -> {K, V} end, Decoder)
                       end, Spec),
  map(fun maps:from_list/1, sequence(Decoders));
to_map(Spec) when is_map(Spec) ->
  to_map(maps:to_list(Spec)).

%% @doc Decode a JSON list using a decoder
-spec list(decoder(T)) -> decoder([T]).
list(Decoder) ->
  fun (Items) when is_list(Items) -> decode_all(Decoder, Items, 0, {ok, []});
      (Json)                      -> unexpected_type_error(list, Json)
  end.

%% @doc Decode a nonempty JSON list into a nonempty list `[T, ..]'
-spec nonempty_list(T) -> decoder([T, ...]).
nonempty_list(Decoder) ->
  fun (Json = [_X|_Xs]) -> decode_all(Decoder, Json, 0, {ok, []});
      (Json)            -> unexpected_type_error(nonempty_list, Json)
  end.

%% @doc Decode a single index in a JSON list using the specified decoder
-spec index(non_neg_integer(), decoder(T)) -> decoder(T).
index(Index, Decoder) ->
  fun (Items) when is_list(Items) ->
      case decode_nth(Index, Decoder, Items) of
        missing    -> {error, [{missing_index, Index, Items}]};
        {error, E} -> {error, [{at_index, Index, E}]};
        {ok, V}    -> {ok, V}
      end;
      (Json) -> unexpected_type_error(list, Json)
  end.

%% @doc Sequence a bunch of decoders, succeeding with the collected values
-spec sequence([decoder(T)]) -> decoder([T]).
sequence(Decoders) ->
  fun (Json) ->
      lists:foldr(sequence_helper(Json), {ok, []}, Decoders)
  end.

%% @doc Decode a JSON list into an erlang `sets:set(T)'
-spec set(decoder(T)) -> decoder(sets:set((T))).
set(Decoder) ->
  map(fun (Xs) -> sets:from_list(Xs) end, list(Decoder)).

%% @doc Manipulate the values produced by a decoder with a function
%%
%% ```
%%    Dec = dj:map(fun string:uppercase/1, dj:binary()),
%%    Json = <<"\"hello world\"">>,
%%    {ok, <<"HELLO WORLD">>} = dj:decode(Json, Dec).
%% '''
%%
%% @see mapn/2
%% @see chain/2
-spec map(fun((A) -> B), decoder(A)) -> decoder(B).
map(F, Decoder) ->
  fun (Json) ->
      case Decoder(Json) of
        {ok, V} -> {ok, F(V)};
        Error   -> Error
      end
  end.

%% @doc Chain one or more functions that create decoders onto a decoder
%%
%% This has many uses. One possible use is to handle data that may represent
%% different things:
%%
%% ```
%%    -type shape() :: {square, pos_integer()}
%%                   | {oblong, pos_integer(), pos_integer()}.
%%
%%    -spec square() -> dj:decoder(shape()).
%%    square() ->
%%        dj:map(fun (S) -> {square, S} end, dj:field(side, dj:pos_integer())).
%%
%%    -spec oblong() -> dj:decoder(shape()).
%%    oblong() ->
%%        dj:map( fun(L, W) -> {oblong, L, W} end
%%              , [ dj:field(length, dj:pos_integer())
%%                , dj:field(width, dj:pos_integer())
%%                ]
%%              ).
%%
%%    -spec shape(square | oblong) -> dj:decoder(shape()).
%%    shape(square) -> square();
%%    shape(oblong) -> oblong().
%%
%%    -spec shape() -> dj:decoder(shape()).
%%    shape() ->
%%        dj:chain(dj:field(type, dj:atom([square, oblong])), fun shape/1).
%%
%%    {ok, {square, 12}} =
%%        dj:decode(<<"{\"type\": \"square\", \"side\": 12}">>, shape()).
%% '''
%%
%% Occasionally, you may want to chain an operation that doesn't result in a
%% different decoder, but rather results in either failure or success. In that
%% case, use {@link succeed/1} or {@link fail/1}.
%%
%% When more than one function needs to be chained (for example, a function to
%% pattern match, and another function to validate the structure), the second
%% argument may be a list of functions.
%%
%% @see map/2
%% @see succeed/1
%% @see fail/1
-spec chain(decoder(A), ToDecoderB) -> decoder(B) when
    ToDecoderB :: ToDecB | [ToDecB],
    ToDecB :: fun((A) -> decoder(B)).
chain(DecoderA, Funs) when is_list(Funs) ->
  lists:foldl( fun (ToDec, Dec) -> chain(Dec, ToDec) end
             , DecoderA
             , Funs
             );
chain(DecoderA, ToDecoderB) ->
  fun (Json) ->
      case DecoderA(Json) of
        {ok, V}    -> (ToDecoderB(V))(Json);
        {error, E} -> {error, E}
      end
  end.

%% @doc Create a decoder that always fails with the provided term
%%
%% ```
%%    Dec = dj:fail(no_more_bananas),
%%    {error, {dj_errors, [{custom, no_more_bananas}]}}
%%      = dj:decode(<<"true">>, Dec).
%% '''
%%
%% Mostly useful when combined with `chain'.
%%
%% @see chain/2
%% @see succeed/1
-spec fail(E :: term()) -> decoder(V :: term()).
fail(E) ->
  fun (_) ->
      {error, [{custom, E}]}
  end.

%% @doc Create a decoder that always succeeds with the provided term
%%
%% ```
%%    Dec = dj:one_of([ dj:field(online, dj:boolean())
%%                    , dj:succeed(false)
%%                    ]),
%%    Json = << "[ {\"online\": true}"
%%            , ", {\"online\": false}"
%%            , ", {} ]"
%%           >>,
%%    {ok, [true, false, false]} = dj:decode(Json, dj:list(Dec)).
%% '''
%%
%% This function is also useful for hardcoding values in {@link to_map/1},
%% handling failure and success in {@link chain/2} and - as demonstrated -
%% defaulting values using {@link one_of/1}.
%%
%% @see chain/2
%% @see to_map/1
%% @see fail/1
%% @see one_of/1
-spec succeed(T) -> decoder(T).
succeed(V) ->
  fun (_) ->
      {ok, V}
  end.

%% @doc Apply an n-ary function against a list of n decoders
%%
%% ```
%%    Dec = dj:mapn( fun (X, Y, Z) -> {X, Y, Z} end
%%                 , [ dj:field(major, dj:pos_integer())
%%                   , dj:field(minor, dj:non_neg_integer())
%%                   , dj:field(patch, dj:non_neg_integer())
%%                   ]
%%                 ),
%%    Json = << "{ \"major\": 123"
%%            , ", \"minor\": 66"
%%            , ", \"patch\": 0"
%%            , "}">>,
%%    {ok, {123, 66, 0}} = dj:decode(Json, Dec).
%% '''
%%
%% When the arity doesn't match, a custom error is returned with the expected
%% arity (based on the number of decoders passed) and the actual arity of the
%% passed function.
%%
%% ```
%%    Dec = dj:mapn( fun (X, Y) -> {X, Y, 0} end
%%                 , [ dj:field(major, dj:pos_integer())
%%                   , dj:field(minor, dj:non_neg_integer())
%%                   , dj:field(patch, dj:non_neg_integer())
%%                   ]
%%                 ),
%%    Json = << "{ \"major\": 123"
%%            , ", \"minor\": 66"
%%            , ", \"patch\": 0"
%%            , "}">>,
%%    {error, {dj_errors, [{custom, {arity_mismatch, 3, 2}}]}}
%%      = dj:decode(Json, Dec).
%% '''
%%
%% @see map/2
%% @see chain/2
-spec mapn(Fun, [decoder(T)]) -> decoder(V) when
    Fun :: function(),
    T   :: term(),
    V   :: term().
mapn(Fun, Decoders) when is_function(Fun, length(Decoders))->
  map(fun (Vs) -> erlang:apply(Fun, Vs) end, sequence(Decoders));
mapn(Fun, Decoders) ->
  {arity, Actual} = erlang:fun_info(Fun, arity),
  fail({arity_mismatch, length(Decoders), Actual}).

%% @doc Decoding succeeds if the decoder produces exactly the supplied value.
%%
%% This can, for example, be used when a certain field is used to switch between
%% different decoders.
-spec exactly(V, decoder(V)) -> decoder(V).
exactly(V, Decoder) ->
  fun (Json) ->
      case Decoder(Json) of
        {ok, V} -> {ok, V};
        _       -> {error, [{not_exactly, V, Json}]}
      end
  end.

%% @doc Try a bunch of decoders. The first one to succeed will be used.
%%
%% If all decoders fails, the errors are accumulated.
%%
%% ```
%%    Dec = dj:one_of([dj:binary(), dj:integer()]),
%%    {ok, <<"foo">>} = dj:decode(<<"\"foo\"">>, Dec),
%%    {ok, 123} = dj:decode(<<"123">>, Dec),
%%    {error, _} = dj:decode(<<"null">>, Dec).
%% '''
-spec one_of([decoder(V), ...]) -> decoder(V).
one_of(Decoders) ->
  fun (Json) ->
      try_decoders(Decoders, Json, [])
  end.

%% @equiv dj:decode(Json, Decoder, [{labels, attempt_atom}])
-spec decode(Json, decoder(T)) -> result(T, {dj_error, errors()}) when
    Json :: jsx:json_text().
decode(Json, Decoder) ->
  decode(Json, Decoder, [{labels, attempt_atom}]).

%% @doc Run a {@type decoder(T)} against arbirary JSON.
%%
%% The resulting {@type result(T, error())} is either a tuple `{ok, T}' or a
%% tuple `{error, error()}' where {@type error()} represents whatever went wrong
%% during the decoding/validation/transformation process.
%%
%% `Opts` are passed on to `jsx:decode/2'. The option `return_maps' is always
%% added by {@module} and does not need to be specified manually.
%%
%% Use of the functions that create {@type decoder(T)}s and functions that help
%% with composition are discussed individually.
-spec decode(Json, decoder(T), Opts) -> result(T, {dj_error, errors()}) when
    Json     :: jsx:json_text(),
    Opts     :: [Opt],
    Opt      :: jsx:option()
              | labels
              | {labels, LabelOpt},
    LabelOpt :: atom
              | attempt_atom
              | binary
              | existing_atom.
decode(Json, Decoder, Opts) ->
  case attempt_jsx_decode(Json, Opts) of
    {ok, Data} ->
      case Decoder(Data) of
        {ok, _} = R ->
          R;
        {error, E} ->
          {error, {dj_error, E}}
      end;
    {error, E} -> {error, {dj_error, E}}
  end.

%%%-----------------------------------------------------------------------------
%%% Helpers
%%%-----------------------------------------------------------------------------

-spec attempt_jsx_decode(Json, Opts) -> result(Data, Invalid) when
    Json    :: binary(),
    Invalid :: [{invalid_json, Json}],
    Data    :: jsx:json_term(),
    Opts    :: [term()].
attempt_jsx_decode(Json, Opts) ->
  try
    V = jsx:decode(Json, [return_maps | Opts]),
    {ok, V}
  catch
    error:_ -> {error, [{invalid_json, Json}]}
  end.

-spec try_decoders([decoder(T)], Json, [error()])
                  -> result(T, errors()) when Json :: jsx:json_term().
try_decoders([], _Json, Es) ->
  {error, Es};
try_decoders([Decoder | Decoders], Json, Es) ->
  case Decoder(Json) of
    {ok, V} -> {ok, V};
    {error, E} -> try_decoders(Decoders, Json, Es ++ E)
  end.

-spec decode_nth(non_neg_integer(), decoder(T), [V])
                -> missing | result(T, error()) when V :: term().
decode_nth(0,  Decoder, [X | _Xs]) ->
  Decoder(X);
decode_nth(_, _Decoder, []) ->
  missing;
decode_nth(N, Decoder, [_X | Xs]) ->
  decode_nth(N - 1, Decoder, Xs).

-spec decode_all(decoder(T), [V], non_neg_integer(), ResM) -> Res when
    ResM :: result([T], [error()]),
    Res  :: result([T], error()),
    V    :: jsx:json_term().
decode_all(_Decoder, [], _Idx, {ok, Vs}) ->
  {ok, lists:reverse(Vs)};
decode_all(_Decoder, [], _Idx, {error, Es}) ->
  {error, lists:reverse(Es)};
decode_all(Decoder, [X | Xs], Idx, {ok, Vs}) ->
  Res = case Decoder(X) of
          {ok, V}    -> {ok, [V | Vs]};
          {error, E} -> {error, [{at_index, Idx, E}]}
        end,
  decode_all(Decoder, Xs, Idx + 1, Res);
decode_all(Decoder, [X | Xs], Idx, {error, Es}) ->
  Res = case Decoder(X) of
          {ok, _}    -> {error, Es};
          {error, E} -> {error, [{at_index, Idx, E} | Es]}
        end,
  decode_all(Decoder, Xs, Idx + 1, Res).

-spec in_field(field(), result(T, E)) -> result(T, E) when
    T :: term(),
    E :: errors().
in_field(_, Res = {ok, _}) ->
  Res;
in_field(Field, {error, E}) ->
  {error, [{in_field, Field, E}]}.

-spec unexpected_type_error(type(), jsx:json_term()) -> {error, errors()}.
unexpected_type_error(T, Json) ->
  make_error({unexpected_type, T, Json}).

-spec missing_field_error(field(), jsx:json_term()) -> {error, errors()}.
missing_field_error(F, Json) ->
  make_error({missing_field, F, Json}).

-spec make_error(error()) -> {error, errors()}.
make_error(E) ->
  {error, [E]}.

-spec sequence_helper(Json) -> fun((decoder(T), ResXs) -> ResXs) when
    Json :: jsx:json_text(),
    T    :: term(),
    ResXs :: {ok, [V :: term()]} | {error, errors()}.
sequence_helper(Json) ->
  fun (Decoder, Result) ->
      combine_results(Decoder(Json), Result)
  end.

-spec combine_results(ResX, ResXs) -> ResXs when
    ResX :: {ok, V} | {error, errors()},
    ResXs :: {ok, [V]} | {error, errors()},
    V :: term().
combine_results({error, E}, {error, Es}) ->
  {error, E ++ Es};
combine_results({error, E}, {ok, _}) ->
  {error, E};
combine_results({ok, V}, {ok, Vs}) ->
  {ok, [V | Vs]};
combine_results({ok, _}, {error, Es}) ->
  {error, Es}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
