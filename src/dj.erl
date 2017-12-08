-module(dj).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ decode/2
        , object/0
        , object/1
        , array/0
        , array/1
        , lift/1
        , is_text/0
        , is_email/0
        , any/1
        , equals/1
        , one_of/1
        , list_of/1
        , to_atom/0
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

decode(Json, Decoders) ->
  (compose(Decoders))(Json).

object() ->
  object([]).

object(Opts) ->
  fun (Json) ->
      try
        case jsx:decode(Json, [return_maps] ++ Opts) of
          M = #{} -> {ok, M};
          _       -> error
        end
      catch
        error:_ -> error
      end
  end.

array() ->
  array([]).

array(Opts) ->
  fun (Json) ->
      try
        case jsx:decode(Json, [return_maps] ++ Opts) of
          []        -> {ok, []};
          L = [_|_] -> {ok, L};
          _         -> error
        end
      catch
        error:_ -> error
      end
  end.

lift(F) ->
  fun (X) ->
      case F(X) of
        true ->
          {ok, X};
        false ->
          error
      end
  end.

is_text() ->
  lift(fun (X) -> erlang:is_binary(X) end).

is_email() ->
  lift(
    fun
      (T) when is_binary(T) ->
        match == re:run( T
                       , <<"^[^@\s]+@([^.@\s]{2,}\.){1,}[a-z]{2,}$">>
                       , [{capture, none}]
                       );
      (_) ->
        false
    end
   ).

any(L) when is_list(L) ->
  lift(fun (X) -> lists:any(fun ok/1, sequence(L, X)) end).

equals(X) ->
  lift(fun (Y) -> X =:= Y end).

one_of(L) ->
  lift(fun (X) -> lists:member(X, L) end).

list_of(P) ->
  lift(fun (L) -> lists:all(fun ok/1, lists:map(P, L)) end).

to_atom() ->
  fun
    (true) ->
      true;
    (false) ->
      false;
    (null) ->
      null;
    (X) when is_integer(X) ->
      erlang:integer_to_binary(erlang:binary_to_atom(X, utf8));
    (X) when is_float(X) ->
      erlang:float_to_binary(erlang:binary_to_atom(X, utf8));
    (X) when is_binary(X) ->
      erlang:binary_to_atom(X, utf8);
    (X) when is_atom(X) ->
      X
  end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

compose(Fs) when is_list(Fs) ->
  lists:foldl(fun compose/2, fun (X) -> X end, Fs).

compose(F, G) ->
  fun (X) -> F(G(X)) end.

sequence(Fs, X) ->
  sequence(Fs, X, []).

sequence([], _, Ys) ->
  lists:reverse(Ys);
sequence([F | Fs], X, Ys) ->
  sequence(Fs, X, [F(X) | Ys]).

ok({ok, _}) ->
  true;
ok(_) ->
  false.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

decode_object_test() ->
  %% Test simple case
  J = <<"{\"foo\": 42, \"date\": \"2001-01-01\", \"baz\": \"quux\", \"scores\": [1,2,3]}">>,
  M = #{foo => 42, bar => -23, date => {2001,1,1}, baz => quux, scores => [1,2,3]},
  {ok, M} =
    dj:decode(
      J,
      [ dj:object([{labels, atom}])
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
     ),
  %% Test error case: invalid JSON
  error = dj:decode(<<>>, [dj:object()]),
  %% Test error case: valid JSON, but not an object
  error = dj:decode(<<"[1, 2, 3]">>, [dj:object()]),
  %% Done
  ok.

is_email_test() ->
  {ok, _} = (is_email())(<<"michel.rijnders+2@gmx.co.uk">>),
  ok.

decode_array_test() ->
  %% Test simple case
  J = <<"[1, 2, 3]">>,
  L = [1, 2, 3],
  {ok, L} = dj:decode(J, [dj:array()]),
  %% Test empty array
  {ok, []} = dj:decode(<<"[]">>, [dj:array()]),
  %% Test error case: invalid JSON
  error = dj:decode(<<>>, [dj:array()]),
  %% Test error case: valid JSON, but not an array
  error = dj:decode(<<"{}">>, [dj:array()]),
  %% Done
  ok.

any_test() ->
  %% Test simple case
  Any = any([equals(foo), equals(bar), equals(42)]),
  {ok, _} = Any(foo),
  {ok, _} = Any(bar),
  {ok, _} = Any(42),
  error = Any(23),
  %% Done
  ok.

-endif.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
