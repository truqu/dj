-module(dj).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ decode/2
        , object/1
        , array/1
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

decode(Json, Decoders) ->
  (compose(Decoders))(Json).

-spec object(binary()) -> {ok, map()} | error.
object(Json) ->
  try
    case jsx:decode(Json, [return_maps]) of
      M = #{} -> {ok, M};
      _       -> error
    end
  catch
    error:_ -> error
  end.

-spec array(binary()) -> {ok, [any()]}.
array(Json) ->
  try
    case jsx:decode(Json, [return_maps]) of
      []        -> {ok, []};
      L = [_|_] -> {ok, L};
      _         -> error
    end
  catch
    error:_ -> error
  end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

compose(Fs) when is_list(Fs) ->
  lists:foldl(fun compose/2, fun (X) -> X end, Fs).

compose(F, G) ->
  fun (X) -> F(G(X)) end.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

decode_object_test() ->
  %% Test simple case
  J = <<"{\"foo\": 42}">>,
  M = #{foo => 42},
  {ok, M} = dj:decode(J, [ fun dj:object/1
                         , fun dj_map:keys_as_atoms/1
                         , dj_map:has_key(foo)
                         ]
                     ),
  %% Test error case: invalid JSON
  error = dj:decode(<<>>, [fun dj:object/1, fun dj_map:keys_as_atoms/1]),
  %% Test error case: valid JSON, but not an object
  error = dj:decode(<<"[1, 2, 3]">>, [fun dj:object/1]),
  %% Done
  ok.

decode_array_test() ->
  %% Test simple case
  J = <<"[1, 2, 3]">>,
  L = [1, 2, 3],
  {ok, L} = dj:decode(J, [fun dj:array/1]),
  %% Test empty array
  {ok, []} = dj:decode(<<"[]">>, [fun dj:array/1]),
  %% Test error case: invalid JSON
  error = dj:decode(<<>>, [fun dj:array/1]),
  %% Test error case: valid JSON, but not an array
  error = dj:decode(<<"{}">>, [fun dj:array/1]),
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
