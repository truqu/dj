-module(dj).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ decode/2
        , object/0
        , array/0
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

decode(Json, Decoders) ->
  (compose(Decoders))(Json).

object() ->
  fun (Json) ->
      try
        case jsx:decode(Json, [return_maps]) of
          M = #{} -> {ok, M};
          _       -> error
        end
      catch
        error:_ -> error
      end
  end.

array() ->
  fun(Json) ->
      try
        case jsx:decode(Json, [return_maps]) of
          []        -> {ok, []};
          L = [_|_] -> {ok, L};
          _         -> error
        end
      catch
        error:_ -> error
      end
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
  M = #{foo => 42, bar => -23},
  {ok, M} = dj:decode(J, [ dj:object()
                         , dj_map:keys_as_atoms()
                         , dj_map:is_key(foo)
                         , dj_map:value_isa(foo, dj_int:is_pos())
                         , dj_map:put_default(bar, -23)
                         , dj_map:value_isa(bar, dj_int:is_neg())
                         ]
                     ),
  %% Test error case: invalid JSON
  error = dj:decode(<<>>, [dj:object(), dj_map:keys_as_atoms()]),
  %% Test error case: valid JSON, but not an object
  error = dj:decode(<<"[1, 2, 3]">>, [dj:object()]),
  %% Done
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

-endif.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
