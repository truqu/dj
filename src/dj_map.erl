-module(dj_map).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ keys_as_atoms/0
        , is_key/1
        , value_isa/2
        , put_default/2
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

keys_as_atoms() ->
  fun
    ({ok, M}) when is_map(M) ->
      { ok
      , maps:from_list(
          [{erlang:binary_to_atom(K, utf8), V} || {K,V} <- maps:to_list(M)])
      };
    (_) ->
      error
  end.

is_key(K) ->
  fun
    ({ok, M})  when is_map(M) ->
      case maps:is_key(K, M) of
        false -> error;
        true  -> {ok, M}
      end;
    (_) ->
      error
  end.

value_isa(K, P) ->
  fun
    ({ok, M})  when is_map(M) ->
      V = maps:get(K, M),
      case P(V) of
        false -> error;
        true  -> {ok, M}
      end;
    (_) ->
      error
  end.

put_default(K, D) ->
  fun
    ({ok, M})  when is_map(M) ->
      {ok, maps:put(K, maps:get(K, M, D), M)};
    (_) ->
      error
  end.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

keys_as_atoms_test() ->
  %% Test simple case
  M1 = #{ <<"foo">> => 23, <<"bar">> => 42},
  M2 = #{ foo => 23, bar => 42},
  {ok, M2} = (keys_as_atoms())({ok, M1}),
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
