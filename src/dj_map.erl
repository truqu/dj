-module(dj_map).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ keys_as_atoms/0
        , has_key/1
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

has_key(K) ->
  fun
    ({ok, M})  when is_map(M) ->
      case maps:is_key(K, M) of
        false -> error;
        true  -> {ok, M}
      end;
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
