-module(dj_maps).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ is_key/1
        , value_isa/2
        , put_default/2
        , update_with/2
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

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
        {ok, _} ->
          {ok, M};
        error ->
          error
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

update_with(K, F) ->
  fun
    ({ok, M})  when is_map(M) ->
      {ok, maps:update_with(K, F, M)};
    (_) ->
      error
  end.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
