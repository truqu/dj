-module(dj_lists).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ map/1
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

map(F) when is_function(F) ->
  fun
    ({ok, L}) when is_list(L) -> {ok, [F(X) || X <- L]};
    (_)                       -> error
  end.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

map_test() ->
  %% Test simple case
  {ok, [2, 3, 4]} = (map(fun (X) -> X + 1 end))({ok, [1, 2, 3]}),
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
