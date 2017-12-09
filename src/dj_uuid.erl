-module(dj_uuid).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ %% validation
          is_v4/0
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

is_v4() ->
  fun
    (X)  when is_binary(X) ->
      case re:run(X, <<"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>) of
        {match, _} -> true;
        _          -> false
      end;
    (_) ->
      false
  end.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

is_v4_test() ->
  true = (is_v4())(<<"2e2d65cb-437e-4abf-a7e8-7d97ad9d37a8">>),
  false = (is_v4())(<<"42c1f4f4-a81c-11e7-abc4-cec278b6b50a">>),
  ok.

-endif.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
