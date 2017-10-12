-module(dj_int).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ is_neg/0
        , is_pos/0
        , is_pos_or_infinite/0
        , to_int_or_infinite/0
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

is_neg() ->
  fun
    (N)  when is_integer(N) ->
      N < 0;
    (_) ->
      false
  end.

is_pos() ->
  fun
    (N)  when is_integer(N) ->
      N > 0;
    (_) ->
      false
  end.

is_pos_or_infinite() ->
  fun
    (N) when is_integer(N) ->
      N > 0;
    (N) when is_binary(N) ->
      N == <<"infinite">>;
    (_) ->
      false
  end.

to_int_or_infinite() ->
  fun
    (N) when is_binary(N) ->
      erlang:display({binary, N}),
      infinite;
    (N) ->
      erlang:display({non_binary, N}),
      N
  end.


%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
