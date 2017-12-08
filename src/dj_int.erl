-module(dj_int).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ is_neg/0
        , is_pos/0
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

is_neg() ->
  dj:lift(
    fun
      (N)  when is_integer(N) ->
        N < 0;
      (_) ->
        false
    end
   ).

is_pos() ->
  dj:lift(
    fun
      (N)  when is_integer(N) ->
        N > 0;
      (_) ->
        false
    end
   ).

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
