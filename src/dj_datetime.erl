-module(dj_datetime).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ is_full_date/1
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

is_full_date(rfc3339) ->
  RE =  <<"^([0-9]+)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$">>,
  fun
    (B) when is_binary(B) ->
      case re:run(B, RE, [{capture, none}]) of
        match   -> true;
        nomatch -> false
      end;
    (_) ->
      false
  end.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

is_full_date_test() ->
  %% Setup
  P = is_full_date(rfc3339),
  %% Test
  true = P(<<"2018-04-01">>),
  false = P(<<"invalid">>),
  false = P("not a binary"),
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
