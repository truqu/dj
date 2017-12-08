-module(dj_datetime).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ is_full_date/1
        , full_date_to_tuple/1
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

is_full_date(rfc3339) ->
  RE =  <<"^([0-9]+)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$">>,
  fun
    (B) when is_binary(B) ->
      case re:run(B, RE, [{capture, all_but_first, binary}]) of
        nomatch ->
          error;
        {match, [Y, M, D]} ->
          ToInt = fun erlang:binary_to_integer/1,
          case calendar:valid_date(ToInt(Y), ToInt(M), ToInt(D)) of
            true ->
              {ok, B};
            false ->
              error
          end
      end;
    (_) ->
      error
  end.

full_date_to_tuple(rfc3339) ->
  RE =  <<"^([0-9]+)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$">>,
  fun (B) ->
      {match, [Y, M, D]} = re:run(B, RE, [{capture, all_but_first, binary}]),
      ToInt = fun erlang:binary_to_integer/1,
      {ToInt(Y), ToInt(M), ToInt(D)}
  end.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

is_full_date_test() ->
  %% Setup
  P = is_full_date(rfc3339),
  %% Test
  {ok, _} = P(<<"2018-04-01">>),
  error = P(<<"2017-02-31">>),
  error = P(<<"invalid">>),
  error = P("not a binary"),
  %% Done
  ok.

full_date_to_tuple_test() ->
  %% Test
  {2018, 4, 1} = (full_date_to_tuple(rfc3339))(<<"2018-04-01">>),
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
