-module(dj).

%% API
-export([ object/1
        , array/1
        ]
       ).

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

-spec object(binary()) -> {ok, map()} | error.
object(Json) ->
  try
    case jsx:decode(Json, [return_maps]) of
      M = #{} -> {ok, M};
      _       -> error
    end
  catch
    error:_ -> error
  end.

-spec array(binary()) -> {ok, [any()]}.
array(Json) ->
  try
    case jsx:decode(Json, [return_maps]) of
      []        -> {ok, []};
      L = [_|_] -> {ok, L};
      _         -> error
    end
  catch
    error:_ -> error
  end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
