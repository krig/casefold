-module(casefold_ffi).
-export([split_lines/1]).

split_lines(String) ->
  case re:compile(<<"\r\n|\r|\n">>) of
    {ok, Compiled} -> re:split(String, Compiled, [{return, binary}]);
    {error, {_Str, _Pos}} -> []
  end.
