-module(casefold_ffi).
-export([split_lines/1, split_words/1]).

split_lines(String) ->
  case re:compile(<<"\r\n|\r|\n">>, [unicode, ucp]) of
    {ok, Compiled} -> re:split(String, Compiled, [{return, binary}]);
    {error, {_Str, _Pos}} -> []
  end.

split_words(String) ->
  case re:compile(<<"\\s+">>, [unicode, ucp, multiline]) of
    {ok, Compiled} -> re:split(String, Compiled, [{return, binary}]);
    {error, {_Str, _Pos}} -> []
  end.
