-module(casefold_ffi).
-export([split_lines/1, split_words/1, format_compile/0, format_split/3]).

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

format_compile() ->
  binary:compile_pattern([<<"{">>, <<"}">>]).

format_split(Pattern, Format, Acc) ->
    case binary:match(Format, Pattern) of
        nomatch -> {binary:join([Acc, Format], <<"">>), <<"">>, <<"">>};
        {Index, Length} ->
            {binary:join([Acc, binary:part(Format, 0, Index)], <<"">>),
             binary:part(Format, Index, Length),
             binary:part(Format, Index + Length, byte_size(Format) - Index - Length)}
    end.
