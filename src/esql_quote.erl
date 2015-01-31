%%%--------------------------------------------------------
%%% @author Lzz <liuzhongzheng2012@gmail.com>
%%% @doc
%%%
%%% @end
%%% @Date: 2015-01-31 19:46:24
%%% @Last Modified by: Lzz
%%% @Last Modified time: 2015-01-31 20:09:51
%%%--------------------------------------------------------

-module(esql_quote).

-export([quote/1]).


-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(code:priv_dir(esql_quote), "esql_quote"), 0).

quote(_) ->
    erlang:nif_error("esql_quote not load").
