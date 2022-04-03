-module(attr).

-export([f/0]).
-import(t2, [what/0]).
-author("Me").
-random_tag(#{tag_map => fancy}).

f() ->
    hello.
