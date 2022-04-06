a() ->
    case A =/= undefined of
        true ->
            ok;
        _ ->
            throw(bad)
    end.
