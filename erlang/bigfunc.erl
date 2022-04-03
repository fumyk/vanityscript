fu(A, B) ->
    C = A + B,
    erlang:send(self(), C).
