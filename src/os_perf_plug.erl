-module(os_perf_plug).

%% observer_cli_plugin Callback API
-export([attributes/1, sheet_header/0, sheet_body/1]).

attributes(PrevState) ->
    {[], PrevState}.

sheet_header() ->
    [
     #{title => "# samples", width => 25},
     #{title => "top of the call stack", width => 100}
    ].

sheet_body(PrevState) ->
    OSPid = os:getpid(),
    Cmd = io_lib:format("sudo /usr/share/bcc/tools/profile -F 4000 -f -p ~s 1", [OSPid]),
    Result = os:cmd(Cmd),

    M = lists:foldl(fun([Stack, Num], Map) ->
                            maps:update_with(Stack, fun(V) -> V + Num end, Num, Map)
                    end,
                    #{},
                    lists:sort(lists:map(fun(Line) ->
                                                 case string:tokens(Line, " ") of
                                                     [Stack, Num] -> [string:tokens(Stack, ";"), list_to_integer(Num)];
                                                     _ -> [Line, -1]
                                                 end
                                         end,
                                         string:tokens(Result, "\n")
                                        )
                              )
                   ),
    SheetBody = lists:map(fun({Stack, Num}) -> [Num, lists:last(Stack)] end, maps:to_list(M)),

    {SheetBody, PrevState}.
