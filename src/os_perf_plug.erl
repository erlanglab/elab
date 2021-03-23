-module(os_perf_plug).

-define(RESET_BG, <<"\e[49m">>).
-define(RESET, <<"\e[0m">>).
-define(GRAY_BG, <<"\e[7m">>).
-define(YELLOW, <<"\e[33m">>).
-define(RED, <<"\e[31m">>).
-define(L_RED, <<"\e[48m">>).
-define(GREEN, <<"\e[32;1m">>).
-define(L_GREEN, <<"\e[92m">>).
-define(CHOOSE_BG, <<"\e[42m">>).
-define(RED_BG, <<"\e[48;2;184;0;0m">>).
-define(L_GRAY_BG, <<"\e[48;2;80;80;80m">>).
-define(UNDERLINE, <<"\e[4m">>).

%% observer_cli_plugin Callback API
-export([attributes/1, sheet_header/0, sheet_body/1]).

attributes(PrevState) ->
    Attrs =
        [
         %% [#{content => "$global::int_div_rem_body_shared", width => 100, color => ?L_GREEN}],
         %% [#{content => "do_syscall_64", width => 100}],
         %% [#{content => "entry_SYSCALL_64_after_hwframe", width => 100}],
         %% [#{content => "epoll_wait", width => 100}],
         %% [#{content => "1_scheduler", width => 100, color => ?L_RED}]
        ],

    {Attrs, PrevState}.

sheet_header() ->
    [
     #{title => "# samples", width => 25},
     #{title => "top of the call stack", width => 100}
    ].

sheet_body(PrevState) ->
    SheetBody = get_stacks(),
    {SheetBody, PrevState}.


get_stacks() ->
    OSPid = os:getpid(),
    Cmd = io_lib:format("sudo /usr/share/bcc/tools/profile -F 4000 -f -p ~s 1", [OSPid]),
    Result = os:cmd(Cmd),

    M = lists:foldl(fun([Stack, Num], Map) ->
                            maps:update_with(Stack, fun(V) -> V + Num end, Num, Map)
                    end,
                    #{},
                    lists:sort(lists:map(fun(Line) ->
                                                 case string:split(Line, " ", trailing) of
                                                     [Stack, Num] ->
                                                         case catch list_to_integer(Num) of
                                                             NumInt when is_integer(NumInt) ->
                                                                 [string:tokens(Stack, ";"), NumInt];
                                                             _ ->
                                                                 [["skipped"], -1]
                                                         end;
                                                     _ -> [[Line], -1]
                                                 end
                                         end,
                                         string:tokens(Result, "\n")
                                        )
                              )
                   ),
    lists:map(fun({Stack, Num}) ->
                      TopFrame = lists:last(Stack),
                      ColourTopFrame = case hd(TopFrame) of
                                           $$ -> <<?GREEN/binary, (list_to_binary(TopFrame))/binary, ?RESET/binary>>;
                                           _ -> TopFrame
                                       end,
                      [Num, ColourTopFrame] end, maps:to_list(M)).

