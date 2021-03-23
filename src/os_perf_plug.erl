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

attributes(undefined) ->
    init_ets_tab(),
    attributes(?MODULE);
attributes(PrevState) ->
    Stacks = get_stacks(),
    ets:insert(?MODULE, {?MODULE, Stacks}),

    {TopNum, TopStack} = maps:fold(fun(Stack, Num, {MaxNum, _}) when Num > MaxNum ->
                                            {Num, Stack};
                                      (_, _, Acc) ->
                                           Acc
                                   end,
                                   {-2, [""]},
                                   Stacks),

    StackAttrs = lists:foldl(fun(Frame, Acc) ->
                                     [[#{content => " frame " ++ integer_to_list(length(Acc)), width => 19},
                                       #{content => Frame, width => 99}] | Acc]
                             end,
                             [],
                             TopStack),
    NumAttr = [[#{content => " # samples", width => 19},
                #{content => integer_to_list(TopNum), width => 99}]],
    
    {NumAttr ++ StackAttrs, PrevState}.

sheet_header() ->
    [
     #{title => "# samples", width => 20},
     #{title => "top of the call stack", width => 100}
    ].

sheet_body(undefined) ->
    init_ets_tab(),
    sheet_body(?MODULE);
sheet_body(PrevState) ->
    file:write_file("/tmp/example.txt", io_lib:format("~p sheet_body~n", [time()]), [append]),
    [{?MODULE, Stacks}] = ets:lookup(?MODULE, ?MODULE),
    SheetBody = lists:map(fun({Stack, Num}) ->
                                  TopFrame = lists:last(Stack),
                                  ColourTopFrame = case hd(TopFrame) of
                                                       $$ -> <<?GREEN/binary, (list_to_binary(TopFrame))/binary, ?RESET/binary>>;
                                                       _ -> TopFrame
                                                   end,
                                  [Num, ColourTopFrame] end, maps:to_list(Stacks)),

    {SheetBody, PrevState}.


init_ets_tab() ->
    case ets:info(?MODULE) of
        undefined ->
            ets:new(?MODULE, [named_table, public]);
        _ ->
            ok
    end.

get_stacks() ->
    OSPid = os:getpid(),
    Cmd = io_lib:format("sudo /usr/share/bcc/tools/profile -F 4000 -f -p ~s 1", [OSPid]),
    Result = os:cmd(Cmd),

    lists:foldl(fun([Stack, Num], Map) ->
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
               ).

