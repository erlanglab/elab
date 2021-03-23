-module(os_perf_plug).

%% observer_cli_plugin Callback API
-export([attributes/1, sheet_header/0, sheet_body/1]).

attributes(PrevState) ->
    {[], PrevState}.

sheet_header() ->
    [
     #{title => "Header_1", width => 34},
     #{title => "Header_2", width => 34},
     #{title => "Header_3", width => 34}
    ].

sheet_body(PrevState) ->
    SheetBody = [
                 [row1hd1, row1hd2, row1hd3],
                 [row2hd1, row2hd2, row2hd3],
                 [row3hd1, row3hd2, row3hd3],
                 [row4hd1, row4hd2, row4hd3],
                 [row5hd1, row5hd2, row5hd3]
                ],
    {SheetBody, PrevState}.
