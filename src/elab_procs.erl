-module(elab_procs).

-export([parse/1,
         info/0]).

-define(RE_RUN_OPTION, [{capture, all_names, binary}]).
-define(RE_PATTERN, <<"\\((?<MFA>.*) \\+ ">>).
    
info() ->
    parse(erlang:system_info(procs)).

parse(ProcsBin) ->
    {ok, MP} = re:compile(?RE_PATTERN),
    parse_dump(binary:split(ProcsBin, <<"\n">>, [global]), {[], []}, MP).

parse_dump([], {ProcAcc, PortAcc}, _MP) ->
    #{procs => ProcAcc, ports => PortAcc};
parse_dump([<<"=proc:", ProcId/binary>> | T], {ProcAcc, PortAcc}, MP) ->
    {Proc, Tail} = parse_dump_proc(T, #{pid => ProcId}, MP),
    parse_dump(Tail, {[Proc | ProcAcc], PortAcc}, MP);
parse_dump([<<"=port:", PortId/binary>> | T], {ProcAcc, PortAcc}, MP) ->
    {Port, Tail} = parse_dump_port(T, #{port => PortId}),
    parse_dump(Tail, {ProcAcc, [Port | PortAcc]}, MP).

%% -------------------------
%% Example of a process dump
%% -------------------------
%% =proc:<0.0.0>
%% State: Waiting
%% Name: init
%% Spawned as: erl_init:start/2
%% Spawned by: []
%% Message queue length: 0
%% Number of heap fragments: 0
%% Heap fragment data: 0
%% Link list: [<0.9.0>, <0.44.0>, <0.42.0>, <0.10.0>]
%% Reductions: 3266
%% Stack+heap: 987
%% OldHeap: 610
%% Heap unused: 844
%% OldHeap unused: 470
%% BinVHeap: 0
%% OldBinVHeap: 0
%% BinVHeap unused: 46422
%% OldBinVHeap unused: 46422
%% Memory: 13776
%% Last scheduled in for: module1:stack_test3/1">>
%% Stack dump:
%% Program counter: 0x00007fca33edd528 (init:loop/1 + 56)
%% arity = 0
%%
%% 0x00007f73e3840220 []
%%
%% 0x00007f73e3840228 Return addr 0x00007f73e8835900 (module1:stack_test3/1 + 64)
%%
%% 0x00007f73e3840230 Return addr 0x00007f73e8835898 (module2:stack_test2/1 + 64)
%%
%% 0x00007f73e3840238 Return addr 0x00007f73e8835830 (module3:stack_test1/0 + 64)
%%
%% 0x00007f73e3840240 Return addr 0x0000560877cfc748 (<terminate process normally>)
%%
%% 0x0000000104874940 Return addr 0x000000010320dac0 (<terminate process normally>)
%% Internal State: ACT_PRIO_NORMAL | USR_PRIO_NORMAL | PRQ_PRIO_NORMAL

parse_dump_proc([<<"State: ", State/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{state => State}, MP);
parse_dump_proc([<<"Name: ", Name/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{name => Name}, MP);
parse_dump_proc([<<"Spawned as: ", SpawnedAs/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{spawned_as => SpawnedAs}, MP);
parse_dump_proc([<<"Spawned by: ", SpawnedBy/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{spawned_by => SpawnedBy}, MP);
parse_dump_proc([<<"Message queue length: ", MessageQueueLen/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{message_queue_len => MessageQueueLen}, MP);
parse_dump_proc([<<"Number of heap fragments: ", HeapFragmentNum/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{heap_fragment_num => HeapFragmentNum}, MP);
parse_dump_proc([<<"Heap fragment data: ", HeapFragmentData/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{heap_fragment_data => HeapFragmentData}, MP);
parse_dump_proc([<<"Current call: ", CurrentCall/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{current_call => CurrentCall}, MP);
parse_dump_proc([<<"Last calls: ", LastCalls/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{last_calls => LastCalls}, MP);
parse_dump_proc([<<"Dictionary: ", Dictionary/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{dictionary => Dictionary}, MP);
parse_dump_proc([<<"Link list: ", Links/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{links => Links}, MP);
parse_dump_proc([<<"Reductions: 0">> | T], Map, MP) ->
    % a special case of zero reductions of the just spawned process
    % zeros don't work well with a logarithmic scale, so make it 1
    parse_dump_proc(T, Map#{reductions => <<"1">>}, MP);
parse_dump_proc([<<"Reductions: ", Reductions/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{reductions => Reductions}, MP);
parse_dump_proc([<<"Stack+heap: ", StackHeapSize/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{stack_heap_size => StackHeapSize}, MP);
parse_dump_proc([<<"OldHeap: ", OldHeap/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{old_heap_size => OldHeap}, MP);
parse_dump_proc([<<"Heap unused: ", HeapUnused/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{heap_unused_size => HeapUnused}, MP);
parse_dump_proc([<<"OldHeap unused: ", OldHeapUnused/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{old_heap_unused_size => OldHeapUnused}, MP);
parse_dump_proc([<<"BinVHeap: ", BinVHeap/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{bin_vheap_size => BinVHeap}, MP);
parse_dump_proc([<<"OldBinVHeap: ", OldBinVHeap/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{bin_old_vheap_size => OldBinVHeap}, MP);
parse_dump_proc([<<"BinVHeap unused: ", BinVHeapUnused/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{bin_vheap_unused_size => BinVHeapUnused}, MP);
parse_dump_proc([<<"OldBinVHeap unused: ", OldBinVHeapUnused/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{bin_old_vheap_unused_size => OldBinVHeapUnused}, MP);
parse_dump_proc([<<"Memory: ", Memory/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{memory => Memory}, MP);
parse_dump_proc([<<"Last scheduled in for: ", LastScheduled/binary>> | T], Map, MP) ->
    parse_dump_proc(T, Map#{last_scheduled_in_for => LastScheduled}, MP);
parse_dump_proc([<<"Stack dump:">> | T], Map, MP) ->
    {StackDump, T2} = parse_dump_stack(T, [], MP),
    parse_dump_proc(T2, Map#{stack_dump => StackDump}, MP);
parse_dump_proc([<<"Internal State: ", InternalState/binary>> | T], Map, _MP) ->
    {Map#{internal_state => InternalState}, T};
parse_dump_proc([], Map, _MP) ->
    {Map, []};
parse_dump_proc([_Line | T], Map, MP) ->
    parse_dump_proc(T, Map, MP).

parse_dump_stack([], StackDumpAcc, _MP) ->
    {StackDumpAcc, []};
parse_dump_stack([<<"Internal State: ", _/binary>> | _] = ProcsDump, StackDumpAcc, _MP) ->
    {StackDumpAcc, ProcsDump};
parse_dump_stack([<<"0x", _/binary>> = Line | T], StackDumpAcc, MP) ->
    case re:run(Line, MP, ?RE_RUN_OPTION) of
        nomatch ->
            parse_dump_stack(T, StackDumpAcc, MP);
        {match, [MFA]} ->
            parse_dump_stack(T, [MFA | StackDumpAcc], MP)
    end;
parse_dump_stack([_ | T], StackDumpAcc, MP) ->
    parse_dump_stack(T, StackDumpAcc, MP).

%% ----------------------
%% Example of a port dump
%% ----------------------
%% =port:#Port<0.4>
%% State: CONNECTED|BINARY_IO|PORT_LOCK
%% Task Flags: CHK_UNSET_BUSY_Q
%% Slot: 32
%% Connected: <0.58.0>
%% Links: <0.58.0>
%% Monitors: (<0.61.0>,#Ref<0.2601428397.3356491778.28209>)
%% Port controls linked-in driver: tcp_inet
%% Input: 0
%% Output: 20
%% Queue: 0
%% Port Data: inet_tcp
parse_dump_port([<<"State: ", State/binary>> | T], Map) ->
    parse_dump_port(T, Map#{state => State});
parse_dump_port([<<"Task Flags: ", TaskFlags/binary>> | T], Map) ->
    parse_dump_port(T, Map#{task_flags => TaskFlags});
parse_dump_port([<<"Slot: ", Slot/binary>> | T], Map) ->
    parse_dump_port(T, Map#{slot => Slot});
parse_dump_port([<<"Connected: ", Connected/binary>> | T], Map) ->
    parse_dump_port(T, Map#{connected => Connected});
parse_dump_port([<<"Links: ", Links/binary>> | T], Map) ->
    parse_dump_port(T, Map#{links => Links});
parse_dump_port([<<"Monitors: ", Monitors/binary>> | T], Map) ->
    parse_dump_port(T, Map#{monitors => Monitors});
parse_dump_port([<<"Port Data: ", PortData/binary>> | T], Map) ->
    parse_dump_port(T, Map#{port_data => PortData});
parse_dump_port([<<"Port ", _/binary>> = Desc | T], Map) ->
    parse_dump_port(T, Map#{desc => Desc});
% zeros don't work well with a logarithmic scale, so make it 1
parse_dump_port([<<"Input: 0">> | T], Map) ->
    parse_dump_port(T, Map#{input => <<"1">>});
parse_dump_port([<<"Output: 0">> | T], Map) ->
    parse_dump_port(T, Map#{output => <<"1">>});
parse_dump_port([<<"Input: ", Input/binary>> | T], Map) ->
    parse_dump_port(T, Map#{input => Input});
parse_dump_port([<<"Output: ", Output/binary>> | T], Map) ->
    parse_dump_port(T, Map#{output => Output});
parse_dump_port([<<"Queue: ", Queue/binary>> | T], Map) ->
    parse_dump_port(T, Map#{queue => Queue});
parse_dump_port([<<"=port:", _/binary>> | _] = PortDump, Map) ->
    {Map, PortDump};
parse_dump_port([<<>> | T], Map) ->
    {Map, T};
parse_dump_port([_Line | T], Map) ->
    parse_dump_port(T, Map).