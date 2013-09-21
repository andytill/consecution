%% common_test suite for consecution

-module(consecution_SUITE).

-include("consecution.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 20}}].

all() -> [test_consecution].

init_per_suite(Config) ->
    Config.
end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    Config.

test_consecution(_Config) ->
    consecution_app:start(a, a),
    timer:sleep(50),

    register(test_pid, self()),

    register(collector, spawn(?MODULE, msg_collector, [])),

    register(pid1, spawn(?MODULE, tracer, [pid2])),
    register(pid2, spawn(?MODULE, echoer, [pid3])),
    register(pid3, spawn(?MODULE, echoer, [pid4])),
    register(pid4, spawn(?MODULE, echoer, [test_pid])),

    receive
        hola ->
            % turn off tracing so we don't log any further messages
            consecution:stop_tracing(),
            ok
    after 2000 ->
        exit(fuck)
    end,

    % wait for the messges to be sent
    timer:sleep(500),

    collector ! {retrieve, self()},
    Collected = receive
        Msgs -> Msgs
    after 2000 ->
        exit(fucked_again)
    end,

    % check the messages are correct, last msg is first in the list
    8 = length(Collected),
    [
        _M1,
        _M2,
        _M3,
        _M4,
        _M5,
        _M6,
        _M7,
        _M8
    ] = Collected,

    #seq_trace{ flag = 'receive', from = pid4, to = test_pid } = _M1,
    #seq_trace{ flag = send,      from = pid4, to = test_pid } = _M2,
    #seq_trace{ flag = 'receive', from = pid3, to = pid4 }     = _M3,
    #seq_trace{ flag = send,      from = pid3, to = pid4}      = _M4,
    #seq_trace{ flag = 'receive', from = pid2, to = pid3 }     = _M5,
    #seq_trace{ flag = send,      from = pid2, to = pid3 }     = _M6,
    #seq_trace{ flag = 'receive', from = pid1, to = pid2 }     = _M7,
    #seq_trace{ flag = send,      from = pid1, to = pid2 }     = _M8,

    consecution_app:stop(normal),
    ok.

tracer(P) ->
    timer:sleep(1000),

    consecution:trace(888, #seq_trace_request{ 
        trace_send = true, 
        logging    = {trace_receiver, collector} 
    }), 
    P ! hola,
    tracer(P).

echoer(P) ->
    receive 
        M -> 
            true = consecution:is_trace_active(),
            P ! M
    end,
    echoer(P).

msg_collector() ->
    msg_collector1([]).

msg_collector1(C) ->
    receive
        {retrieve, From} -> From ! C;
        M                -> msg_collector1([M | C])
    end.


