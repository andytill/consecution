-module(consecution_logger).

-export([print_trace/2]).

-include("consecution.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

print_trace(#seq_trace_request{ logging = error_logger }, Trace) -> 
    error_logger:info_msg(to_string(Trace));
print_trace(#seq_trace_request{ logging = ct }, Trace) -> 
    ct:print(to_string(Trace));
print_trace(#seq_trace_request{ logging = {trace_receiver, Pid} }, Trace) -> 
    Pid ! to_seq_trace(Trace).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

to_seq_trace(T) ->
    {Flag, Serial, From, To, Msg} = element(3, T),
    #seq_trace{ 
        flag      = Flag,
        label     = element(2, T),
        serial    = Serial,
        from      = pid_name(From),
        to        = pid_name(To),
        message   = Msg,

        % timestamp will only be added if requested
        timestamp = case tuple_size(T) of
            3 -> undefined;
            4 -> element(4, T)
        end
    }.

to_string({seq_trace, Label, TraceInfo}) ->
    format("~p ~p", [Label, print_trace_info(TraceInfo)]);
to_string({seq_trace, Label, TraceInfo, TimeStamp}) ->
    format("~p ~p ~p", [Label, TimeStamp, print_trace_info(TraceInfo)]).

print_trace_info({print, Serial, From, _, Info}) ->
    format("~p printed ~p", [pid_name(From), Info]);
print_trace_info({'receive', Serial, From, To, Message}) ->
    format("~p ~p received from ~p >>> ~p", [Serial, pid_name(To), pid_name(From), Message]);
print_trace_info({send, Serial, From, To, Message}) ->
    format("~p ~p sent to ~p >>> ~p", [Serial, pid_name(From), pid_name(To), Message]).

format(Format, Data) ->
    R = io_lib:format(Format, Data),
    lists:flatten(R).

pid_name(P) when is_pid(P) ->
    case erlang:process_info(P, registered_name) of
        {registered_name, N}    -> N;
        _                       -> P
    end;
pid_name(S = [$< | _]) ->
    % for some reason we sometimes get the pid as a list so convert it back to a pid to
    % attempt to get it's registered name
    try
        pid_name(list_to_pid(S))
    catch
        _:_ -> S
    end;
pid_name(P) ->
    P.