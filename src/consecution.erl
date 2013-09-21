-module(consecution).

-behaviour(gen_server).

-include("consecution.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,

    trace/1,
    trace/2,
    is_trace_active/0,
    stop_tracing/0
    ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).

-record(state, { traces = dict:new() }).

trace(Label) ->
    trace(Label, #seq_trace_request{}).

trace(Label, Req) when is_integer(Label) ->
    #seq_trace_request{ 
        trace_send    = TS,
        trace_recv    = TR,
        add_timestamp = Stmp,
        trace_print   = P
    } = Req,

    % store *before* we start tracing
    gen_server:cast(?SERVER, {store_trace, Label, Req}),

    seq_trace:set_token(label, Label),
    seq_trace:set_token('receive', TR),
    seq_trace:set_token(print, P),
    seq_trace:set_token(send, TS),
    seq_trace:set_token(timestamp, Stmp),
    ok.

is_trace_active() ->
    seq_trace:get_token() =/= [].

stop_tracing() ->
    seq_trace:set_token([]). 

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    seq_trace:set_system_tracer(whereis(?SERVER)),
    {ok, #state{ }}.

handle_info(Info, #state{ traces = T } = S) when erlang:element(1, Info) == seq_trace ->
    Label     = erlang:element(2, Info),
    {ok, Req} = dict:find(Label, T),
    consecution_logger:print_trace(Req, Info),
    {noreply, S};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({store_trace, Label, Req}, #state{ traces = T } = S) ->
    {noreply, S#state{ traces = dict:store(Label, Req, T) }};
handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



