

-record(seq_trace_request, {
    logging       = error_logger :: error_logger | ct | {trace_receiver, atom() | pid()},
    trace_send    = false        :: boolean(),
    trace_recv    = true         :: boolean(),
    trace_print   = true         :: boolean(),
    add_timestamp = false        :: boolean()
}).

-record(seq_trace, {
    label  :: integer(),
    flag,
    timestamp,
    serial :: {integer(), integer()},
    from   :: atom() | pid(),
    to     :: atom() | pid(),
    message
}).