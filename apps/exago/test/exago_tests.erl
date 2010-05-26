-module(exago_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TUPLE, {"A", "B", "C", "D"}).

tuple_values__test() ->
    "A" = exago_reader:tuple_values_(1, ?TUPLE),
    {"B", "C"} = exago_reader:tuple_values_([2, 3], ?TUPLE).

eval_cond_test() ->
    true = exago_reader:eval_cond({eq, [1], ["A"]}, ?TUPLE),
    true = exago_reader:eval_cond({'and',
				   {eq, [1], ["A"]},
				   {neq, [2, 3], ["C", "B"]}},
				  ?TUPLE).

parse_ts_test() ->
    {{{1994, 11, 15}, {8, 12, 31}}, 5651} =
	exago_reader:parse_ts({"Tue, 15 Nov 1994 08:12:31 GMT"}, rfc, 5651),
    parse_error =
	exago_reader:parse_ts({"Invalid timestamp string"}, rfc, 6000),
    {{{1934, 1, 11}, {16, 0, 30}}, 503000} =
	exago_reader:parse_ts({"11 Jan 1934-16:00:30.500000"},
			      "dd MMM yyyy-hh:mm:ss.ffffff", 3000),    
    parse_error =
	exago_reader:parse_ts({"01 Jan 2001-00:00:00:0000"},
			      "dd MMM yyyy-hh:mm:ss.ffffff", 0),
    parse_error =
	exago_reader:parse_ts({"Asdf"}, fun({_String}) ->
						erlang:error(internal_error)
					end, 0),
    parse_error =
	exago_reader:parse_ts({"Asdf"}, fun({String}) -> String end, 1000).

%% Just testing if any unhandled exception is thrown during the normal operation
sms_example_test() ->
    etc_example_gen:gen(500),
    exago:run(examples:conf_etc_sms()).

sms_example_timed_test() ->
    etc_example_gen:gen(500),
    exago:run(examples:conf_etc_sms_timed()).

sms_example_fauly_test() ->
    etc_example_gen:gen_faulty(500),
    exago:run(examples:conf_etc_sms_faulty()).

sms_example__timed_faulty_test() ->
    etc_example_gen:gen_faulty(500),
    exago:run(examples:conf_etc_sms_faulty_timed()).
