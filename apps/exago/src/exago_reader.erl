%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author  Hans Svensson
%%% @doc Reader
%%%
%%% Created : 22 Apr 2010 by Atilla Erdodi

%%% Copyright (c) 2009,2010  Erlang Solutions formerly Erlang Training & Consulting 
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%% * Neither the name of the Erlang Solutions nor the names of its
%%%   contributors may be used to endorse or promote products
%%%   derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-module(exago_reader).
-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').

-export([read_files/1]).

-export([eval_cond/2]). % needed by the validator function in exago_conf

-compile(export_all).

%% @spec read_files(Conf::list()) -> TrTbl::tid()
%% @doc Reads and parses the log files
-spec read_files(list()) -> tid().
read_files(Conf) ->
    % ETS table to store events to abstract, and temporary mapping information
    Tbl  = ets:new(abstr_event_tbl, [duplicate_bag, public]),
    Tbl2 = ets:new(abstr_event_tbl_2, [duplicate_bag, public]),
    Files = exago_conf:get_files(Conf),
    
    [exago_events:e_info(lists:flatten(io_lib:format("  ~p", [X])))
     || X <- Files],
    lists:foreach(
      fun({FileName, FileDetailsProplist}) ->
	      read_file(Tbl, Tbl2, FileName, FileDetailsProplist)
      end, exago_conf:get_files(Conf)),
    ets:match_delete(Tbl, {{'$mapping', '_', '_'}, '_'}),
    {ok, {Tbl, Tbl2}}.

%% @spec read_file(Tbl::tid(), Tbl2::tid(), File::string(),
%%                 FileDetails::list()) -> TrTbl
%% @doc Reads and parses a log file
-spec read_file(tid(), tid(), string(), list()) -> {tid(), tid()}.
read_file(Tbl, Tbl2, FileName, FileDetails) ->
    ParseFun = proplists:get_value(parse_fun, FileDetails, csv),
    ParseOpts = proplists:get_value(parse_opts, FileDetails),
    Data = parse_file(FileName, ParseFun, ParseOpts),
    build_events_tbl(Tbl, Tbl2, FileName, FileDetails, Data).

%% @spec build_events_tbl(Tbl::tid(), Tbl2::tid(),
%%                        FileName::string(), FileDetails::list(),
%%                        Data::list(tuple())) -> {Tbl::tid(), Tbl2::tid()}
%% @doc Builds the events table, extracts the specified values
%%      and resolves the relations. Events with transaction id
%%      will be put into the first table, events w/o into the second
-spec build_events_tbl(tid(), tid(), string(), list(), list(tuple())) -> {tid(), tid()}.
build_events_tbl(Tbl, Tbl2, _FileName, _FileDetails, []) ->
    {Tbl, Tbl2};
build_events_tbl(Tbl, Tbl2, FileName, FileDetails, [Tuple | Data]) ->
    Filter = proplists:get_value(filter, FileDetails, true),
    case eval_cond(Filter, Tuple) of
	true ->
	    [TsFormat, Offset, Value, Time, SessId, TrId, Mappings] =
		[proplists:get_value(Key, FileDetails) ||
		    Key <- [ts_format, offset, abstract_value, timestamp,
			    session_id, transaction_id, mappings]],
	    TheValue = tuple_values(Tbl, Value, Tuple, FileName),
	    TheTime = case Time of
			  undefined -> no_timestamp;
			  []   -> no_timestamp;
			  _  ->
			      parse_ts(
				tuple_values(Tbl, Time, Tuple, FileName),
				       TsFormat, Offset)
			  end,
	    TheTrId = tuple_values(Tbl, TrId, Tuple, FileName),
	    TheSessId = tuple_values(Tbl, SessId, Tuple, FileName),
	    case Mappings of
		undefined ->
		    no_mappings;
		[] ->
		    no_mappings;
		_ ->
		    lists:foreach(fun({Id, FromFlds, ToFlds}) ->
					  FromValue = tuple_values(Tbl, FromFlds, Tuple, FileName),
					  case ToFlds of
					      transaction_id ->
						  insert_mapping(Tbl, Id, FromValue, TheTrId);
					      session_id ->
						  insert_mapping(Tbl, Id, FromValue, TheSessId);
					      _ ->
						  ToValue =
						      tuple_values(Tbl, ToFlds, Tuple, FileName),
						  insert_mapping(Tbl, Id, FromValue, ToValue)
					  end;
				     ({Id, FromFlds, ToFlds, Cond}) ->
					  case _CondRes = eval_cond(Cond, Tuple) of
					      true ->
						  FromValue = tuple_values(Tbl, FromFlds, Tuple, FileName),
						  case ToFlds of
						      transaction_id ->
							  insert_mapping(Tbl, Id, FromValue, TheTrId);
						      session_id ->
							  insert_mapping(Tbl, Id, FromValue, TheSessId);
						      _ ->
							  ToValue = tuple_values(Tbl, ToFlds, Tuple, FileName),
							  insert_mapping(Tbl, Id, FromValue, ToValue)
						  end;
					      false ->
						  ok
					  end				  
				  end, Mappings)
	    end,
	    insert_record(Tbl, Tbl2, TheTrId, TheSessId, TheTime, TheValue);
	_ ->
	    ok
    end,
    build_events_tbl(Tbl, Tbl2, FileName, FileDetails, Data).

lookup_mapping(Tbl, {Id, FromValue}) ->
    case ets:lookup(Tbl, {'$mapping', Id, FromValue}) of
        [{_, ToValue}] ->
            ToValue;
        L = [{_, TheKey} | _] ->
	    % Emit a warning and return the first match
	    exago_events:e_nondet_map(Id, L),
            TheKey;
        [] ->
	    exago_events:e_undef_map(Id, FromValue),
            none
    end.

insert_mapping(Tbl, Id, FromValue, ToValue) ->
    ets:insert(Tbl, {{'$mapping', Id, FromValue}, ToValue}).
insert_record(_Tbl, Tbl2, none, TheSessId, TheTime, TheAbstrValue) ->
    ets:insert(Tbl2, {TheSessId, {{TheTime, TheAbstrValue}, TheSessId}});
insert_record(Tbl, _Tbl2, TheTrId, TheSessId, TheTime, TheAbstrValue) ->
    ets:insert(Tbl, {TheTrId, {{TheTime, TheAbstrValue}, TheSessId}}).

%% @spec tuple_values(Tbl::tid(),FieldDef::field_def(),Line::tuple(),Src::string()) -> tuple()
%% @doc Extract values from a parsed line and resolves the dependencies
tuple_values(_Tbl, undefined, _Tuple, _Filename) ->
    none;
tuple_values(_Tbl, none, _Tuple, _FileName) ->
    none;
tuple_values(_Tbl, [], _Tuple, _FileName) ->
    none;
tuple_values(_Tbl, filename, _Tuple, FileName) ->
    FileName;
tuple_values(Tbl, {MapId, FromFlds}, Tuple, Src) ->
    FromValue = tuple_values(Tbl, FromFlds, Tuple, Src),
    lookup_mapping(Tbl, {MapId, FromValue});
tuple_values(_Tbl, N, Tuple, _FileName) when is_integer(N) ->
    element(N, Tuple);
tuple_values(Tbl, Flds, Tuple, FileName) when is_list(Flds) ->
    list_to_tuple([if
		       N == filename ->
			   FileName;
                       is_integer(N) ->
                           case length(tuple_to_list(Tuple)) >= N of
                               true  -> element(N, Tuple);
                               false -> none
                           end;
                       is_tuple(N) ->
                           tuple_values(Tbl, Flds, Tuple, FileName)
                   end || N <- Flds]).

%% @spec tuple_values_(FieldDef::field_def(), Line::tuple()) -> tuple()
%% @doc Extract values from a parsed line but doesn't resolves the dependencies.
%%      Used when evaluating filter conditions
tuple_values_(none, _Tuple) ->
    none;
tuple_values_([], _Tuple) ->
    none;
tuple_values_(N, Tuple) when is_integer(N) -> 
    element(N, Tuple);
tuple_values_(Flds, Tuple) when is_list(Flds) -> 
    list_to_tuple([if
                       is_integer(N) ->
                           case length(tuple_to_list(Tuple)) >= N of
                               true  -> element(N, Tuple);
                               false -> none % emit warning?
                           end;
                       is_tuple(N) ->
                           tuple_values_(Flds, Tuple)
                   end || N <- Flds]).

eval_cond(true, _Tuple) ->
    true;
eval_cond(none, _Tuple) ->
    true;
eval_cond([], _Tuple) ->
    true;
eval_cond(Cond, Tuple) when is_list(Cond) ->
    eval_and(Cond, Tuple);
eval_cond(Cond, Tuple) when is_tuple(Cond) ->
    case element(1, Cond) of
        'and' -> eval_and(tl(tuple_to_list(Cond)),Tuple);
        'or' -> eval_or(tl(tuple_to_list(Cond)),Tuple);
        'xor' -> eval_xor(tl(tuple_to_list(Cond)),Tuple);
	_ -> case Cond of
		 {neq, FieldSpec,Values} -> not eval_eq(FieldSpec,Values,Tuple);
		 {eq, FieldSpec,Values} -> eval_eq(FieldSpec,Values,Tuple);
		 {'not', Cond} -> not eval_cond(Cond,Tuple);
		 _ -> syntax_error
	     end
    end;
eval_cond(_, _Tuple) ->
    syntax_error.

eval_eq(FieldSpec, Values, Tuple)  when is_list(Values) ->
    Values2 = tuple_values_(FieldSpec, Tuple),
    Values2 == list_to_tuple(Values);
eval_eq(FieldSpec, Values, Tuple) when is_tuple(Values)->
    Values2 = tuple_values_(FieldSpec, Tuple),
    Values2 == Values.

eval_and([],_Tuple) ->
    syntax_error;
eval_and([Cond],Tuple) ->
    eval_cond(Cond,Tuple);
eval_and([Cond | Rest],Tuple) ->
    case eval_cond(Cond,Tuple) of
	true ->
	    eval_and(Rest,Tuple);
	false ->
	    false;
	_ ->
	    syntax_error
    end.

eval_or([],_Tuple) ->
    parse_error;
eval_or([Cond],Tuple) ->
    eval_cond(Cond,Tuple);
eval_or([Cond | Rest],Tuple) ->
    case eval_cond(Cond,Tuple) of
	false ->
	    eval_or(Rest,Tuple);
	true ->
	    true;
	_ ->
	    syntax_error
    end.

eval_xor([],_Tuple) ->
    parse_error;
eval_xor([Cond],Tuple) ->
    eval_cond(Cond,Tuple);
eval_xor([Cond | Rest],Tuple) ->
    case eval_cond(Cond,Tuple) of
	true ->
	    not eval_or(Rest,Tuple);
	false ->
	    eval_xor(Rest,Tuple);
	_ ->
	    syntax_error
    end.

%% @spec init(List::list()) -> List2::list()
%% @doc Removes the tail of a list
-spec init(list()) -> list().
init(List) ->
    lists:reverse(tl(lists:reverse(List))).

%% @spec parse_file(File::string(), ParseFun::list(), ParseOpts::list()) ->
%%                      Data::list(tuple())
%% @doc Read and parse a CSV-logfile
-spec parse_file(string(), function(), list()) -> list(tuple()).
parse_file(File, ParseFun, ParseOpts) ->
    {ok, IoDev} = file:open(File, [read]),
    Data = parse_file(IoDev, file:read_line(IoDev), ParseFun, ParseOpts),
    file:close(IoDev),
    Data.

parse_file(_IoDev, eof, _ParseFun, _ParseOpts) ->
    [];
parse_file(IoDev, {ok, Str}, ParseFun, ParseOpts) ->
    [parse_str(init(Str), ParseFun, ParseOpts) |
     parse_file(IoDev, file:read_line(IoDev), ParseFun, ParseOpts)].

parse_str(Str, csv, ParseOpts) ->
    Delimiter = proplists:get_value(delimiter, ParseOpts),
    SStr = tokens(Str,Delimiter),
    list_to_tuple(SStr);
parse_str(Str, ParseFun, ParseOpts) ->
    try ParseFun(Str, ParseOpts) of
	Result ->
	    Result
    catch
	Class:Error ->
	    exago_events:e_parsefun_error(Class, Error),
	    throw({parsefun_error, Class, Error})
    end.

%% @spec tokens(String::string(), Seperators::string()) ->
%%           ListOfStrings::list(string())
%% @doc Return a list of tokens seperated by characters in Seperators.
%%      Unlike the function in the string module,
%%      it handles empty fields as well.
-spec tokens(string(), string()) -> list(string()).
tokens(S, Seps) ->
    tokens_acc(S, Seps, [], []).

tokens_acc([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
	true -> tokens_acc(S, Seps, [lists:reverse(Cs)|Toks], []);
	false -> tokens_acc(S, Seps, Toks, [C|Cs])
    end;
tokens_acc([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).

%% @spec parse_ts(TsString::string(), Format::string() | atom(),
%%                Offset::integer()) -> ParsedTs::tuple(integer())
%% @doc Parse timestamp string
-spec parse_ts(string()|tuple(), string() | atom, integer()) ->
    {tuple(integer()), integer()}.
parse_ts(TimestampStr, TsParseFun, Offset) when is_function(TsParseFun)->
    case (catch TsParseFun(TimestampStr)) of
	{{{Year, Month, Day}, {Hour, Minute, Second}}, MicroSec} ->
	    case (catch add_offset({{{Year, Month, Day},
				     {Hour, Minute, Second}}, MicroSec}, Offset)) of
		{{{Year2, Month2, Day2}, {Hour2, Minute2, Second2}}, MicroSec2} ->
		    {{{Year2, Month2, Day2}, {Hour2, Minute2, Second2}}, MicroSec2};
		_ ->
		    parse_error
	    end;
	_ ->
	    parse_error
    end;
parse_ts(Timestamp, noparse, _Offset) ->
    Timestamp;
parse_ts({TimestampStr}, rfc, Offset) ->
    case httpd_util:convert_request_date(TimestampStr) of
	bad_date ->
	    parse_error;
	{{Year, Month, Day}, {Hour, Minute, Second}} ->
	    MicroSec = 0,
	    add_offset({{{Year, Month, Day}, {Hour, Minute, Second}}, MicroSec}, Offset)
    end;
parse_ts({MegaSecStr, SecStr, MicroSecStr}, erlang, Offset) ->
    case catch {list_to_integer(MegaSecStr),
		list_to_integer(SecStr),
		list_to_integer(MicroSecStr)} of
	{MegaSec, Sec, MicroSec} ->
	    {{Year, Month, Day}, {Hour, Minute, Second}} =
		calendar:now_to_datetime({MegaSec, Sec, MicroSec}),
	    add_offset({{{Year, Month, Day}, {Hour, Minute, Second}}, MicroSec}, Offset);
	_ ->
	    parse_error
    end;
parse_ts({TimestampStr}, FormatStr, Offset) ->
    case parse_ts_(TimestampStr, FormatStr) of
	parse_error ->
	    parse_error;
	{{Year, Month, Day, Hour, Minute, Second, SecFract}, FracLength} ->
	    MicroSec = trunc(SecFract * math:pow(10, 6 - FracLength)),
	    add_offset({{{Year, Month, Day},
			 {Hour, Minute, Second}}, MicroSec}, Offset)
    end.
    
parse_ts_(TimestampStr, FormatStr) ->
    case parse_ts_acc(TimestampStr, FormatStr, {[],[],[],[],[],[],[]}) of
	parse_error ->
	    parse_error;
	{StrList, FracLength} ->
	    ParsedList = [element(1, string:to_integer(Str)) || Str <- StrList],
	    case lists:any(fun(Int) -> Int == error end, ParsedList) of
		true -> parse_error;
		false -> {list_to_tuple(ParsedList), FracLength}
	    end
    end.

parse_ts_acc([], [], {Year, Month, Day, Hour, Minute, Second, SecFrac}) ->
    Month2 = case string:to_upper(lists:reverse(Month)) of
		 "JAN" -> "01";
		 "FEB" -> "02";
		 "MAR" -> "03";
		 "APR" -> "04";
		 "MAY" -> "05";
		 "JUN" -> "06";
		 "JUL" -> "07";
		 "AUG" -> "08";
		 "SEP" -> "09";
		 "OCT" -> "10";
		 "NOV" -> "11";
		 "DEC" -> "12";
		 Int -> Int
	     end,
    {[lists:reverse(Year), Month2, lists:reverse(Day),
      lists:reverse(Hour), lists:reverse(Minute), lists:reverse(Second),
      lists:reverse(SecFrac)], length(SecFrac)};
parse_ts_acc([], _, _) ->
    parse_error;
parse_ts_acc(_, [], _) ->
    parse_error;
parse_ts_acc([TsHead | TsRest], [FHead | FRest],
	     {Year, Month, Day, Hour, Minute, Second, SecFract}) ->
    case FHead of
	$y -> parse_ts_acc(TsRest, FRest,
		       {[TsHead | Year], Month, Day,
			Hour, Minute, Second, SecFract});
	$M -> parse_ts_acc(TsRest, FRest,
			   {Year, [TsHead | Month], Day,
			    Hour, Minute, Second, SecFract});
	$d -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, [TsHead | Day],
			    Hour, Minute, Second, SecFract});
	$h -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, Day,
			    [TsHead | Hour], Minute, Second, SecFract});
	$m -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, Day,
			    Hour, [TsHead | Minute], Second, SecFract});
	$s -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, Day,
			    Hour, Minute, [TsHead | Second], SecFract});
	$f -> parse_ts_acc(TsRest, FRest,
			   {Year, Month, Day,
			    Hour, Minute, Second, [TsHead | SecFract]});
	%% Space in the format string indicates arbitrary character
	32 -> parse_ts_acc(TsRest, FRest,{Year, Month, Day,
					  Hour, Minute, Second, SecFract});
	TsHead -> parse_ts_acc(TsRest, FRest,
			       {Year, Month, Day,
				Hour, Minute, Second, SecFract});
	_Other -> parse_error
    end.

add_offset({{{Year, Month, Day}, {Hour, Minute, Second}}, MicroSec}, Offset) ->
    MSSum = MicroSec + Offset,
    MicroSec2 = MSSum rem 1000000,
    MSDiv = MSSum div 1000000,
    SecSum = Second + MSDiv,
    Second2 = SecSum rem 60,
    SecDiv = SecSum div 60,
    MinSum = Minute + SecDiv,
    Minute2 = MinSum rem 60,
    MinDiv = MinSum div 60,
    Hour2 = Hour + MinDiv,
    {{{Year, Month, Day}, {Hour2, Minute2, Second2}}, MicroSec2}.

