%%%-------------------------------------------------------------------
%%% @author  Bartlomiej Puzon <bartlomiej.puzon@erlang-solutions.com>
%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author  Hans Svensson
%%% @doc An event broker.
%%% In order to generate reports, the event_broker must be started.
%%% Any event handler subscribed will receive events (see code for format)
%%% Event functions return the respective event counter.
%%% Analyzer should send session failure details and session information
%%% within the proper e_start_session and e_start_session events.
%%%
%%% Created : 18 Jan 2010 by Bartlomiej Puzon

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

-module(exago_events).

-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').

-behaviour(gen_event).

-export([start_link/0,
	 add_handler/2,
	 e_nondet_map/2,
	 e_undef_map/2,
	 e_parsefun_error/2,
	 e_ts_parse_error/1,
	 e_abstrfun_error/2,
	 e_info/1,
	 e_section/1,
	 e_sm_info/4,
	 e_start_report/1,
	 e_start_session/1,
	 e_stop_report/0,
	 e_stop_session/0]).

-define(SERVER, exago_event_manager).


%%--------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | {error,{already_started,pid()}}.
start_link() ->
    exago_event_counter:start_link(),
    gen_event:start_link({local, ?SERVER}).

%% @spec add_handler(Module::atom(),[Args::term()]) -> ok|{'EXIT',Reason}|term()
%% @doc Adds an event handler
-spec add_handler(atom(), [term()]) -> ok | {'EXIT',term()} | term().
add_handler(Module, Args) ->
    gen_event:add_handler(?SERVER, Module, Args).

%% API

%% @doc the beginning of the report
-spec e_start_report(term()) -> term().
e_start_report(Title) ->
    send({control, {start, Title}}).

%% @doc the end of the report
-spec e_stop_report() -> term().
e_stop_report() ->
    gen_event:sync_notify(?SERVER, {control, {stop}}).

%% @doc the end of the report
-spec e_start_session(term()) -> term().
e_start_session(Id) ->
    send({control, {failing_session_start, Id}}).

%% @doc the end of a session
-spec e_stop_session() -> term().
e_stop_session() ->
    send({control, {failing_session_stop}}).

%% @doc beginning of a new report section
-spec e_section(term()) -> term().
e_section(Name) ->
    send({control, {section, Name}}).

%% @doc information about the current session
-spec e_sm_info(term(),term(),term(),term()) -> term().
e_sm_info(SessionId, AbsEvents, Reason, Specification) ->
    send({session_info, {info, SessionId, AbsEvents,
			 Reason, Specification}}).

%% @doc non deterministic mapping
-spec e_nondet_map(atom(), term()) -> term().
e_nondet_map(Id, Pairs) ->
    send({warning, {nondet_map, Id, Pairs}}).

%% @doc undefined mapping
-spec e_undef_map(atom(), term()) -> term().
e_undef_map(Id, FromValue) ->
    send({warning, {undef_map, Id, FromValue}}).

%% @doc custom parser function threw exception
-spec e_parsefun_error(atom(), term()) -> term().
e_parsefun_error(Class, Error) ->
    send({warning, {parsefun_error, Class, Error}}).

%% @doc timstamp parse error
-spec e_ts_parse_error(string()) -> term().
e_ts_parse_error(TsString) ->
    send({warning, {ts_parse_error, TsString}}).

%% @doc abstraction function threw exception
-spec e_abstrfun_error(atom(), term()) -> term().
e_abstrfun_error(Class, Error) ->
    send({warning, {abstrfun_error, Class, Error}}).

%% @doc information message
-spec e_info(term()) -> term().
e_info(Msg) ->
    send({info, {info, Msg}}).

%% @private
send(Event) ->
    gen_event:notify(?SERVER, Event),
    {_, Ev} = Event,
    exago_event_counter:count(element(1, Ev)).
