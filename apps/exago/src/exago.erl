%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author  Hans Svensson
%%% @doc Audit log monitor (Main module).
%%%
%%% Created : 30 Mar 2009 by Hans Svensson 

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

-module(exago).
-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').
-export([run/1]).

%% @spec run(Conf::list()) -> ok
%% @doc Starts the analysis using the provided configuration record.
-spec run(Conf::list()) -> {ok, tid()} | {nospec, {tid(), tid(), tid()}}
			       | {invalid_conf, term()}.
run(Conf) ->
    io:format("Validating the configuration record..."),
    case (catch exago_conf:validate(Conf)) of
	true ->
	    io:format("success.\n"),
	    exago_events:start_link(),
	    exago_events:add_handler(exago_html_writer,
				     [{destination, "report.html"}]),
	    exago_events:e_start_report(exago_conf:get_name(Conf)),
	    exago_events:e_section(exago_conf:get_name(Conf)),
	    
	    io:format("Sorting log files by relations..."),

	    {TSort, SortedConf} = timer:tc(exago_conf, sort_file_conf, [Conf]),
	    io:format("done. (~p)\n", [TSort]),
	    
	    io:format("Reading and resolving log entries..."),
	    {TRead, {ok, {Tbl, Tbl2}}} =
		timer:tc(exago_reader, read_files, [SortedConf]),
	    io:format("done. (~p)\n", [TRead]),
	    
	    io:format("Structuring transactions..."),
	    TrAbstrFun = exago_conf:get_trans_abstrfun(SortedConf),
	    {TStructTrans, {ok, Tbl2}} = 
		timer:tc(exago_abstr, abstract,
			 [Tbl, Tbl2, TrAbstrFun, session_id]),
	    io:format("done. (~p)\n", [TStructTrans]),
	    
	    io:format("Structuring sessions..."),
	    SessAbstrFun = exago_conf:get_sess_abstrfun(SortedConf),
	    AbstrSessTbl = ets:new(abstr_sess_tbl, [duplicate_bag, public]),
	    {TStructSess, {ok, AbstrSessTbl}} =
		timer:tc(exago_abstr, abstract,
			 [Tbl2, AbstrSessTbl, SessAbstrFun]),
	    io:format("done. (~p)\n", [TStructSess]),
	    
	    case exago_conf:get_spec(SortedConf) of
		undefined ->
		    exago_events:e_stop_report(),
		    io:format("No abstract model of the system present.\n"),
		    {no_spec, {Tbl, Tbl2, AbstrSessTbl}};
		Specification ->		    
		    io:format("Checking against the abstract model..."),
		    {TCheck, ResultTbl} = case Specification of
					      {CallBackMod, Spec} ->
						  timer:tc(CallBackMod, check,
 							   [AbstrSessTbl,Spec]);
					      Spec ->
						  timer:tc(statem, check,
							   [AbstrSessTbl,Spec])
					  end,
		    io:format("done. (~p)\n", [TCheck]),
 		    
		    exago_events:e_section("Failing sessions:"),
		    io:format("Generating session reports and cleaning up..."),
		    gen_sess_reports(ResultTbl, AbstrSessTbl, Spec),
		    ets:delete(Tbl),
		    ets:delete(Tbl2),
		    ets:delete(AbstrSessTbl),
		    exago_events:e_stop_report(),
		    io:format("done.\n"),

		    {ok, ResultTbl}
	    end;
	{false, Details} ->
	    io:format("fail!\n"),
	    {invalid_conf, Details};
	Error ->
	    io:format("fail!\n"),
	    {invalid_conf, Error}
    end.

gen_sess_reports(ResultTbl, SessionTbl, Spec) ->
    exago_utils:ets_map(fun({SessionId, SessionRes}) ->
				report_fun({SessionId, SessionRes},
					   SessionTbl, Spec)
			end, ResultTbl).

report_fun({SessionId, SessionRes}, SessionTbl, Spec) ->
    case SessionRes of
        {passed, _N} ->
            ok;
        {failed, Reason} ->
            {[SessionId | _], AbstrEvents} =
		lists:unzip(ets:lookup(SessionTbl, SessionId)),
	    {AbstrEvents2, _}  = lists:unzip(AbstrEvents),
	    exago_events:e_sm_info(SessionId, Spec, Reason, AbstrEvents2)
    end.
