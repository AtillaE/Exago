%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author  Hans Svensson
%%% @doc Examples - configuration and test code
%%%
%%% Created :  7 Apr 2009 by Hans Svensson 

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

-module(examples).
-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').

-export([conf_elevators/0,
         conf_etc_sms/0, conf_etc_sms_timed/0,
         conf_etc_sms_faulty/0, conf_etc_sms_faulty_timed/0]).

%%%
%% Elevator example
%%%
-spec conf_elevators() -> list().
conf_elevators() ->
    [{name, "Elevators"},
     {base_dir, "./"},
     {files, [[{wildcard, "Log.txt"},
	       {delimiter, ","},
	       {ts_format, "yyyy-MM-dd hh:mm:ss:fffffff"},
	       {transaction_id,[2]},
	       {session_id, [4]},
	       {timestamp, [1]},
	       {abstract_value, [3,5,6]}]]},
     {trans_abstr, fun([{Timestamp, AbstrVal} 
			 | _]) ->
			    case AbstrVal of
				{"reset", "closed", _Floor} ->
				    {Timestamp, AbstrVal};
				{"open", none, none} ->
				    {Timestamp, AbstrVal};
				{"close", none, none} ->
				    {Timestamp, AbstrVal};
				{"approaching", _Floor, none} ->
				    {Timestamp, AbstrVal};
				{"stopped_at", _Floor, none} ->
				    {Timestamp, AbstrVal};
				_ -> 
				    none
			    end
		    end},
     {sess_abstr, fun(Trs) ->
			    lists:map(fun({Timestamp,
					   AbstrVal}) ->
					      {Timestamp,
					       case AbstrVal of
						   {"reset", "closed", Floor} ->
						       "reset_to_" ++ Floor;
						   {"open", none, none} ->
						       "open";
						   {"close", none, none} ->
						       "close";
						   {"approaching", Floor, none} ->
						       "approaching_" ++ Floor;
						   {"stopped_at", Floor, none} ->
						       "stopped_at_" ++ Floor;
						   _Other ->
						       _Other
					       end}
				      end, Trs)
			       end},
     {specification , {statem, [{states, [{0,"Init"},
                                          {1,"Closed at floor 1"},
                                          {2,"Open at floor 1"},
                                          {3,"Approaching floor 1"},
                                          {4,"Closed at floor 2"},
                                          {5,"Open at floor 2"},
                                          {6,"Approaching floor 2"},
                                          {7,"Closed at floor 3"},
                                          {8,"Open at floor 3"},
                                          {9,"Approaching floor 3"}]},
                                 {trans, [{0,1,"reset_to_1"},
					 {0,4,"reset_to_2"},
					 {0,7,"reset_to_3"},
					 {1,2,"open"},
					 {2,1,"close", {gt,100}},
					 {1,6,"approaching_to_2"},
					 {6,3,"approaching_to_1"},
					 {6,9,"approaching_to_3"},
					 {6,4,"stopped_at_2"},
					 {4,5,"open"},
					 {5,4,"close", {gt, 100}},
					 {4,3,"approaching_1"},
					 {4,9,"approaching_3"},
					 {9,7,"stopped_at_3"},
					 {7,8,"open"},
					 {7,6,"approaching_2"},
					 {8,7,"close", {gt, 100}},
					 {3,1,"stopped_at_1"}]},
				{terminal, [0,1,2,3,4,5,6,7,8,9]},
				{good, [0,1,2,3,4,5,6,7,8,9]}]}}].


%%%
%% ETC SMS example
%%%
-spec conf_etc_sms_timed() -> list().
conf_etc_sms_timed() ->
    conf_etc_sms("etc_ex_") ++ [{specification, {statem, sms_timed_statem()}}].

-spec conf_etc_sms_faulty_timed() -> list().
conf_etc_sms_faulty_timed() ->
    conf_etc_sms("etc_ex_faulty_") ++ [{specification, {statem, sms_timed_statem()}}].

-spec conf_etc_sms() -> list().
conf_etc_sms() ->
    conf_etc_sms("etc_ex_") ++ [{specification, {statem, sms_statem()}}].

-spec conf_etc_sms_faulty() -> list().
conf_etc_sms_faulty() ->
    conf_etc_sms("etc_ex_faulty_") ++ [{specification, {statem, sms_statem()}}].

conf_etc_sms(FilePre) ->
    [{name, "ETC SMS Example"},
     {ts_format, "'yyyy-MM-dd hh:mm:ss:fffffff'"},
     {files, [[{wildcard, FilePre ++ "Req.log"},
	       {session_id, [3]},
	       {timestamp, [2]},
	       {abstract_value, [filename]}],
	      
	      [{wildcard, FilePre ++ "ReqAck.log"},
	       {session_id, [3]},
	       {timestamp, [2]},
	       {abstract_value, [filename]}],
	      
	      [{wildcard, FilePre ++ "ReqErr.log"},
	       {session_id, [3]},
	       {timestamp, [2]},
	       {abstract_value, [filename]}],
 
	      [{wildcard, FilePre ++ "ReqSMS.log"},
	       {session_id, [3]},
	       {timestamp, [2]},
	       {abstract_value, [filename]},
	       {mappings, [{sid, [4], [3]}]}],
	      
	      [{wildcard, FilePre ++ "AckSMS.log"},   
	       {session_id, {sid, [3]}},
	       {timestamp, [2]},
	       {abstract_value, [filename]}]]},
     
     {sess_abstr, fun(Trs) ->
			  lists:map(fun abstr_sms/1,Trs)
		  end}].

abstr_sms({Ts,{File}}) ->
    L = string:tokens(filename:basename(File),"_."),
    case lists:nth(length(L) - 1, L) of
        "Req" ->
            {Ts,req};
        "ReqSMS" ->
            {Ts,req_sms};
        "AckSMS" ->
            {Ts,ack_sms};
        "ReqAck" ->
            {Ts,req_ack};
        "ReqErr" ->
            {Ts,req_err}
    end.


%%%
%% State machine for SMS example
%%%

sms_statem() ->
    [{states, [{0, "State 0"},
	       {1, "State 1"},
	       {2, "State 2"},
	       {3, "State 3"},
	       {4, "State 4"},
	       {5, "State 5"},
	       {6, "State 6"}]},
     {trans, [{0, 1, req},
	      {1, 2, req_sms},
	      {2, 3, ack_sms},
	      {3, 4, req_ack},
	      {1, 5, req_err},
	      {5, 6, req_ack},
	      {5, 6, ack_sms},
	      {4, 6, req},
	      {4, 6, req_sms}]},
     {terminal, [4,5,6]},
     {good,  [4,5]}]. 

sms_timed_statem() ->
    [{states, [0,1,2,3,4,5,6]},
     {trans, [{0,1,req},
	      {1,2,req_sms, {lt,180}},
	      {2,3,ack_sms},
	      {3,4,req_ack},
	      {1,5,req_err, {geq,180}},
	      {5,6,req_ack},
	      {5,6,ack_sms},
	      {4,6,req},
	      {4,6,req_sms}]},
     {terminal, [4,5,6]},
     {good, [4,5]}].
