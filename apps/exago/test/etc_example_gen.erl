%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author  Hans Svensson
%%% Created : 26 Feb 2009 by Hans Svensson <>

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
 
 
-module(etc_example_gen). 
-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').
 
-export([gen/1, gen_faulty/1]). 
 
-define(TIMEOUT_PROB,0.1). 
-define(FAULT_PROB,0.05). 
-define(TIMEOUT,180). % Seconds!! 
 
%% @doc Generates example logs 
gen(N) -> 
    gen(N,"etc_ex"). 
 
%% @doc Generates faulty example logs 
gen_faulty(N) -> 
    gen_faulty(N,"etc_ex_faulty"). 
 
gen(N,FileBase) -> 
    gen(N,FileBase,false). 
 
gen_faulty(N,FileBase) -> 
    gen(N,FileBase,true). 
 
gen(N,FileBase,WithFaults) -> 
    Logs = [ L || {ok,L} <- [file:open( 
                               lists:concat([FileBase, "_", Ln, ".log"]), 
                               [write])  
                             || Ln <- ["Req", 
                                       "ReqAck", 
                                       "ReqErr", 
                                       "ReqSMS", 
                                       "AckSMS"]]], 
    {R1,R2,R3} = now(), 
    random:seed(R1,R2,R3), 
    gen(N,Logs,WithFaults,1,1), 
    [file:close(L) || L <- Logs], 
    ok. 
 
gen(0,_,_,_,_) -> 
    ok; 
gen(N,Logs,false,RId,SId) -> 
    {RId2,SId2} = gen_session(Logs,RId,SId), 
    gen(N-1,Logs,false,RId2,SId2); 
gen(N,Logs,true,RId,SId) -> 
    F = random:uniform(), 
    case F < ?FAULT_PROB of 
        true ->  
            {RId2,SId2} = gen_faulty_session(Logs,RId,SId); 
        false -> 
            {RId2,SId2} = gen_session(Logs,RId,SId) 
    end, 
    gen(N-1,Logs,true,RId2,SId2). 
 
gen_session(Ls = [L1,_L2,_L3,_L4,_L5],RId,SId) -> 
    T = random:uniform(), 
    TS = gen_TS(), 
    write_log(L1,[req,format_TS(TS),RId]), 
    case T < ?TIMEOUT_PROB of 
        true -> 
            gen_timeout_session(Ls,TS,RId,SId); 
        false -> 
            gen_ok_session(Ls,TS,RId,SId) 
    end.

gen_faulty_session(Ls = [L1,L2,L3,L4,_L5],RId,SId) -> 
    TS = gen_TS(), 
    write_log(L1,[req,format_TS(TS),RId]), 
    case random:uniform(7) of 
        1 -> 
            gen_timeout_session(Ls,TS,RId,SId), 
            TS2 = gen_TS(no_timeout,TS), 
            write_log(L2,[req_ack,format_TS(TS2),RId,ack]), 
            {RId+1,SId}; 
        2 -> 
            gen_ok_session(Ls,TS,RId,SId), 
            TS2 = gen_TS(timeout,TS), 
            write_log(L3,[req_error,format_TS(TS2),RId,timeout]), 
            {RId+1,SId+1}; 
        3 -> 
            gen_ok_session(Ls,TS,RId,SId), 
            TS2 = gen_TS(no_timeout,TS), 
            write_log(L2,[req_ack,format_TS(TS2),RId,ack]), 
            {RId+1,SId+1}; 
        4 -> 
            gen_ok_session(Ls,TS,RId,SId), 
            TS2 = gen_TS(no_timeout,TS), 
            write_log(L4,[reqSMS,format_TS(TS2),RId,SId+1,random:uniform(255)]), 
            {RId+1,SId+2}; 
        5 -> 
            gen_ok_session(Ls,TS,RId,SId), 
            TS2 = gen_TS(no_timeout,TS), 
            write_log(L4,[reqSMS,format_TS(TS2),RId+1,SId,random:uniform(255)]), 
            {RId+2,SId+1}; 
        6 -> 
            gen_ok_session(Ls,TS,RId,SId), 
            TS2 = gen_TS(), 
            write_log(L1,[req,format_TS(TS2),RId]), 
            {RId+1,SId+1}; 
        7 -> 
            gen_timeout_session(Ls,TS,RId,SId), 
            TS2 = gen_TS(), 
            write_log(L1,[req,format_TS(TS2),RId]), 
            {RId+1,SId} 
    end.		 
 
gen_timeout_session([_L1,_L2,L3,_L4,_L5],TS,RId,SId) -> 
    TS2 = gen_TS(timeout,TS), 
    write_log(L3,[req_error,format_TS(TS2),RId,timeout]), 
    {RId+1,SId}. 
 
gen_ok_session([_L1,L2,_L3,L4,L5],TS,RId,SId) -> 
    TS2 = gen_TS(no_timeout,TS),
    TS3 = gen_TS(just_after,TS2), 
    TS4 = gen_TS(just_after,TS3), 
    write_log(L4,[reqSMS,format_TS(TS2),RId,SId,random:uniform(255)]), 
    write_log(L5,[ackSMS,format_TS(TS3),SId,ack]), 
    write_log(L2,[req_ack,format_TS(TS4),RId,ack]), 
    {RId+1,SId+1}. 
 
write_log(File,Args) -> 
    FStr = "~w" ++ lists:concat([",~w" || _ <- tl(Args)]) ++ "\n", 
    io:format(File,FStr,Args). 
 
format_TS({MegaSec, Sec, MicroSec}) ->
    {{Year,Month,Day}, {Hour,Min,Seconds}} = 
	calendar:now_to_universal_time({MegaSec, Sec, MicroSec}),
    list_to_atom(
      lists:flatten(
	io_lib:fwrite("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B:~7..0B",
		      [Year, Month, Day, Hour, Min, Seconds, MicroSec]))).

gen_TS() -> 
    TS = case get(ts_seed) of 
             undefined -> 
                 {1235,660000,0}; 
             Val -> 
                 Val 
         end, 
    put(ts_seed,gen_TS(just_after,TS)), 
    TS. 
 
gen_TS(timeout,{MeS,S,MiS}) -> 
    Offs = random:uniform(100), 
    MiOffs = random:uniform(999999), 
    {MeS,S+?TIMEOUT+Offs,(MiS + MiOffs) rem 1000000}; 
gen_TS(no_timeout,{MeS,S,MiS}) -> 
    Offs = random:uniform(10), 
    MiOffs = random:uniform(999999), 
    {MeS,S+Offs,(MiS + MiOffs) rem 1000000}; 
gen_TS(just_before,{MeS,S,MiS}) -> 
    MiOffs = random:uniform(500000), 
    case MiS < MiOffs of 
        true -> 
            {MeS,S-1,1000000 + (MiS - MiOffs)}; 
        false -> 
            {MeS,S,(MiS - MiOffs)} 
    end; 
gen_TS(just_after,{MeS,S,MiS}) -> 
    MiOffs = random:uniform(500000), 
    case MiS + MiOffs > 1000000 of 
        true -> 
            {MeS,S+1,(MiS + MiOffs) rem 1000000}; 
        false -> 
            {MeS,S,(MiS + MiOffs) rem 1000000} 
    end. 
    
