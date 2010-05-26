%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author  Hans Svensson
%%% @doc Comparing and defining state machines 
%%%
%%% Created : 7 Apr 2009 by Hans Svensson 
 
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
 
-module(statem). 

-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').
 
-export([get_state_name/2]).
-export([message/1, validate/1, check/2, visualize/3]). 
 
%% @spec get_state_name(Statem::list(), N::integer()) -> string() 
%% @doc Returns the name of the given state 
-spec get_state_name(list(), integer()) -> string().
get_state_name(Statem, N) ->
    States = proplists:get_value(states, Statem),
    case lists:member(N, States) of 
	true -> 
	    integer_to_list(N);
	false ->
	    case lists:keyfind(N, 1, States) of 
		false -> 
		    invalid_state; 
		State -> 
		    case length(tuple_to_list(State)) > 1 of 
			true -> 
			    element(2,State); 
			false -> 
			    integer_to_list(element(1,State)) 
		    end 
	    end
    end.
 
%%% 
%%% API for checking against state mahines 
%%%

message({nomatch, LastState, _LastTransIndex, LastTrans}) ->    
    io_lib:format("No matching transition "
		  "for <i>~p</i> in state ~p",
		  [LastTrans, LastState]);
message({nomatch_inlimit, LastState, _LastTransIndex, LastTrans}) ->
    io_lib:format("No matching transition within time constraint "
		  "for <i>~p</i> in state ~p",
		  [LastTrans, LastState]);
message({nondet, LastState, _LastTransIndex, LastTrans}) ->
    io_lib:format( "Non deterministic transition for <i>~p</i> in state ~p",
		  [LastTrans, LastState]);

message({nonterm, LastState, _LastTransIndex, _LastTrans}) ->
    io_lib:format("Session ends in a non terminal state (~p)", [LastState]);
    
message({badterm, LastState, _LastTransIndex, _LastTrans}) ->
    io_lib:format("Session ends in a bad terminal state (~p)",
		  [LastState]).

%% @doc Checks whether the given specification record is valid
%% @spec validate(Spec::list()) -> true | false
-spec validate(list()) -> true | false.
validate(Spec) when is_list(Spec) ->
    true;
validate(_Other) ->
    false.
 
%% @spec check(DateEts::tid(), Statem::list()) -> tid() 
%% @doc Check the abstract sessions stored in DataEts against a state machine 
-spec check(tid(), list()) -> tid(). 
check(DataEts,Statem) -> 
    ResultEts =  
        exago_utils:ets_parmap_to_ets(fun(Sess) -> 
					      R = check_session(Sess,Statem),
					      R
				      end, DataEts),
    ResultEts.
 
%% @doc Checks an abstract session againist a state machine 
check_session(Trans, Spec) ->
    {[SessionId | _], Trans2} = lists:unzip(Trans),
    {Trans3, _} = lists:unzip(Trans2),
    InitSt = proplists:get_value(init_st, Spec, 0),
    [StTrans, Terminal, Good] =
	[proplists:get_value(Key, Spec) || Key <- [trans, terminal, good]],
    case check_session(Trans3, StTrans, InitSt, undefined, 1) of 
        {passed,N,I} -> 
            case lists:member(N, Terminal) of 
                false ->                     
                    {SessionId, {failed, {nonterm, N, I, none}}}; 
                true -> 
                    case lists:member(N,Good) of 
                        true -> 
                            {SessionId,{passed,N}}; 
                        false -> 
                            {SessionId,{failed,{badterm,N,I,none}}} 
                    end 
            end; 
	Other -> 
	    {SessionId, Other} 
    end. 
 
check_session([],_,N,_,I) ->
    {passed,N,I};
check_session([{Ts,Tr} | Trans],StTrans,N,Ts0,I) ->
    NextStatesWithLimit =
        [ Y || {X,Y,Tr2,_Limit} <- StTrans, 
               abstrcmd_eq(Tr,Tr2),
               X == N],

    NextStatesInLimit =  
        [ Y || {X,Y,Tr2,Limit} <- StTrans, 
	       abstrcmd_eq(Tr,Tr2),
               X == N, 
               in_limit(Limit,Ts0,Ts)],

    NextStatesWithoutLimit = 
        [ Y || {X,Y,Tr2} <- StTrans,
	       abstrcmd_eq(Tr,Tr2), 
               X == N],

    case NextStatesWithoutLimit ++ NextStatesInLimit of 
        [] -> 
	    case NextStatesWithLimit == [] of 
		true -> 
		    {failed,{nomatch,N,I,Tr}}; 
		false -> 
 		    {failed,{nomatch_inlimit,N,I,Tr}} 
	    end; 
        [N2] -> 
	    check_session(Trans, StTrans, N2, Ts, I+1); 
	_Ns ->
            %% Non deterministic state machine 
            {failed,{nondet,N,I,Tr}} 
    end. 
 

abstrcmd_eq(Command,Pattern) ->
    if
        Pattern == '_' ->
            true;
        is_tuple(Command) and is_tuple(Pattern) ->
            CList = tuple_to_list(Command),
            PList = tuple_to_list(Pattern),
            case length(CList) == length(PList) of
		true ->
		    CPList = lists:zip(CList, PList),
		    lists:foldl(fun({C, P}, Result) ->
					Result and abstrcmd_eq(C, P)
				end, true, CPList);
		false -> false
            end;
        true ->
            Command == Pattern
    end.

%% @doc Check a time limit,  
in_limit(undefined,_,_) -> 
    true; 
in_limit({lt,X},Ts0,Ts1) -> 
    D = exago_utils:ts_diff(Ts0,Ts1), 
    D < X * 100000; 
in_limit({gt,X},Ts0,Ts1) -> 
    D = exago_utils:ts_diff(Ts0,Ts1), 
    D > X * 100000; 
in_limit({leq,X},Ts0,Ts1) -> 
    D = exago_utils:ts_diff(Ts0,Ts1),
    D =< X * 100000; 
in_limit({geq,X},Ts0,Ts1) -> 
    D = exago_utils:ts_diff(Ts0,Ts1),
    D >= X * 100000; 
in_limit(_,_,_) -> 
    true.

%%% 
%%% API for visualizing state machines 
%%% 
 
%% @spec visualize(Statem::list(), EndState::integer(), File::string()) -> ok 
%% @doc Visualize state machine
-spec visualize(Statem::list(), EndState::integer(), File::string()) -> ok.
visualize(Statem, EndState, File) ->
    States = proplists:get_value(states, Statem),
    Trans = proplists:get_value(trans, Statem),
    {ok, Io} = file:open(File,[write]),
    io:fwrite(Io,"digraph G {\n#size=\"10,10\"\n",[]),
    [writeState(Io,Statem,EndState,S) || S <- States],
    [writeTrans(Io,T) || T <- Trans],
    io:fwrite(Io,"\n}\n",[]),
    file:close(Io). 
 
writeState(Io,Statem,EndState,{S,Str}) ->
    InitSt = proplists:get_value(init_st, Statem, 0),
    [Terminal, Good] =
	[proplists:get_value(Key, Statem) || Key <- [terminal, good]],
    
    IsInit = InitSt == S,
    IsTerm = lists:member(S, Terminal),
    IsGoodTerm = IsTerm andalso lists:member(S, Good),
    IsBadTerm = IsTerm andalso not IsGoodTerm,
    FillColor = if S == EndState -> "lightgray";
               true -> "white" 
            end, 
    Shape = if  IsInit -> "ellipse"; 
                IsGoodTerm -> "doublecircle"; 
                IsBadTerm -> "Mcircle"; 
                true -> "circle"  
            end, 
    io:fwrite(Io, "st~p [shape=~s,fillcolor=~s,style=filled,fontsize=10,"
	      "label=\"~s\"];\n", [S, Shape, FillColor, Str]); 
writeState(Io,Statem,EndState,S) -> 
    writeState(Io, Statem, EndState, {S, "state"++integer_to_list(S)}). 
 
writeTrans(Io, {N, M, Tr}) -> 
    io:fwrite(Io, "st~p -> st~p [fontsize=10,label=\"~s\"];\n",
	      [N, M, print_tuple(Tr)]); 
writeTrans(Io, {N, M, Tr, _}) -> 
    io:fwrite(Io, "st~p -> st~p [fontsize=10,label=\"~s\"];\n",
	      [N, M, print_tuple(Tr)]).

print_tuple(T) when is_integer(T) ->
    integer_to_list(T);
print_tuple(T) when is_list(T) -> 
    "\\\"" ++ T ++ "\\\"";
print_tuple(T) when is_atom(T) -> 
    atom_to_list(T);
print_tuple(T) when is_tuple(T) -> 
    [E | L] = tuple_to_list(T),
    "{" ++
	lists:foldl(fun(TupEl, Acc) -> 
			    Acc ++ ", " ++ print_tuple(TupEl)
		       end, print_tuple(E), L)
	++ "}".
 
