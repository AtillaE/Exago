%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author  Hans Svensson

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

-module(exago_utils).
-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').

-export([ets_parmap_to_ets/2,
	 ets_parmap_to_ets/3,
	 ets_map_to_ets/2,
         ets_map/2,
	 convert_datetime/1, add_datetime/2, ts_diff/2]).

%% @spec ets_map_to_ets(Fun::function(),Tbl::tid()) -> Tbl2::tid()
%% @doc Map function on an ets table, stores the results in another ets table.
-spec ets_map_to_ets(Fun::function(),Tbl::tid()) -> Tbl2::tid().
ets_map_to_ets(Fun,Tbl) ->
    Tbl2 = ets:new(mapped_table,[duplicate_bag]),
    ets_map_to_ets2(Tbl,Fun,ets:first(Tbl),Tbl2).

ets_map_to_ets2(_,_,'$end_of_table',Tbl2) ->
    Tbl2;
ets_map_to_ets2(Tbl,Fun,Key,Tbl2) ->
    [Sess] = ets:lookup(Tbl,Key),
    ets:insert(Tbl2, Fun(Sess)),
    ets_map_to_ets2(Tbl,Fun,ets:next(Tbl,Key),Tbl2).

%% @spec ets_map(Fun::function(), Tbl::tid()) -> list()
%% @doc Map function on an ets table, returns the results as a list.
-spec ets_map(Fun::function(), Tbl::tid()) -> list().
ets_map(Fun,Tbl) ->
    ets_map2(Tbl,Fun,ets:first(Tbl)).

ets_map2(_,_,'$end_of_table') ->
    [];
ets_map2(Tbl,Fun,Key) ->
    [Sess] = ets:lookup(Tbl,Key),
    [Fun(Sess) | ets_map2(Tbl, Fun, ets:next(Tbl,Key))].

%% parrallel map
-spec ets_parmap_to_ets(Fun::function(),Tbl::tid()) -> ok.
ets_parmap_to_ets(Fun, Tbl) ->
    Tbl2 = ets:new(mapped_table, [duplicate_bag, public]),
    Pid = self(),
    ets_parmap_to_ets2(Tbl, Fun, ets:first(Tbl), Tbl2, Pid, 0).

-spec ets_parmap_to_ets(Fun::function(), Tbl::tid(), Tbl2::tid()) -> ok.
ets_parmap_to_ets(Fun, Tbl, Tbl2) ->
    Pid = self(),
    ets_parmap_to_ets2(Tbl, Fun, ets:first(Tbl), Tbl2, Pid, 0).

ets_parmap_to_ets2(Tbl,_,'$end_of_table',Tbl2,_Pid,I) ->
    ets_parmap_wait(I,Tbl,Tbl2);
ets_parmap_to_ets2(Tbl,Fun,Key,Tbl2,Pid,I) ->
    _NewPid = spawn_link(fun() ->
                           Datas = ets:lookup(Tbl,Key),
			   case Fun(Datas) of
			       none ->
				   ok;
			       Value ->
				   ets:insert(Tbl2, Value)
			   end,
			   Pid ! {map_done, Tbl, Tbl2}
                   end),
    ets_parmap_to_ets2(Tbl,Fun,ets:next(Tbl,Key),Tbl2,Pid,I+1).


ets_parmap_wait(0,_Tbl,Tbl2) ->
    Tbl2;
ets_parmap_wait(I,Tbl,Tbl2) ->
    receive
        {map_done,Tbl,Tbl2} -> 
            ets_parmap_wait(I-1,Tbl,Tbl2)
    end.

-spec ts_diff({{tuple(),tuple()},integer()} | {list()} | term(),
	      {{tuple(),tuple()},integer()} | {list()} | term()) -> integer().
ts_diff({TsString1}, {TsString2})
  when is_list(TsString1) and is_list(TsString2) ->
    if
	TsString1 == TsString2 ->
	    0;
	TsString2 < TsString1 ->
	    1;
	true ->
	    -1
    end;
ts_diff({DateTime1, MicroSec1}, {DateTime2, MicroSec2})
  when is_tuple(DateTime1) and is_integer(MicroSec1) and
       is_tuple(DateTime2) and is_integer(MicroSec2) ->
    (calendar:datetime_to_gregorian_seconds(DateTime2) -
     calendar:datetime_to_gregorian_seconds(DateTime1)) * 1000000
	+ (MicroSec2 - MicroSec1);
ts_diff(_,_) ->
    0.


% internally time values (timestamps, offsets) are represented using the format
% {{{Year, Month, Day}, {Hour, Min, Sec}}, MicroSec}
convert_datetime({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    Sec2 = trunc(Sec),
    MicroSec = (Sec - Sec2) * 1000000,
    {{{Year, Month, Day}, {Hour, Min, Sec2}}, MicroSec};
convert_datetime({{{Year, Month, Day}, {Hour, Min, Sec}}, MicroSec}) ->
    {{{Year, Month, Day}, {Hour, Min, Sec}}, MicroSec}.


%% TODO: refactor code duplication with add_offset
%% TODO: should handle day and month 'overflows'
add_datetime({{{Year1, Month1, Day1}, {Hour1, Min1, Sec1}}, MicroSec1},
	 {{{Year2, Month2, Day2}, {Hour2, Min2, Sec2}}, MicroSec2}) ->
    MSSum = MicroSec1 + MicroSec2,
    MicroSec = MSSum rem 1000000,
    MSDiv = MSSum div 1000000,
    SecSum = Sec1 + Sec2 + MSDiv,
    Sec = SecSum rem 60,
    SecDiv = SecSum div 60,
    MinSum = Min1 + Min2 + SecDiv,
    Min = MinSum rem 60,
    MinDiv = MinSum div 60,
    HourSum = Hour1 + Hour2 + MinDiv,
    Hour = HourSum rem 60,
    HourDiv = HourSum div 60,
    Day = Day1 + Day2 + HourDiv,
    Month = Month1 + Month2,
    Year = Year1 + Year2,
    {{{Year, Month, Day}, {Hour, Min, Sec}}, MicroSec}.

