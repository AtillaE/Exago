%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author  Hans Svensson
%%% @doc Abstraction
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

-module(exago_abstr).
-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').

-export([abstract/3, abstract/4]).

%% @spec abstract(Tbl::tid(), Tbl2::rid(),
%%                AbstrFun::function()) -> tid()
%% @doc Groups, sorts and abstracts the events in the source table
%%      using the provided abstraction function and inserts it
%%      with the original key into the destination table
-spec abstract(Tbl::tid(), Tbl2::tid(),
	       AbstrFun::function()) -> tid().
abstract(_Tbl, Tbl2, undefined) ->
    {ok, Tbl2};
abstract(Tbl, Tbl2, AbstrFun) ->
    abstract(Tbl, Tbl2, AbstrFun, current).

%% @spec abstract(Tbl::tid(), Tbl2::rid(),
%%                AbstrFun::function(), NewKeyPos::atom()) -> tid()
%% @doc Groups, sorts and abstracts the events in the source table
%%      using the provided abstraction function and inserts it
%%      with the specified key into the destination table
-spec abstract(Tbl::tid(), Tbl2::tid(),
	       AbstrFun::function(),
	       NewKeyPos::atom()) -> tid().
abstract(_Tbl, Tbl2, undefined, _NewKeyPos) ->
    {ok, Tbl2};
abstract(Tbl, Tbl2, AbstrFun, NewKeyPos) ->
%% NewKeyPos describes by which field we are going to aggregate
%% in the next step. Currently we support only 2 abstraction levels
%% (sessions and transactions), so the possible valid values
%% for NewKeyPos are sess_id, trans_id.
    exago_utils:ets_parmap_to_ets(
      fun(EventsWithKey) ->
	      {[Key | _], EventsWithSessId} = lists:unzip(EventsWithKey),
	      {Events, SessIds} = lists:unzip(EventsWithSessId),
              %% WARNING: no checking for required subset/superset relations!
	      case Key of
		  abstrfun_error ->
		      % propagate errors
		      lists:map(fun({Event, SessId}) ->
					NewKey = case NewKeyPos of
						     current -> Key;
						     session_id -> SessId
						 end,
					{NewKey, {Event, SessId}}
				end, EventsWithSessId); %!
		  _ ->
		      SortedEvents = sort_by_time(Events),
		      case do_abstr(SortedEvents, AbstrFun) of
			  none ->
			      none;
			  AbstrEventList when is_list(AbstrEventList) ->
			      NewKey = case NewKeyPos of
					   current -> Key;
					   session_id -> hd(SessIds)
				       end,
			      lists:map(fun(none) ->
						none;
					   (AbstrEvent) ->
						{NewKey,
						 {AbstrEvent, hd(SessIds)}}
					end, AbstrEventList);
			  AbstrEvent ->
			      NewKey = case NewKeyPos of
					   current -> Key;
					   session_id -> hd(SessIds)
				       end,
			      {NewKey, {AbstrEvent, hd(SessIds)}}
		      end
	      end
      end, Tbl, Tbl2),
    {ok, Tbl2}.

%% @spec do_abstr(Events::list(tuple()),
%%                AbstrFun::function()) ->
%%           Result | {abstrfun_error, {Error, Events}}
%% @doc Calls the abstraction function 
do_abstr(Events, AbstrFun) ->
    AbstrEvent = try AbstrFun(Events) of
		     Result ->
			 Result
		 catch
		     Class:Error ->
			 exago_events:e_abstrfun_error(Class, Error),
			 {abstrfun_error, {Error, Events}}
		 end,
    AbstrEvent.

%% @spec sort_by_time(Trs::list(tuple())) -> SortedTrs::list(tuple())
%% @doc Sort transactions by time
-spec sort_by_time(list(tuple())) -> list().
sort_by_time(Trs) ->
    lists:sort(fun cmp_time/2, Trs).

%% @spec cmp_time(Timestamp1::tuple(), Timestamp2::tuple()) -> true | false
%% @doc Compares timestamps
-spec cmp_time(tuple(string()), tuple(string())) -> true | false.
cmp_time(Trs1, Trs2) ->
    exago_utils:ts_diff(element(1, Trs1), element(1, Trs2)) > 0.
