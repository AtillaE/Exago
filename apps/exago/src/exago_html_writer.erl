%%%-------------------------------------------------------------------
%%% File    : exago_html_writer.erl
%%% Author  : Bartlomiej Puzon <bartlomiej.puzon@erlang-solutions.com>
%%% Description : Generates HTML reports. An HTML report containts the list
%%% of failing sessions along with the reasons and state machine graphs
%%% for them in a separate webpages
%%%
%%% Created : 18 Jan 2010 by  <bartlomiej.puzon@erlang-solutions.com>

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
%%%-------------------------------------------------------------------
-module(exago_html_writer).

-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).


-record(state, {dest, output, vis_table}).

-define(SUBDIR, "sessions").
-define(IMG_FORMAT, "png").
-define(HTML_HEADER(Title),
	"<HTML>"
	"<HEAD><TITLE>" ++ Title ++ "</TITLE></HEAD>"
	"<BODY>").
-define(HTML_FOOTER, "</BODY></HTML>").
-define(ERROR(Msg), "<B>ERROR:</B>" ++ Msg ++ "<BR>").
-define(WARNING(Msg), "<B>Warning:</B> " ++ Msg ++ "<BR>").
-define(HEADING(Msg), "<H3>" ++ Msg ++ "</H3><BR>\n").

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
-spec init([term()]) -> {ok, #state{}}.
init(Args) ->
    Name = proplists:get_value(destination, Args, "r.html"),
    file:make_dir(?SUBDIR),
    VisTable = ets:new(vis_table, [set]),
    {ok, #state{dest = Name, vis_table = VisTable}}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------

%%Control data - starting, stopping, headings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_event(term(),#state{}) -> {ok, #state{}}.
handle_event({control, {start, Title}}, State) ->
    Name = State#state.dest,
    {ok, File} = file:open(Name, [write]),
    State2 = State#state{output = File},
    write(State2, ?HTML_HEADER(Title), []),
    {ok, State2};

handle_event({control, {stop}}, State) ->
    write(State, ?HTML_FOOTER, []),
    file:close(State#state.output),
    {ok, State#state{output = []}};

handle_event({control, {section, SectionName}}, State) ->
    write(State, ?HEADING(SectionName), []),
    {ok, State};

%%Session data - formats failure reasons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event({session_info, {info, SessionId, Specification,
			     FailDetails, AbsCommands}}, State) ->
    
    {Reason, LastState, LastTransIndex, LastTrans} = FailDetails,
    
    SessionPrefix = term_to_filename(SessionId),
    {CallBackMod, Spec} = case Specification of
			      {CallBackMod_, Spec_} ->
				  {CallBackMod_, Spec_};
			      Spec_ ->
				  {statem, Spec_}
			  end,
    
    {ok, File} =
	file:open(
	  filename:join(?SUBDIR ++ "/",  SessionPrefix ++ ".html"), [write]),
    
    io:format(File, ?HTML_HEADER("Failing session "
				 ++ term_to_filename(SessionId)), []),
    io:format(File, ?HEADING(to_str("Failing session ~p", [SessionId])), []),
    
    
    io:format(File, "<p>" ++ CallBackMod:message(FailDetails) ++"</p>", []),
    io:format(File, "The faulty abstract event: <pre>~p</pre>\n",[LastTrans]),
    io:format(File, "Last state: <pre>~p</pre>\n", [LastState]),
    %%TODO: callback:get_state_name(Spec, State)
    
    io:format(File, ?HEADING(to_str("Abstracted transitions in session"
				    "<pre>~p</pre>\n", [SessionId])), []),    
    lists:foldl(fun(X, Acc) ->
			case Acc == LastTransIndex of
			    true ->
				io:format(File,
					  "<pre> -> ~p <- </pre>\n", [X]);
			    false ->
				io:format(File,
					  "<pre>    ~p    </pre>\n", [X])
			end,
			Acc + 1
		end, 1, AbsCommands),    

    io:format(File,?HEADING("Abstract model"), []),    
    case ets:lookup(State#state.vis_table, LastState) of
	[{LastState, ImgFileName_}] ->
	    ImgFileName = ImgFileName_;
	[] ->
	    ImgPrefix =	term_to_filename(CallBackMod)
		++ "_" ++ term_to_filename(LastState),
	    CallBackMod:visualize(Spec, LastState,
				  filename:join(?SUBDIR,
						ImgPrefix ++ ".graph")),
	    os:cmd("dot "
		   ++ filename:join(?SUBDIR, ImgPrefix) ++ ".graph "
		   ++ "-T" ++ ?IMG_FORMAT ++ " -o"
		   ++ filename:join(?SUBDIR, ImgPrefix) ++ "." ++ ?IMG_FORMAT),
	    
	    ImgFileName = ImgPrefix ++ "." ++ ?IMG_FORMAT,
	    ets:insert(State#state.vis_table, {LastState, ImgFileName})
    end,
    
    io:format(File,"<img src=\"" ++ ImgFileName ++ "\">", []),
    io:format(File, ?HTML_FOOTER, []),
    
    file:close(File),
    
    write(State, "<a href=\"" ++ ?SUBDIR ++ "/" ++ SessionPrefix ++ ".html\">"
	   "~p failed: ~p</a><br>~n", [SessionPrefix, Reason]),
    
    {ok, State};

handle_event({info, {info, Msg}}, State) ->
    write(State, "<i>" ++ Msg ++ "</i><br>\n", []),
    {ok, State};

handle_event({warning, {nondet_map, Id, Pairs}}, State) ->
    write(State, ?WARNING(to_str("Mapping ~p non-deterministic: ~p ~n",
				 [Id, Pairs])), []),
    {ok, State};

handle_event({warning, {undef_map, Id, FromValue}}, State) ->
    write(State, ?WARNING(to_str("Mapping ~p not defined on ~p ~n",
				 [Id, FromValue])), []),
    {ok, State};

handle_event({warning, {parsefun_error, Class, Error}}, State) ->
    write(State, ?ERROR(to_str("Custom parser function threw exception<br>~n "
			       "Class: <pre>~p</pre> ~n "
			       "Exception: <pre>~p<pre> ~n ",
			       [Class, Error])), []),    
    {ok, State};

handle_event({warning, {ts_parse_error, _TsString}}, State) ->
    % do nothing, just let the error propagate
    {ok, State};

handle_event({warning, {abstrfun_error, _Class, _Error}}, State) ->
    % do nothing, just let the error propagate
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
-spec handle_call(term(),#state{}) -> {ok, term(), #state{}}.
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
-spec handle_info(term(),#state{})->{ok,#state{}}.
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
-spec terminate(term(),#state{}) -> ok.
terminate(_Reason, State) ->
    ets:delete(State#state.vis_table),
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
-spec code_change(term(),term(),#state{}) -> {ok,#state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

write(State, Msg, Args) ->
    io:format(State#state.output, Msg, Args).

to_str(Msg, Args) ->
    lists:flatten(io_lib:format(Msg, Args)).

term_to_filename(Term) when is_tuple(Term)->
    Lst = tuple_to_list(Term),
    S = term_to_filename(Lst),
    "session_" ++ S;

term_to_filename(Int) when is_integer(Int) ->
    remove_invalid_chars(integer_to_list(Int));
term_to_filename(Atom) when is_atom(Atom) ->
    remove_invalid_chars(atom_to_list(Atom));

term_to_filename([])->
    "";
term_to_filename([none | Rest]) ->
    "_" ++ "none"
        ++ term_to_filename(Rest);
term_to_filename([Head | Rest])  ->
    "_" ++ remove_invalid_chars(Head)
        ++ term_to_filename(Rest).

remove_invalid_chars(C) when is_integer(C) ->
    case is_valid(C) of
	true ->
            $_;
	false ->
            C
    end;
remove_invalid_chars([]) ->
    [];
remove_invalid_chars([C | Rest]) ->
    [remove_invalid_chars(C) |
     remove_invalid_chars(Rest)].

is_valid(C) ->
    not lists:member(C,[$q,$w,$e,$r,$t,$z,$u,
                        $i,$o,$p,$a,$s,$d,$f,
                        $g,$h,$j,$k,$l,$y,$x,
                        $c,$v,$b,$n,$m,$Q,$W,
                        $E,$R,$T,$Z,$U,$I,$O,
                        $P,$A,$S,$D,$F,$G,$H,
                        $J,$K,$L,$Y,$X,$C,$V,$B,$N,$M,
                        $0,$1,$2,$3,$4,$5,$6,$7,$8,$9]).
