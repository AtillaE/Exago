%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-solutions.com>
%%% @author  Atilla Erdodi <atilla@erlang-solutions.com>
%%% @author  Hans Svensson
%%% @doc Defining and handling Audit log configurations
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


-module(exago_conf).
-author('<protest@erlang-solutions.com>').
-copyright('Erlang Solutions Ltd.').
 
-export([validate/1, sort_file_conf/1,
         get_name/1, get_files/1,
         get_trans_abstrfun/1,
         get_sess_abstrfun/1,
         get_spec/1]).

-include("exago_conf.hrl").

%% @doc Checks whether the given configuration record is valid
%% @spec validate(Conf::proplist()) -> true | {false, list()}
%% @todo add more conditions and include the reason for failure in the report
-spec validate(list()) -> true | false.
validate(Conf) when is_list(Conf) ->
    ValidKeys = [name, base_dir, ts_format, files,
		 trans_abstr, sess_abstr, specification],
    PropValidKeys = lists:foldl(fun(Key, Res) ->
					Res andalso lists:member(Key, ValidKeys)
				end, true, proplists:get_keys(Conf)),
    
    PropFileListPresent = proplists:is_defined(files, Conf) andalso
	is_list(proplists:get_value(files, Conf)),
    % file list is mandatory

    PropFileListValid =
	lists:foldl(fun(FileSpec, true) ->                         
			    case validate_filespec(FileSpec) of
				true ->
				    true;
				{false, Details} ->
				    {false, [{FileSpec, Details}]}
			    end;
		       (FileSpec, {false, FailedFileSpecList}) ->
			    case validate_filespec(FileSpec) of
				true ->
				    {false, FailedFileSpecList};
				{false, Details} ->
				    {false,
				     [{FileSpec, Details}
				      | FailedFileSpecList]}
			    end
		    end,
		    true,
		    proplists:get_value(files, Conf)),
    
    PropSessAbstrPresent = proplists:is_defined(sess_abstr, Conf) andalso
	is_function(proplists:get_value(sess_abstr, Conf)),
    % session abstraction is mandatory

    Spec = proplists:get_value(specification, Conf),
    {SpecAtom, SpecDetails} = case Spec of
				  undefined ->
				      {undefined, undefined};
				  {Atom, Details} when is_atom(Atom) ->
				      {Atom, Details};
				  _ ->
				      {undefined, undefined}
			      end,
    PropSpecValid = SpecAtom == undefined
	orelse SpecAtom:validate(SpecDetails),
    
    Properties = [{valid_keys, PropValidKeys},
		  {filelist_present, PropFileListPresent},
		  {filelist_valid, PropFileListValid},
		  {sess_abstr_present, PropSessAbstrPresent},
		  {spec_valid, PropSpecValid}],
    case lists:foldl(fun({Prop, Res}, Acc) ->
			     case Res of
				 false -> [Prop | Acc];
				 {false, Details_} -> 
                                     [{Prop, Details_} | Acc];
				 _ -> Acc
			     end
		     end, [], lists:reverse(Properties)) of
	[] ->
	    true;
	FailedProps ->
	    {false, FailedProps}
    end.

validate_filespec(FileSpec) ->
    ValidKeys = [wildcard, parse_fun, parse_opts, delimiter,
		 ts_format, filter, mappings, offset,
		 transaction_id, session_id, timestamp, abstract_value],
    PropValidKeys = lists:foldl(fun(Key, Res) ->
					Res andalso lists:member(Key, ValidKeys)
				end, true, proplists:get_keys(FileSpec)),
    FilterCond = proplists:get_value(filter, FileSpec),
    PropFilterCondValid =
	case  FilterCond of
	    undefined ->
		true;
	    _ ->
		syntax_error /= exago_reader:eval_cond(FilterCond, {})
	end,
    
    Properties = [{valid_keys, PropValidKeys},
		  {filtercond_valid, PropFilterCondValid}],

    case lists:foldl(fun({Prop, Bool}, Acc) ->
			     case Bool of
				 false -> [Prop | Acc];
				 {false, _} -> [Prop | Acc];
				 _ -> Acc
			     end
		     end, [], lists:reverse(Properties)) of
	[] ->
	    true;
	FailedProps ->
	    {false, FailedProps}
    end.

%% @spec sort_file_conf(Conf::list()) -> list()
%% @doc Topological sorting of the audit file configurations
-spec sort_file_conf(Conf::list()) -> list().
sort_file_conf(Conf) ->
    Files = proplists:get_value(files, Conf),
    SortedFiles = sort_fileconf(Files),    
    [{files, SortedFiles} | proplists:delete(files, Conf)].

%% @spec sort_fileconf(FileConfList::list(list())) -> list(list())
%% @doc Topologically sorts the file configurations graph,
%%      where the nodes are file specifications and the
%%      the edges are the relations between them defined by the mappings.
-spec sort_fileconf(list(list())) -> list(list()).
sort_fileconf(FileConfList) ->
    TaggedList = tag_fileconfs(FileConfList),
    NoInbound = lists:filter(fun({_FileConf, Inbound, _Outbound}) ->
                                     Inbound =:= []
                             end, TaggedList),
    Sorted = sort_fileconf2(substract_lists(TaggedList, NoInbound), NoInbound),
    lists:map(fun({FileConf, _Inbound, _Outbound}) -> FileConf end, Sorted).

sort_fileconf2(_TaggedList, []) ->
    [];
sort_fileconf2(TaggedList, _Stack = [Head | Rest]) ->
    NewTaggedList = lists:map(fun({FileConf, Inbound, Outbound}) ->
                                      {FileConf,
                                       substract_lists(Inbound,element(3,Head)),
                                       Outbound}
                              end, TaggedList),
    NoInbound = lists:filter(fun({_FileConf, Inbound, _Outbound}) ->
                                     Inbound =:= []
                             end, NewTaggedList),
    [Head | sort_fileconf2(substract_lists(NewTaggedList, NoInbound),
                    NoInbound ++ Rest)].

%% @spec substract_lists(L1, L2) -> L3
%% @doc  Removes the elements of L2 from L1.
%%       Unlike the builtin '--' operator, it removes every occurence
-spec substract_lists(list(),list()) -> list().
substract_lists(L1, L2) ->
    lists:filter(fun(X) -> not lists:member(X, L2) end, L1).

%% @spec tag_fileconfs(FileConfList) -> [{FileConf, EdgeList, EdgeList}]
%% @doc Tags fileconfs with lists of in- and outbound edges.
-spec tag_fileconfs(list(list())) -> list({list(),
                                           list(atom()),
                                           list(atom())}).
tag_fileconfs(FileConfList) ->
    [{X, pre(X), succ(X)} || X <- FileConfList].

%% @doc Returns the inbound edges for a node in the fileconf graph
%% @spec pre(FileConf) -> EdgeList
-spec pre(list()) -> list(atom()).
pre(FileConf) ->
    lists:flatten(
      [pre2(X) || X <- [proplists:get_all_values(transaction_id, FileConf),
			proplists:get_all_values(session_id, FileConf),
			proplists:get_all_values(timestamp, FileConf),
			proplists:get_all_values(abstract_value, FileConf),
			proplists:get_all_values(mappings, FileConf)]]).

pre2({Id, From, To}) when is_atom(Id) ->
    pre2(From) ++ pre2(To);

pre2({Id, From})  ->
    [Id | pre2(From)];

pre2(List) when is_list(List) ->
    [pre2(X) || X <- List];

pre2(_Integer) when is_integer(_Integer) ->
    [];

pre2(filename) ->
    [];

pre2(none) ->
    [].

%% @spec succ(FileConf) -> EdgeList
%% @doc Returns the outbound edges for a node in the fileconf graph
-spec succ(list()) -> list(atom()).
succ(FileConf) ->
    case proplists:is_defined(mappings, FileConf) of
	true ->
	    [succ2(X) || X <- proplists:get_value(mappings, FileConf)];
	false ->
	    []
    end.

succ2({Id, _From, _To}) when is_atom(Id)->
    Id.

%%%
%% @spec get_name(Conf::list()) -> string()
%% @doc Returns the name of the configuration
-spec get_name(Conf::list()) -> string().
get_name(Conf) ->
    proplists:get_value(name, Conf, ?DEFAULT_NAME).

%% @spec get_files(Conf::list()) -> list({string(), tuple()})
%% @doc Returns parse info for all files in a configuration
-spec get_files(Conf::list()) -> [{string(), tuple()} | [{string(), tuple()}]].
get_files(Conf) ->
    BDir = proplists:get_value(base_dir, Conf, ?DEFAULT_BASE_DIR),
    DefDel = proplists:get_value(delimiter, Conf, ?DEFAULT_DELIMITER),
    DefTs = proplists:get_value(ts_format, Conf, ?DEFAULT_TS_FORMAT),
    FileConfs = proplists:get_value(files, Conf, []),

    AddBDir = fun(Wildcard) -> case BDir of
				   ""   -> Wildcard;
				   BDir -> filename:join([BDir, Wildcard])
			       end
	      end,

    % expand wildcards and set defaults
    [{File,
      add_def_prop(
	[{ts_format, DefTs},
	 {offset, ?DEFAULT_OFFSET},
	 {parse_opts, add_def_prop({delimiter, DefDel},
				   proplists:get_value(parse_opts,
						       FileConf,
						       ?DEFAULT_PARSE_OPTS))}],
	FileConf)}
      || FileConf <- FileConfs,
         File <- filelib:wildcard(
		   AddBDir(proplists:get_value(wildcard, FileConf)))].

%% @doc adds a propery to a proplist if it is not defined yet
-spec add_def_prop(list() | {atom(), term()}, list()) -> list().
add_def_prop(List, Proplist) when is_list(List) ->
    lists:foldl(fun(X, Acc) -> add_def_prop(X, Acc) end, Proplist, List);
add_def_prop({Key, Value}, Proplist) ->
    case proplists:get_value(Key, Proplist) of
	undefined ->
	    [{Key, Value} | Proplist];
	_ ->
	    Proplist
    end.

%% @spec get_trans_abstrfun(Conf::#audit_conf{}) ->
%%           function()
%% @doc Returns the transaction abstraction function
-spec get_trans_abstrfun(Conf::list()) -> function().
get_trans_abstrfun(Conf) ->
    proplists:get_value(trans_abstr, Conf).

%% @spec get_sess_abstrfun(Conf::#audit_conf{})-> function()
%% @doc Sets the session abstraction function
-spec get_sess_abstrfun(Conf::list())-> function().
get_sess_abstrfun(Conf) ->
    proplists:get_value(sess_abstr, Conf, ?DEFAULT_SESS_ABSTR).

%% @spec get_spec(Conf::list) -> list()
%% @doc Returns the state machine specification
-spec get_spec(Conf::list()) -> {atom(), list()} | undefined.
get_spec(Conf) ->
    proplists:get_value(specification, Conf).
