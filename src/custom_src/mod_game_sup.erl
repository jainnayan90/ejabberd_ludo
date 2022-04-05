%%%----------------------------------------------------------------------
%%% File    : mod_proxy65.erl
%%% Author  : Evgeniy Khramtsov <xram@jabber.ru>
%%% Purpose : Main supervisor.
%%% Created : 12 Oct 2006 by Evgeniy Khramtsov <xram@jabber.ru>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_game_sup).

-author('nayan.jain@havrock.com').

-behaviour(gen_mod).

-behaviour(supervisor).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3]).

%% supervisor callbacks.
-export([init/1]).

-export([start_link/2, mod_opt_type/1, mod_options/1, depends/2, process/2, user_send_packet/1, process_info/2]).

-define(PROCNAME, ejabberd_mod_game_sup).

-type c2s_state() :: ejabberd_c2s:state().

-include("translate.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("mygame.hrl").
-include("xmpp.hrl").

-callback init() -> any().
-callback register_stream(binary(), pid()) -> ok | {error, any()}.
-callback unregister_stream(binary()) -> ok | {error, any()}.
-callback activate_stream(binary(), binary(), pos_integer() | infinity, node()) ->
    ok | {error, limit | conflict | notfound | term()}.

start(Host, Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 100),
     ejabberd_hooks:add(c2s_handle_info, Host, ?MODULE,
                        process_info, 200),
    
    ?WARNING_MSG("in start  supppppppppp ~n~p~n~n~n", [{Host, Opts}]),
    ChildSpec = {?MODULE, {?MODULE, start_link, [Host, Opts]},
		 transient, infinity, supervisor, [?MODULE]},
    supervisor:start_child(ejabberd_gen_mod_sup, ChildSpec).

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
                          user_send_packet, 100),
     ejabberd_hooks:delete(c2s_handle_info, Host, ?MODULE,
                           process_info, 200),

    supervisor:terminate_child(ejabberd_gen_mod_sup, ?MODULE),
    supervisor:delete_child(ejabberd_gen_mod_sup, ?MODULE).

reload(Host, NewOpts, OldOpts) ->
    ok.

start_link(Host, Opts) ->
    ?WARNING_MSG("in startlink sup ********************** ~n~n~n~n~n~n~n~p~n~n~n", [application:start(eredis_pooler)]),
    %%application:start(eredis_pooler),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Opts]).


init([Host, Opts]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    
    %% AChild = #{id => queue_manager,
    %% 	       start => {queue_manager, start_link, [[]]},
    %% 	       restart => permanent,
    %%            shutdown => 5000,
    %% 	       type => worker,
    %% 	       modules => [queue_manager]},
    
    AChild = get_queue_specs(),
    BChild = #{id => game_manager,
    	       start => {game_manager, start_link, []},
    	       restart => permanent,
               shutdown => 5000,
    	       type => supervisor,
    	       modules => [game_manager]},
    
    
    {ok, {SupFlags, AChild ++ [BChild]}}.
 

depends(_Host, _Opts) ->
    [].


get_data_from_json(Q) ->
    [{_, Q1}] = Q,
    {RequestData} = jiffy:decode(Q1),
    RequestData.

get_data_from_requestdata(RequestData, Tag) ->
    case lists:keyfind(Tag, 1, RequestData) of
	{_, M1} ->
	    M1;
	_ ->
	    <<>>
    end.

process(Path, Request) ->
    process_url(Path, Request).


%% process_url([<<"signin">>], 
%% 	    #request{method = 'POST', q = Q, ip = {Ip, _Port},
%% 		     lang = Lang, host = Host}) ->
%%     RequestData = get_data_from_json(Q),
%%     Username = get_data_from_requestdata(RequestData, <<"username">>),
%%     Password = get_data_from_requestdata(RequestData, <<"password">>),
%%     Name = get_data_from_requestdata(RequestData, <<"name">>),
    
%%     ValidPassword = true,%%crypto:hash(md5, Password) == Username,
%%     if Username == <<>> orelse Password == <<>> orelse ValidPassword == false->
%% 	    ?WARNING_MSG("~n~n INVALID Mobile OR PASSWORD : ~p~n~n~n", [{Username, Password}]),
%% 	    Res = jiffy:encode({[
%% 				 {<<"error">>, <<"Invalid Mobile or Password.">>}
%% 				]}),
%% 	    {400, [], Res};
%%        true ->
%% 	    %%Res1 = ejabberd_auth_sql:try_register(Username, Host, Password),
%% 	    Res1 = ejabberd_auth_sql:try_register(Username, ?LSERVER, Password),
%% 	    ?WARNING_MSG("~n~n~n~n~n~n printing register query ~p~n~n~n~n~n~n~n", [Res1]),
%% 	    case Res1 of
%% 		ok ->
%% 		    game_queries:update_guest_name(?LSERVER, Username, Name),
%% 		    AuthToken = get_jwt_token(Username, Host),
%% 		    Res = jiffy:encode({[
%% 					 {<<"success">>, <<"true">>},
%% 					 {<<"authToken">>, AuthToken},
%% 					 {<<"userId">>, Username}
%% 					]}),
%% 		    {200, [], Res};
%% 		{error, exists} ->
%% 		    game_queries:update_guest_name(?LSERVER, Username, Name),
%% 		    AuthToken = get_jwt_token(Username, Host),
%%                     Res = jiffy:encode({[
%% 					 {<<"success">>, <<"true">>},
%% 					 {<<"authToken">>, AuthToken},
%% 					 {<<"userId">>, Username}
%% 					]}),
%% 		    {200, [], Res};
%% 		_ ->
%% 		    Res = jiffy:encode({[
%% 					 {<<"error">>, <<"Please Try Later.">>}
%% 					]}),
%% 		    {400, [], Res}
%% 	    end
%%     end;



%% process_url([<<"facebook_signin">>], 
%% 	    #request{method = 'POST', q = Q, ip = {Ip, _Port},
%% 		     lang = Lang, host = Host}) ->
%%     RequestData = get_data_from_json(Q),
%%     FbToken = get_data_from_requestdata(RequestData, <<"token">>),
%%     Username = get_data_from_requestdata(RequestData, <<"username">>),
%%     Name = get_data_from_requestdata(RequestData, <<"name">>),
    
%%     if Username == <<>> orelse FbToken == <<>>  ->
%% 	    ?WARNING_MSG("~n~n INVALID Mobile OR PASSWORD : ~p~n~n~n", [{Username, FbToken}]),
%% 	    Res = jiffy:encode({[
%% 				 {<<"error">>, <<"Invalid Mobile or Password.">>}
%% 				]}),
%% 	    {400, [], Res};
%%        true ->
%% %%	    AccesTokenURL = ?FB_GRAPH_URL ++ "oauth/access_token?client_id=" ++ ?APP_ID ++ "&client_secret=" ++ ?APP_SECRET_KEY ++ "&grant_type=client_credentials",
%% %%	    AccessData = httpc:request(AccesTokenURL),
%% %%	    ?WARNING_MSG("~n~n~n~n~n Access Token Data ~p~n~n~n~n~n~n", [AccessData]),


%% %%	    DebugURL = ?FB_GRAPH_URL ++ "debug_token?input_token=" ++ binary_to_list(FbToken) ++ "&access_token=" ++ ?APP_ID ++ "|" ++ ?APP_SECRET_KEY,
%% %%	    ?WARNING_MSG("~n~n~n~n~n printing fb auth validation url ~p~n~n~n~n~n~n", [DebugURL]),


	    
%% %%	    FBData = httpc:request(DebugURL),
%% %%	    ?WARNING_MSG("~n~n~n~n~n logging fb data ~p~n~n~n~n~n~n", [FBData]),
%% %%	    nayan = 1,

%% 	    case ejabberd_auth_facebook:validate_user(Username, FbToken) of
%% 		false -> 
%% 		    Res = jiffy:encode({[
%% 					 {<<"error">>, <<"Invalid Mobile or Password.">>}
%% 					]}),
%% 		    {400, [], Res};
%% 		true ->
	    
%% 		    Password = <<"">>,
%% 		    Res1 = ejabberd_auth_sql:try_register(Username, Host, Password),
%% 		    ?WARNING_MSG("~n~n~n~n~n~n printing register query ~p~n~n~n~n~n~n~n", [Res1]),
%% 		    case Res1 of
%% 			ok ->
%% 			    game_queries:update_guest_name(?LSERVER, Username, Name),
%% 			    Res = jiffy:encode({[
%% 						 {<<"success">>, <<"true">>}
%% 						]}),
%% 			    {200, [], Res};
%% 			{error, exists} ->
%% 			    Res = jiffy:encode({[
%% 						 {<<"success">>, <<"true">>}
%% 						]}),
%% 			    {200, [], Res};
%% 			_ ->
%% 			    Res = jiffy:encode({[
%% 						 {<<"error">>, <<"Please Try Later.">>}
%% 						]}),
%% 			    {400, [], Res}
%% 		    end
%% 	    end
%%     end;


process_url(_Path, _Request) ->
    {400, [], "Failure"}.



-spec user_send_packet({stanza(), c2s_state()}) -> {stanza(), c2s_state()}.
user_send_packet({#iq{type = Type,
                      to = #jid{luser = U1, lserver = S1}} = IQ, State})
  when  S1 == ?GAME_ROUTE ->
    spawn(namespace_manager, handle_stanza, [IQ]),
    {drop, State};

user_send_packet({#iq{id=ID, type = Type,
                      to = #jid{luser = U1, lserver = S1}} = IQ, State}=Acc) ->
    ?WARNING_MSG("in user send packet  ~p~n~n~n", [{ID, binary:match(ID, <<"_session_auth_">>)}]),
    case binary:match(ID, <<"_session_auth_">>) of
	{0, _} ->
	    spawn(namespace_manager, handle_stanza, [IQ]);
	_ ->
	    ok
    end,
    Acc;

user_send_packet(Acc) ->
    ?CRITICAL_MSG("in mod game sup printing other type of packet ~n~p~n~n", [Acc]),
    Acc.


mod_opt_type(host) ->
    fun ejabberd_config:v_host/1;


mod_opt_type(custom_headers) ->
    fun(Headers) ->
            lists:map(fun({K, V}) ->
                              {iolist_to_binary(K), iolist_to_binary(V)}
                      end, Headers)
    end;

mod_opt_type(admin_ip_access) ->
     fun acl:access_rules_validator/1.

mod_options(_) -> [{admin_ip_access, none}].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_jwt_token(UserID, Host) ->
    Claims = [{user_id, UserID}, {sid, Host}],
    {ok, Token} = jwt:encode(<<"HS256">>, Claims, ?JWT_SECRET_KEY),
    %%redis:set_token(UserID,Token),                                                                                                                                                                        
    Token.

get_queue_specs() ->
    TwoPlayers = lists:map(fun({Level, Chips}) -> 
				   Procname = list_to_atom("queue_manager2_" ++ integer_to_list(Level)),
				   #{id => Procname,
				     start => {queue_manager, start_link, [[{level, Level}, {chips, Chips}, {procname, Procname}, {min_players, 2}, {max_players, 2}]]},
				     restart => permanent,
				     shutdown => 5000,
				     type => worker,
				     modules => [queue_manager]}
				       
			   end, ?LEVELS_MAP),
    ThreePlayers = lists:map(fun({Level, Chips}) -> 
				     Procname = list_to_atom("queue_manager3_" ++ integer_to_list(Level)),
				     #{id => Procname,
				       start => {queue_manager, start_link, [[{level, Level}, {chips, Chips}, {procname, Procname}, {min_players, 3}, {max_players, 4}]]},
				       restart => permanent,
				       shutdown => 5000,
				       type => worker,
				       modules => [queue_manager]}
					 
			     end, ?LEVELS_MAP),
    TwoPlayers ++ ThreePlayers.
    

process_info(State, {replaced, User, Server, Resource}) ->
    ?WARNING_MSG("~n~n~n~n~n replace connection info info: ~p~n~n~n~n", [{}]),
    FromJID = game_utils:make_jid(?GAME_ROUTE),
    PlayerId = <<User/binary, "@", Server/binary, "/", Resource/binary>>,
    ToJID = game_utils:make_jid(PlayerId),
    Id = game_utils:get_iq_id(ToJID, <<"reconnection">>),
    IQ = #iq{id=Id, type='set', from=FromJID, to=ToJID, sub_els=[]},
    %%Pkt = xmpp:make_error(IQ, #xmlel{name = <<"error">>, attrs = [], children = [{xmlcdata, <<"connection_replaced">>}]}),
    ?WARNING_MSG("~n~n~n~n~nlogging pkt in replaced ~p~n~n~n~n~n", [IQ]),
    xmpp_stream_in:send_error(State, IQ, #xmlel{name = <<"error_connection_replaced">>, attrs = [], children = [{xmlcdata, <<"connection_replaced">>}]}),

    {stop, State};

process_info(State, Info) ->
    State.
