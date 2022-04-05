%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_jwt.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Authentication using JWT tokens
%%% Created : 16 Mar 2019 by Mickael Remond <mremond@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
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

-module(ejabberd_auth_facebook).

-author('mremond@process-one.net').

-behaviour(ejabberd_auth).

-export([start/1, stop/1, check_password/4,
	 store_type/1, plain_password_required/1,
         user_exists/2, use_cache/1,validate_user/2
        ]).

-include("xmpp.hrl").
-include("logger.hrl").
-include("mygame.hrl").



%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
	ok.

stop(_Host) -> ok.

plain_password_required(_Host) -> true.

store_type(_Host) -> external.

-spec check_password(binary(), binary(), binary(), binary()) -> {ets_cache:tag(), boolean() | {stop, boolean()}}.
check_password(User, AuthzId, Server, Token) ->
    %% MREMOND: Should we move the AuthzId check at a higher level in
    %%          the call stack?
    ?WARNING_MSG("in ejabberd facebook check password ~n~n~n~n~p~n~n~n~n", [{binary:match(User, <<"fb_">>)}]),
    case binary:match(User, <<"fb_">>) of
	{0, _} ->
	    validate_user(User, Token);
	_ ->
	    false
    end.



user_exists(_User, _Host) -> {nocache, false}.

use_cache(_) ->
    false.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

validate_user(User, Token) ->
    DebugURL = ?FB_GRAPH_URL ++ "debug_token?input_token=" ++ binary_to_list(Token) ++ "&access_token=" ++ http_uri:encode(?APP_ID ++ "|" ++ ?APP_SECRET_KEY),
    ?WARNING_MSG("~n~n~n~n~n printing fb auth validation url ~p~n~n~n~n~n~n", [DebugURL]),
    
    case httpc:request(DebugURL) of
	{ok, {_A, _B, C}} ->
	    ?WARNING_MSG("~n~n~n~n~n printing fb auth validation url ~p~n~n~n~n~n~n", [C]), 
	    {[{<<"data">>, {DataTuples}}]} = jiffy:decode(C),
	    case lists:keyfind(<<"user_id">>, 1, DataTuples) of
		{<<"user_id">>, UserId} ->
		    UserId1 = <<"fb_",  UserId/binary>>,
		    ?WARNING_MSG("~n~n~n~n~n printing fb auth validation url ~p~n~n~n~n~n~n", [{UserId1, User}]),
		    UserId1 == User;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.
