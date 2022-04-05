-module(game_utils).

-export([send_error_stanza/2, send_result_stanza/2, set_attribute/2, set_namespace/2,
	 get_iq_id/2, make_iq_stanza/5, send_result_stanza/1, timer_expire_time/1, calculate_timeout/1,
	 current_time/0, broadcast_stanza/5, make_jid/1, broadcast_stanza_to_rest/6, get_element/2, check_for_winner/1,
	 register_device_details/2, send_chips_update/3, get_table_level/1]).

-include("logger.hrl").
-include("xmpp.hrl").
-include("mygame.hrl").
-include("namespaces.hrl").

current_time() ->
    {M, S, _Ms} = os:timestamp(),
    M * 1000000 + S*1000.

timer_expire_time(Timeout) ->
    Time = current_time(),
    Time + Timeout.


calculate_timeout(Timer) ->
    Current = current_time(),
    if Timer == false orelse Current > Timer ->
            0;
       true ->
            (Timer - Current)
    end.



get_iq_id(PlayerId, NS) ->
    ID = integer_to_binary(rand:uniform(10000000000000)),
    PID = jid:to_string(PlayerId),
    <<PID/binary, ":", NS/binary, ":", ID/binary>>.

set_namespace(El, NS) ->
    set_attribute(El, {<<"xmlns">>, NS}).

set_attribute(#xmlel{attrs=Attrs}=El, {Key, Val} = Attr) ->
    Attrs1 = lists:keystore(Key, 1, Attrs, Attr),
    El#xmlel{attrs=Attrs1}.
    


send_error_stanza(Stanza, El) ->
    ErrorStanza = xmpp:make_error(Stanza, El),
    ejabberd_router:route(ErrorStanza).

send_result_stanza(Stanza) ->
    [QueryEl] = Stanza#iq.sub_els,
    ResultStanza = xmpp:make_iq_result(Stanza, QueryEl),
    ejabberd_router:route(ResultStanza).


send_result_stanza(Stanza, El) ->
    ResultStanza = xmpp:make_iq_result(Stanza, El),
    ejabberd_router:route(ResultStanza).


jid_to_binary(#jid{user = U, server = S, resource = R,
                   luser = LU, lserver = LS, lresource = LR}) ->
    #jid{user = iolist_to_binary(U),
         server = iolist_to_binary(S),
         resource = iolist_to_binary(R),
         luser = iolist_to_binary(LU),
         lserver = iolist_to_binary(LS),
         lresource = iolist_to_binary(LR)}.

make_jid(BJID) ->
    case binary:split(BJID, <<"@">>) of
	[LUser, LServer] ->
	    jid:make(LUser, LServer);
	_ ->
	    jid:make(<<"">>, BJID)
    end.

make_iq_stanza(Id, From, To, Type, Query) ->
    IQ = #iq{id=Id, type=Type, from=From, to=To, sub_els=Query}.
	
broadcast_stanza(Players, Id, From, Type, Query) ->
    lists:map(fun (Player) -> 
		      IQ = #iq{id=Id, type=Type, from=From, to=Player#player.jid, sub_els=Query},
		      ejabberd_router:route(IQ)
	      end, Players).

broadcast_stanza_to_rest(Players, Id, From, Type, Query, Sender) ->
    lists:map(fun (Player) -> 
		      ?WARNING_MSG("~n~n~n~n printing ids in rest stanza ~p~n~n~n~n~n", [{Player#player.id, Sender}]),
		      if Player#player.id =/= Sender ->
			      IQ = #iq{id=Id, type=Type, from=From, to=Player#player.jid, sub_els=Query},
			      ejabberd_router:route(IQ);
			 true  ->
			      ok
		      end
	      end, Players).

get_element(Query, El) ->
    Elements = xmpp:get_els(Query),
    ?WARNING_MSG(" ~n~n~n~n in get elements printing el ~p~n~n~n~n", [{Query, lists:keysearch(El, 2, Elements)}]),
    case lists:keysearch(El, 2, Elements) of
	{value, {xmlel, El, _, [{xmlcdata, Val}]}} ->
	    Val;
	_ ->
	    <<"">>
    end.

check_for_winner(Markers) ->
    WinnerIndexes = [
		     [0, 1, 2],
		     [3, 4, 5],
		     [6, 7, 8],
		     [0, 3, 6],
		     [1, 4, 7],
		     [2, 5, 8],
		     [0, 4, 8],
		     [2, 4, 6]
		    ],
    WinnerCombo = lists:filter(fun([I1, I2, I3]) -> 
				       M1 = array:get(I1, Markers),
				       M2 = array:get(I2, Markers),
				       M3 = array:get(I3, Markers),
				       if M1 == M2 andalso M1 == M3 andalso M1 =/= undefined->
					       true;
					  true ->
					       false
				       end
			       end, WinnerIndexes),
    ?WARNING_MSG(" ~n~n~n~n printing winnerCombo ~p~n~n~n~n~n", [WinnerCombo]),
    if WinnerCombo =/= [] ->
	    [[I0, I1, I2] = WC | _R] = WinnerCombo, 
	    Marker = array:get(I0, Markers),
	    {true, WC, Marker};
       true ->
	    false
    end.


register_device_details(Stanza, SubEl) ->
    ?WARNING_MSG("\n\n\n\n\n\n\n\n\n in register device details ~p~n~n~n~n", [{Stanza}]),
    FromJID = xmpp:get_from(Stanza),
    PlayerId = FromJID#jid.luser,
    DeviceType = get_element(SubEl, <<"deviceType">>),
    PushToken = get_element(SubEl, <<"pushToken">>),
    DeviceId = get_element(SubEl, <<"deviceId">>),
    case game_queries:register_device_details(?LSERVER, PlayerId, DeviceType, PushToken, DeviceId) of
	ok ->
	    send_result_stanza(Stanza);
	_ ->
	    send_error_stanza(Stanza, #xmlel{name = <<"error">>, attrs = [], children = [{xmlcdata, <<"Error! Try Later.">>}]})
    end.


send_chips_update(UserId, Chips, Level) ->
    FromJID = make_jid(?GAME_ROUTE),
    ChipsEl = #xmlel{name = <<"chips">>, attrs = [], children = [{xmlcdata, integer_to_binary(Chips)}]},
    LevelEl = #xmlel{name = <<"level">>, attrs = [], children = [{xmlcdata, integer_to_binary(Level)}]},
    QueryEl = #xmlel{name = <<"query">>, attrs = [], children = [ChipsEl, LevelEl]},
    QueryEl1 = game_utils:set_namespace(QueryEl, ?NS_UPDATE_CHIPS),
    Id = game_utils:get_iq_id(FromJID, ?NS_UPDATE_CHIPS),
    
    
    IQ = #iq{id=Id, type='set', from=FromJID, to=UserId, sub_els=[QueryEl1]},
    ejabberd_router:route(IQ).



get_level(Chips, [{Level, LChips}| R]) ->
    if Level == 1 andalso Chips >= LChips ->
	    Level;
       (Chips  >= LChips * 2) ->
	    Level;
       true  ->
	    get_level(Chips, R)
    end;
get_level(Chips, []) ->
    false.

get_table_level(Chips) ->
    get_level(Chips, lists:reverse(?LEVELS_MAP)).



