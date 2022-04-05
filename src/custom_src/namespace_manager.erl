-module(namespace_manager).

-export([handle_stanza/1]).

-include("logger.hrl").
-include("mygame.hrl").
-include("xmpp.hrl").
-include("namespaces.hrl").



handle_stanza(#iq{to = To, lang = Lang, sub_els = [SubEl]} = Stanza) ->
    Sender = xmpp:get_from(Stanza),
    Receiver = xmpp:get_to(Stanza),
    XMLNS = xmpp:get_ns(SubEl),
    case XMLNS of 
	?NEW_START_SESSION ->
	    send_player_info(Sender);
	?NS_ACTIVEGAME ->
	    send_player_info(Sender, Receiver),
	    GameId = game_manager:get_active_game({Stanza, SubEl}),
	    GameIdEl = #xmlel{name = <<"game_id">>, attrs = [], children = [{xmlcdata, GameId}]},
	    QueryEl = #xmlel{name = <<"query">>, attrs = [], children = [GameIdEl]},
            QueryEl1 = game_utils:set_namespace(QueryEl, ?NS_ACTIVEGAME),
	    game_utils:send_result_stanza(Stanza, QueryEl1);
	?NS_PLAY_ONLINE ->
	    ActiveGameId = game_manager:get_active_game({Stanza, SubEl}),
	    if ActiveGameId =/= <<"null">> ->
		    game_utils:send_error_stanza(Stanza, #xmlel{name = <<"error">>, attrs = [{<<"error_type">>, <<"already_playing">>}], children = [{xmlcdata, <<"You are already playing on a table!">>}]});
	       true ->
		    GameMode = game_utils:get_element(SubEl, <<"mode">>),
		    ?WARNING_MSG("~n~n~n~n~n~n##################### printing game mode in play online ~p~n~n~n~n~n###############################", [GameMode]),
		    case  game_queries:get_player_info(?LSERVER, Sender#jid.luser) of
			{ok, [UserId, Level, Chips, ImageUrl, Name]} ->
			    case game_utils:get_table_level(binary_to_integer(Chips)) of
				false ->
				    game_utils:send_error_stanza(Stanza, #xmlel{name = <<"error">>, attrs = [{<<"error_type">>, <<"insufficient_chips">>}], children = [{xmlcdata, <<"Insufficient Chips!">>}]});
				TableLevel ->
				    game_utils:send_result_stanza(Stanza),
				    if GameMode == ?TWO_PLAYER_MODE ->
					    QueueProc = list_to_atom("queue_manager2_" ++ integer_to_list(TableLevel));
				       true ->
				    QueueProc = list_to_atom("queue_manager3_" ++ integer_to_list(TableLevel))
				    end,
				    (QueueProc ! {add_player, [Sender, binary_to_integer(Level), binary_to_integer(Chips), ImageUrl, Name]})
			    end;
			
			_ ->
			    game_utils:send_error_stanza(Stanza, #xmlel{name = <<"error">>, attrs = [], children = [{xmlcdata, <<"Invalid Player!">>}]})
		    end
	    end;
	?NS_PLAYER_DICE_ROLL ->
	    game_manager:send_to_game({dice_roll, Stanza, SubEl});
	?NS_PLAYER_PAWN_MOVE ->
	    game_manager:send_to_game({pawn_move, Stanza, SubEl});
	?NS_QUIT_GAME ->
	    game_manager:send_to_game({player_quit, Stanza, SubEl});
	?NS_JOIN_ACTIVE_GAME ->
	    game_manager:send_to_game({send_game_info, Stanza, SubEl});
	?NS_DEVICE_TOKEN ->
	    ?WARNING_MSG("\n\n\n\n\n\n\n in device token namespace_manager ~p~n~n~n~n~n", [{Stanza}]),
	    game_utils:register_device_details(Stanza, SubEl);
	_ ->
	    game_utils:send_error_stanza(Stanza, #xmlel{name = <<"error">>, attrs = [], children = [{xmlcdata, <<"Invalid Stanza!">>}]})
    end;

handle_stanza(Stanza) ->
    ok.

send_player_info(Sender) ->
    send_player_info(Sender, game_utils:make_jid(?GAME_ROUTE)).

send_player_info(Sender, Receiver) ->
    ?WARNING_MSG("~n~n~n~n~n~n~n Printing send_player_info ~p~n~n", [{Sender, Receiver}]),
    case game_queries:get_player_info(Sender#jid.lserver, Sender#jid.luser) of
	{ok, [UserId, Level, Chips, ImageUrl, Name]} ->
	    {UserId, Level, Chips, ImageUrl, Name} = {UserId, Level, Chips, ImageUrl, Name};
	_ ->
	    {UserId, Level, Chips, ImageUrl, Name} = {Sender#jid.luser, <<"1">>, <<"2000">>, <<"">>, <<"Guest">>},
	    game_queries:create_new_player(Sender#jid.lserver, UserId, Level, Chips, ImageUrl, Name, Sender#jid.lserver)
    end,
    UserIdEl = #xmlel{name = <<"user_id">>, attrs = [], children = [{xmlcdata, UserId}]},
    LevelEl = #xmlel{name = <<"level">>, attrs = [], children = [{xmlcdata, Level}]},
    ChipsEl = #xmlel{name = <<"chips">>, attrs = [], children = [{xmlcdata, Chips}]},
    ImageEl = #xmlel{name = <<"image_url">>, attrs = [], children = [{xmlcdata, ImageUrl}]},
    NameEl = #xmlel{name = <<"name">>, attrs = [], children = [{xmlcdata, Name}]},
    QueryEl = #xmlel{name = <<"query">>, attrs = [], children = [UserIdEl, LevelEl, ChipsEl, ImageEl, NameEl]},
    QueryEl1 = game_utils:set_namespace(QueryEl, ?NS_PLAYERINFO),
    Id = game_utils:get_iq_id(Sender, ?NS_PLAYERINFO),
    Stanza = game_utils:make_iq_stanza(Id, Receiver, Sender, 'set', [QueryEl1]),
    ejabberd_router:route(Stanza).

    

