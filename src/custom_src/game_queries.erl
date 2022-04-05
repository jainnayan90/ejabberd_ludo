-module(game_queries).

-export([
	 get_player_info/2, 
	 register_device_details/5,
	 update_user_chips/5,
	 update_guest_name/3,
	 create_new_player/7
	]).

-include("logger.hrl").
-include("xmpp.hrl").


create_new_player(LServer, UserId, Level, Chips, ImageUrl, Name, Domain) ->
    Query = <<"INSERT INTO players_info (user_id, name, domain, level, chips, image_url) VALUES ('",
	      UserId/binary,
	      "', '",
	      Name/binary,
	      "', '",
	      Domain/binary,
	      "', '",
	      Level/binary,
	      "', '",
	      Chips/binary,
	      "', '",
	      ImageUrl/binary,
	      "');"
	    >>,
    Res = ejabberd_sql:sql_query(LServer, Query).
    


get_player_info(LServer, PlayerId) ->
    Res = ejabberd_sql:sql_query(LServer, <<"SELECT user_id, level, chips, image_url, name FROM players_info WHERE  user_id = '",
                                            PlayerId/binary,
                                            "' ;">>),
    ?WARNING_MSG("printing result in select user query ~n~p~n~n~n~n~n", [{Res, LServer}]),
    case Res of
        {selected, _Cols, [PlayerInfo]} ->
            {ok,  PlayerInfo};
        _ ->
            error
    end.

register_device_details(LServer, PlayerId, DeviceType, PushToken, DeviceId) ->    
    Res = ejabberd_sql:sql_query(LServer, <<"INSERT INTO players_device_tokens (`user_id`, `device_type`, `device_id`, `token`) VALUES ('", 
					    PlayerId/binary,
					    "', '",
					    DeviceType/binary,
					    "', '",
					    DeviceId/binary,
					    "', '",
					    PushToken/binary,
					    "') ON DUPLICATE KEY UPDATE token='",
					    PushToken/binary,
					    "';">>),
    case Res of
	{updated, 1} ->
	    ok;
	_ ->
	    error
    end.
    
update_user_chips(LServer, UserId, TotalMoney, RemainingMoney, Level) ->
    ChipsDiff = RemainingMoney - TotalMoney,
    ChipsWon = integer_to_binary(ChipsDiff),
    NewLevel = integer_to_binary(Level),
    ejabberd_sql:sql_query(LServer, <<"UPDATE players_info set chips = chips +",
				      ChipsWon/binary,
				      ", level = ",
				      NewLevel/binary,
				      " WHERE user_id='",
				      (UserId#jid.luser)/binary,
				      "';">>),
    game_utils:send_chips_update(UserId, TotalMoney + ChipsDiff, Level).

update_guest_name(LServer, Username, Name) ->
    ejabberd_sql:sql_query(LServer, <<"UPDATE players_info set name = '", 
				      Name/binary, 
				      "' WHERE user_id = '",
				      Username/binary,
				      "';">>).
    
