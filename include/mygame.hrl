

-record(player, {id, level, image_url, chips, remaining_chips, name, jid, markers, index, delta}).
%% -record(board_cell, {id, safe, pawns=[]})
-define(JWT_SECRET_KEY, <<"bnfdfhhjfhh7*67686HGGH8766676776&^%^&%&^%&^&^*&FGHJBJHVGHJHGhdfjfiu7rty456875rhtr,gbryfeh78a656755%&^%%^%^&%``75675765">>).
-define(GAME_ROUTE, <<"ludo.havrock.com">>).
-define(GUEST_SERVER, <<"guest.ludo.havrock.com">>).
-define(LSERVER, <<"havrock">>).

-define(QUEUE_TIMEOUT, 2000).
-define(GAME_START_TIME, 4000).
-define(TABLE_COUNTER, <<"TABLE_COUNTER">>).
-define(STURN_TIMEOUT, 11000).
-define(CTURN_TIMEOUT, 10000).
-define(TURN_DIFF, 1000).

-define(RESULT_TIMEOUT, 3000).
-define(REDIS_POOL, redis1).
-define(ACTIVE_GAME_KEY, <<"-active-game">>).


-define(LEVELS_MAP, [
		     {1, 100},
		     {2, 200},
		     {3, 300},
		     {4, 400},
		     {5, 500}
		    ]).
-define(INDEX_KEY, [{1, <<"r">>, 0}, {2, <<"g">>, -13}, {3, <<"y">>, -26}, {4, <<"b">>, -39}]).

-define(COLLISION_FREE_CELLS, [1, 14, 27, 40]).

-define(PAWN_COLLISION_TIME, 50).
-define(PAWN_MOVE_TIME, 300).
-define(TWO_PLAYER_MODE, <<"two_player">>).
-define(THREE_PLAYER_MODE, <<"three_player">>).

-define(FB_GRAPH_URL, "https://graph.facebook.com/").
-define(APP_ID, "2642071886114112").
-define(APP_SECRET_KEY, "9f6f91bbc5c80d0078aec0f5eca59446").

%% -define(BOARD, [
%% 		#board_cell{id=1, safe=false, pawns=[]},
%% 		#board_cell{id=2, safe=false, pawns=[]},
%% 		#board_cell{id=3, safe=false, pawns=[]},
%% 		#board_cell{id=4, safe=false, pawns=[]},
%% 		#board_cell{id=5, safe=false, pawns=[]},
%% 		#board_cell{id=6, safe=false, pawns=[]},
%% 		#board_cell{id=7, safe=false, pawns=[]},
%% 		#board_cell{id=8, safe=false, pawns=[]},
%% 		#board_cell{id=9, safe=false, pawns=[]},
%% 		#board_cell{id=10, safe=false, pawns=[]},
%% 		#board_cell{id=11, safe=false, pawns=[]},
%% 		#board_cell{id=12, safe=false, pawns=[]},
%% 		#board_cell{id=13, safe=false, pawns=[]},
%% 		#board_cell{id=14, safe=false, pawns=[]},
%% 		#board_cell{id=15, safe=false, pawns=[]},
%% 		#board_cell{id=16, safe=false, pawns=[]},
%% 		#board_cell{id=17, safe=false, pawns=[]},
%% 		#board_cell{id=18, safe=false, pawns=[]},
%% 		#board_cell{id=19, safe=false, pawns=[]},
%% 		#board_cell{id=20, safe=false, pawns=[]},
%% 		#board_cell{id=21, safe=false, pawns=[]},
%% 		#board_cell{id=22, safe=false, pawns=[]},
%% 		#board_cell{id=23, safe=false, pawns=[]},
%% 		#board_cell{id=24, safe=false, pawns=[]},
%% 		#board_cell{id=25, safe=false, pawns=[]},
%% 		#board_cell{id=26, safe=false, pawns=[]},
%% 		#board_cell{id=27, safe=false, pawns=[]},
%% 		#board_cell{id=28, safe=false, pawns=[]},
%% 		#board_cell{id=29, safe=false, pawns=[]},
%% 		#board_cell{id=30, safe=false, pawns=[]},
%% 		#board_cell{id=31, safe=false, pawns=[]},
%% 		#board_cell{id=32, safe=false, pawns=[]},
%% 		#board_cell{id=33, safe=false, pawns=[]},
%% 		#board_cell{id=34, safe=false, pawns=[]},
%% 		#board_cell{id=35, safe=false, pawns=[]},
%% 		#board_cell{id=36, safe=false, pawns=[]},
%% 		#board_cell{id=37, safe=false, pawns=[]},
%% 		#board_cell{id=38, safe=false, pawns=[]},
%% 		#board_cell{id=39, safe=false, pawns=[]},
%% 		#board_cell{id=40, safe=false, pawns=[]},
%% 		#board_cell{id=41, safe=false, pawns=[]},
%% 		#board_cell{id=42, safe=false, pawns=[]},
%% 		#board_cell{id=43, safe=false, pawns=[]},
%% 		#board_cell{id=44, safe=false, pawns=[]},
%% 		#board_cell{id=45, safe=false, pawns=[]},
%% 		#board_cell{id=46, safe=false, pawns=[]},
%% 		#board_cell{id=47, safe=false, pawns=[]},
%% 		#board_cell{id=48, safe=false, pawns=[]},
%% 		#board_cell{id=49, safe=false, pawns=[]},
%% 		#board_cell{id=50, safe=false, pawns=[]},
%% 		#board_cell{id=51, safe=false, pawns=[]},
%% 		#board_cell{id=52, safe=false, pawns=[]},
%% 	       ]).
