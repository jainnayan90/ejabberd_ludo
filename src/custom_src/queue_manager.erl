%%%-------------------------------------------------------------------
%%% @author Nayan Jain <nayanjain>
%%% @copyright (C) 2020, Nayan Jain
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2020 by Nayan Jain <nayanjain>
%%%-------------------------------------------------------------------
-module(queue_manager).

-behaviour(gen_server).


-include("logger.hrl").
-include("mygame.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {level, chips, players=[], min_players, max_players, timer_expire_time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Opts) ->
    {_, Procname} = lists:keyfind(procname, 1, Opts),
    gen_server:start_link({local, Procname}, ?MODULE, Opts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init(Opts) ->
    {_, Level} = lists:keyfind(level, 1, Opts),
    {_, Chips} = lists:keyfind(chips, 1, Opts),
    {_, MinPlayers} = lists:keyfind(min_players, 1, Opts),
    {_, MaxPlayers} = lists:keyfind(max_players, 1, Opts),
    {ok, #state{level=Level, chips=Chips, min_players=MinPlayers, max_players=MaxPlayers, timer_expire_time=game_utils:timer_expire_time(?QUEUE_TIMEOUT)}, ?QUEUE_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    Timeout = game_utils:calculate_timeout(State#state.timer_expire_time),
    {reply, Reply, State, Timeout}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    Timeout = game_utils:calculate_timeout(State#state.timer_expire_time),
    {noreply, State, Timeout}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(Info, State) ->
    case Info of
	timeout ->
	    if (length(State#state.players) >= State#state.min_players ) ->
		    {RemPlayers, Players} = get_players_from_queue(State#state.players, State#state.max_players),
		    game_manager:make_users_play(Players, State#state.level, State#state.chips),
		    NewState = State#state{players=RemPlayers, timer_expire_time=game_utils:timer_expire_time(?QUEUE_TIMEOUT)},
		    {noreply, NewState, ?QUEUE_TIMEOUT};
	       true ->
		    NewState = State#state{timer_expire_time=game_utils:timer_expire_time(?QUEUE_TIMEOUT)},
		    {noreply, NewState, ?QUEUE_TIMEOUT}
	    end;
	_  ->
	    NewState = my_handle_info(Info, State),
	    Timeout = game_utils:calculate_timeout(State#state.timer_expire_time),
	    {noreply, NewState, Timeout}
		
    end.
	

my_handle_info({add_player, [UserId, Level, Chips, ImageUrl, Name]}, State) ->
    ?WARNING_MSG("in queue manager handle info ~n~p~n~n~n~n", [{}]),
    Players = State#state.players ++ [#player{id=jid:to_string(UserId), jid=UserId, level=Level, chips=Chips, image_url=ImageUrl, name=Name, remaining_chips=Chips}],    
    State#state{players=Players};

my_handle_info(_info, State) ->
    State.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_player([P| R], MaxPlayers, Players) ->
    NewPlayers = Players ++ [P],
    if (length(Players) < MaxPlayers) ->
	    get_player(R, MaxPlayers, NewPlayers);
       true  ->
	    {R, NewPlayers}
    end;
get_player(R, _, Players) ->
    {R, Players}.


get_players_from_queue(Players, MaxPlayers) ->
    {R, Players} = get_player(Players, MaxPlayers, []).

    
