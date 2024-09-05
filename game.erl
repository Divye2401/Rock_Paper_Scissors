-module(game).
-export([start/1, active_games/4, check_gamedata/3, count_players_with_positive_credits/1]).

start(Args) ->
    PlayerFile = lists:nth(1, Args),
    {ok, PlayerInfo} = file:consult(PlayerFile), % Read player info from file
    io:format("Rock Papers Scissors World Championship~n"),

    MasterPid = self(),
    PlayerInfoDuplicate = PlayerInfo,
    create_players(PlayerInfo, MasterPid),

    % Initialize empty gamedata dictionary
    Gamedata = dict:new(),
    active_games(PlayerInfo, Gamedata, 1, PlayerInfoDuplicate).

active_games(PlayerInfo, Gamedata, GameId, PlayerInfoDuplicate) ->
    case count_players_with_positive_credits(PlayerInfo) of
        1 ->
            io:format("***Tournament Report***.~n"),
            io:format("~n"),
            io:format("Players:~n"),
            Games = print_players(PlayerInfo, PlayerInfoDuplicate),
            io:format("Total Games : ~w~n", [Games]),
            {Winner, _Credits} = find_winner(PlayerInfo),
            io:format("Winner: ~s~n", [Winner]),
            io:format("See you Next Year...");

        _ ->
            receive
                {new_game_request, Player1, Player2} ->
                    P1Credits = proplists:get_value(Player1, PlayerInfo),
                    P2Credits = proplists:get_value(Player2, PlayerInfo),

                    if P1Credits =< 0 orelse P2Credits =< 0 ->
                           % Send message to Player1 if credits are zero or less
                           case P1Credits of
                               Creds1 when Creds1 =< 0 ->
                                   Player1Pid = whereis(Player1),
                                   Player1Pid ! {dq, GameId};
                               _ ->
                                   ok
                           end,
                           % Send message to Player2 if credits are zero or less
                           case P2Credits of
                               Creds2 when Creds2 =< 0 ->
                                   Player2Pid = whereis(Player2),
                                   Player2Pid ! {dq, GameId};
                               _ ->
                                   ok
                           end,
                           active_games(PlayerInfo, Gamedata, GameId, PlayerInfoDuplicate);

                    true ->
                        Player1Pid = whereis(Player1),
                        Player2Pid = whereis(Player2),

                        % Start the game if both players have positive credits
                        Player1Pid ! {game_started, GameId, Player2, PlayerInfo},
                        Player2Pid ! {game_started, GameId, Player1, PlayerInfo},
                        io:format("+ [~w] new game for ~s -> ~s~n", [GameId, Player1, Player2]),
                        active_games(PlayerInfo, Gamedata, GameId + 1, PlayerInfoDuplicate)
                    end;

                {move, ID, PlayerName, Move} ->
                    MoveName = move_to_string(Move),

                    % Update gamedata with the move
                    UpdatedGamedata = update_gamedata(Gamedata, ID, PlayerName, MoveName),

                    % Check gamedata if the number of moves is even
                    UpdatedPlayerInfo = check_gamedata(PlayerInfo, UpdatedGamedata, ID), % return updated playerinfo

                    % Recursive call back to function with updated credit value
                    active_games(UpdatedPlayerInfo, UpdatedGamedata, GameId, PlayerInfoDuplicate);

                UnknownMsg ->
                    io:format("Unknown message received: ~p~n", [UnknownMsg]),
                    active_games(PlayerInfo, Gamedata, GameId, PlayerInfoDuplicate) % Keep receiving messages
            end
    end.

update_gamedata(Gamedata, GameId, PlayerName, Move) ->
    % Check if there are already moves for this game ID
    case dict:find(GameId, Gamedata) of
        {ok, Moves} ->
            % Add the new move to the end of the list of moves
            UpdatedMoves = Moves ++ [{PlayerName, Move}],
            dict:store(GameId, UpdatedMoves, Gamedata);
        error ->
            % Create a new list with the first move
            dict:store(GameId, [{PlayerName, Move}], Gamedata)
    end.

check_gamedata(PlayerInfo, Gamedata, GameId) ->
    case dict:find(GameId, Gamedata) of
        {ok, Moves} when length(Moves) rem 2 == 0 -> % If list has even moves, can check
            {Player1, Move1} = lists:nth(length(Moves) - 1, Moves),
            {Player2, Move2} = lists:nth(length(Moves), Moves),
            Winner = determine_winner({Player1, Move1}, {Player2, Move2}),
            case Winner of
                "draw" ->
                    Player1Pid = whereis(Player1), % Send again in case of draw
                    Player2Pid = whereis(Player2),
                    Player1Pid ! {game_started, GameId, Player2, dict:fetch(GameId, Gamedata)},
                    Player2Pid ! {game_started, GameId, Player1, dict:fetch(GameId, Gamedata)},
                    io:format("Draw detected. Restarting game with GameId ~w~n", [GameId]),
                    PlayerInfo; % Return original PlayerInfo

                {WinnerName, WinnerMove} -> % Otherwise get winner and loser
                    Loser = case WinnerName of
                                Player1 -> Player2;
                                Player2 -> Player1
                            end,
                    print_full_history(GameId, Gamedata),
                    io:format(" = ~s loses", [Loser]),
                    update_player_credit(PlayerInfo, {WinnerName, WinnerMove}, Loser) % Returns updated Player Info
            end;

        _ ->
            PlayerInfo % Return original PlayerInfo if no valid moves found
    end.

print_full_history(GameId, Gamedata) ->
    case dict:find(GameId, Gamedata) of
        {ok, Moves} ->
            MovesList = lists:map(
                fun({Player, Move}) ->
                    atom_to_list(Player) ++ ":" ++ Move % Convert Move to string
                end,
                Moves
            ),
            MovesString = string:join(MovesList, " -> "),
            io:format("$(~w) ~s", [GameId, MovesString]); 
        error ->
            io:format("No moves found for Game ID: ~p~n", [GameId])
    end.

update_player_credit(PlayerInfo, {Winner, _}, Loser) ->
    UpdatedPlayerInfo = lists:map(
        fun({Name, Credits}) ->
            case Name of
                Winner ->
                    {Name, Credits}; % Winner keeps their credits unchanged
                Loser ->
                    NewCredits = Credits - 1,
                    io:format("[~w credits left]~n", [NewCredits]),
                    {Name, NewCredits}; % Decrease credits for the loser
                _ ->
                    {Name, Credits} % Other players keep their credits unchanged
            end
        end,
        PlayerInfo
    ),
    UpdatedPlayerInfo.

count_players_with_positive_credits(PlayerInfo) ->
    lists:foldl(
        fun({_Name, Credits}, Acc) when Credits > 0 ->
            Acc + 1;
        (_, Acc) ->
            Acc
    end,
    0,
    PlayerInfo
).

find_winner(PlayerInfo) ->
    lists:nth(1,
              lists:filter(
                  fun({_Name, Credits}) ->
                          Credits > 0
                  end,
                  PlayerInfo)
             ).

determine_winner({Player1, "rock"}, {Player2, "scissors"}) -> {Player1, "rock"};
determine_winner({Player1, "scissors"}, {Player2, "paper"}) -> {Player1, "scissors"};
determine_winner({Player1, "paper"}, {Player2, "rock"}) -> {Player1, "paper"};
determine_winner({Player1, Move1}, {Player2, Move2}) when Move1 =:= Move2 -> "draw";
determine_winner({Player1, _}, {Player2, Move2}) -> {Player2, Move2}.

create_players(PlayerInfo, MasterPid) ->
    lists:foreach(fun({Name, Credits}) -> create_player(Name, Credits, PlayerInfo, MasterPid) end, PlayerInfo).

create_player(Name, Credits, PlayerInfo, MasterPid) ->
    Pid = spawn(player, create, [Name, Credits, PlayerInfo, MasterPid]),
    register(Name, Pid),
    timer:sleep(200).

print_players(PlayerInfo, PlayerInitialInfo) ->
    lists:foldl(
        fun({Name, _}, Acc) ->
            CreditsRemaining = proplists:get_value(Name, PlayerInfo),
            CreditsUsed = proplists:get_value(Name, PlayerInitialInfo) - CreditsRemaining,
            io:format("~s: credits used: ~w, credits remaining: ~w~n", [Name, CreditsUsed, CreditsRemaining]),
            Acc + CreditsUsed
        end,
        0,
        PlayerInfo
    ).

move_to_string(1) -> "rock";
move_to_string(2) -> "paper";
move_to_string(3) -> "scissors".
